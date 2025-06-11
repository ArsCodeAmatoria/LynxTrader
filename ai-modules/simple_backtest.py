#!/usr/bin/env python3
"""
LynxTrader Simple Backtesting Engine
Simplified version using only yfinance and existing modules
"""

import os
import time
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional
from dataclasses import dataclass, asdict
import asyncio
import logging
import sqlite3
import json

# Data Sources - only yfinance to avoid conflicts
import yfinance as yf

# TensorBoard Integration
from torch.utils.tensorboard import SummaryWriter
import matplotlib.pyplot as plt

# Our modules
from shorting_signals import ShortingSignalEngine, ShortSignal
from enhanced_tensorboard import EnhancedTensorBoardMonitor

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class SimpleBacktestConfig:
    """Simple backtesting configuration"""
    start_date: str
    end_date: str
    initial_capital: float
    commission: float = 0.001  # 0.1%
    slippage: float = 0.0005   # 0.05%
    symbols: List[str] = None
    timeframes: List[str] = None  # ['5min', '15min', '1hour', '1day']
    enable_shorting: bool = True
    max_leverage: float = 2.0
    benchmark: str = 'SPY'

@dataclass
class SimpleTradeExecution:
    """Simple trade execution record"""
    timestamp: datetime
    symbol: str
    strategy: str
    signal_type: str  # 'BUY', 'SELL', 'SHORT', 'COVER'
    quantity: float
    price: float
    commission: float
    slippage: float
    total_cost: float
    ai_confidence: float
    metadata: Dict[str, Any]

@dataclass
class SimpleBacktestResults:
    """Simple backtest results"""
    config: SimpleBacktestConfig
    start_date: datetime
    end_date: datetime
    initial_capital: float
    final_capital: float
    total_return: float
    annual_return: float
    max_drawdown: float
    sharpe_ratio: float
    win_rate: float
    total_trades: int
    trades: List[SimpleTradeExecution]
    equity_curve: pd.Series
    strategy_performance: Dict[str, Dict[str, float]]

class SimpleDataProvider:
    """Simple data provider using only yfinance"""
    
    async def fetch_data(self, symbol: str, timeframe: str, start_date: str, 
                        end_date: str) -> pd.DataFrame:
        """Fetch historical data from Yahoo Finance"""
        try:
            interval_map = {
                '1min': '1m', '5min': '5m', '15min': '15m', 
                '30min': '30m', '1hour': '1h', '1day': '1d'
            }
            
            interval = interval_map.get(timeframe, '5m')
            ticker = yf.Ticker(symbol)
            
            # For intraday data, we need to limit the period
            if interval in ['1m', '5m', '15m', '30m', '1h']:
                # yfinance limits intraday data to last 60 days
                end_dt = pd.to_datetime(end_date)
                start_dt = max(pd.to_datetime(start_date), end_dt - timedelta(days=59))
                df = ticker.history(start=start_dt, end=end_dt, interval=interval)
            else:
                df = ticker.history(start=start_date, end=end_date, interval=interval)
            
            if df.empty:
                logger.warning(f"No data returned for {symbol}")
                return pd.DataFrame()
            
            # Standardize column names
            df = df.rename(columns={
                'Open': 'open', 'High': 'high', 'Low': 'low', 
                'Close': 'close', 'Volume': 'volume'
            })
            
            df['symbol'] = symbol
            df['timestamp'] = df.index
            df.reset_index(drop=True, inplace=True)
            
            logger.info(f"Fetched {len(df)} bars for {symbol} from yfinance")
            return df
            
        except Exception as e:
            logger.error(f"YFinance error for {symbol}: {e}")
            return pd.DataFrame()

class SimpleBacktestEngine:
    """Simple backtesting engine with TensorBoard integration"""
    
    def __init__(self, config: SimpleBacktestConfig):
        self.config = config
        self.data_provider = SimpleDataProvider()
        self.shorting_engine = ShortingSignalEngine()
        
        # Backtest state
        self.cash = config.initial_capital
        self.positions = {}  # symbol -> quantity (negative for short)
        self.trades = []
        
        # TensorBoard setup
        self.tb_writer = SummaryWriter(
            log_dir=f"tensorboard_logs/simple_backtest_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        )
        
        # Database for results
        self.setup_database()
    
    def setup_database(self):
        """Setup SQLite database for backtest results"""
        self.db_path = 'simple_backtest_results.db'
        conn = sqlite3.connect(self.db_path)
        
        conn.execute('''
            CREATE TABLE IF NOT EXISTS backtest_runs (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id TEXT UNIQUE,
                config TEXT,
                start_time TIMESTAMP,
                end_time TIMESTAMP,
                results TEXT
            )
        ''')
        
        conn.execute('''
            CREATE TABLE IF NOT EXISTS trades (
                id INTEGER PRIMARY KEY AUTOINCREMENT,
                run_id TEXT,
                timestamp TIMESTAMP,
                symbol TEXT,
                strategy TEXT,
                signal_type TEXT,
                quantity REAL,
                price REAL,
                commission REAL,
                total_cost REAL,
                ai_confidence REAL,
                metadata TEXT
            )
        ''')
        
        conn.commit()
        conn.close()
        
        logger.info("Simple backtest database initialized")
    
    async def run_backtest(self) -> SimpleBacktestResults:
        """Execute the complete backtesting process"""
        logger.info("Starting simple backtest...")
        start_time = time.time()
        
        run_id = f"simple_backtest_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        try:
            # 1. Data Collection
            logger.info("Collecting market data...")
            market_data = await self._collect_market_data()
            
            if not market_data:
                raise ValueError("No market data collected")
            
            # 2. Strategy Testing
            logger.info("Testing shorting strategies...")
            await self._test_strategies(market_data)
            
            # 3. Performance Analysis
            logger.info("Analyzing performance...")
            results = await self._analyze_performance(run_id)
            
            # 4. TensorBoard Logging
            logger.info("Logging to TensorBoard...")
            await self._log_to_tensorboard(results)
            
            # 5. Database Storage
            logger.info("Storing results...")
            await self._store_results(run_id, results)
            
            execution_time = time.time() - start_time
            logger.info(f"Simple backtest completed in {execution_time:.2f} seconds")
            
            return results
            
        except Exception as e:
            logger.error(f"Simple backtest failed: {e}")
            raise
    
    async def _collect_market_data(self) -> Dict[str, pd.DataFrame]:
        """Collect market data from yfinance"""
        market_data = {}
        
        symbols = self.config.symbols or ['AAPL', 'MSFT', 'TSLA', 'SPY', 'QQQ']
        timeframes = self.config.timeframes or ['5min', '15min']
        
        for symbol in symbols:
            for timeframe in timeframes:
                try:
                    df = await self.data_provider.fetch_data(
                        symbol, timeframe, 
                        self.config.start_date, self.config.end_date
                    )
                    if not df.empty:
                        key = f"{symbol}_{timeframe}"
                        market_data[key] = df
                        logger.info(f"Loaded {len(df)} bars for {key}")
                except Exception as e:
                    logger.error(f"Failed to load {symbol}_{timeframe}: {e}")
        
        logger.info(f"Collected data for {len(market_data)} symbol/timeframe combinations")
        return market_data
    
    async def _test_strategies(self, market_data: Dict[str, pd.DataFrame]):
        """Test shorting strategies against historical data"""
        for data_key, df in market_data.items():
            symbol, timeframe = data_key.split('_')
            
            logger.info(f"Testing strategies on {symbol} {timeframe}")
            
            # Generate signals using our shorting engine
            signals = self.shorting_engine.generate_signals(symbol, df)
            
            # Execute trades based on signals
            for signal in signals:
                await self._execute_signal_trade(signal, df)
                
                # Log to TensorBoard in real-time
                self._log_signal_to_tensorboard(signal, len(self.trades))
    
    async def _execute_signal_trade(self, signal: ShortSignal, df: pd.DataFrame):
        """Execute a trade based on a signal"""
        try:
            # Find the price at signal timestamp
            signal_time = signal.timestamp
            closest_row = df.iloc[(df['timestamp'] - signal_time).abs().argsort()[:1]]
            
            if closest_row.empty:
                return
            
            execution_price = closest_row['close'].iloc[0]
            
            # Apply slippage
            if signal.signal_type == 'ENTRY' and 'short' in signal.strategy.lower():
                # Short entry - we're selling, so we get slightly lower price
                execution_price *= (1 - self.config.slippage)
                quantity = -abs(signal.metadata.get('position_size', 100))  # Negative for short
            else:
                # Long entry or short cover
                execution_price *= (1 + self.config.slippage)
                quantity = abs(signal.metadata.get('position_size', 100))
            
            # Calculate commission
            commission = abs(quantity * execution_price * self.config.commission)
            total_cost = quantity * execution_price + commission
            
            # Check if we have enough capital
            if total_cost > self.cash and quantity > 0:
                logger.warning(f"Insufficient capital for trade: {total_cost} > {self.cash}")
                return
            
            # Execute the trade
            trade = SimpleTradeExecution(
                timestamp=signal_time,
                symbol=signal.symbol,
                strategy=signal.strategy,
                signal_type='SHORT' if quantity < 0 else 'LONG',
                quantity=abs(quantity),
                price=execution_price,
                commission=commission,
                slippage=abs(quantity * execution_price * self.config.slippage),
                total_cost=total_cost,
                ai_confidence=signal.confidence,
                metadata=signal.metadata
            )
            
            # Update positions and cash
            if signal.symbol not in self.positions:
                self.positions[signal.symbol] = 0
            
            self.positions[signal.symbol] += quantity
            self.cash -= total_cost
            
            self.trades.append(trade)
            
            logger.debug(f"Executed {trade.signal_type} trade: {trade.symbol} @ ${trade.price:.2f}")
            
        except Exception as e:
            logger.error(f"Error executing trade for signal {signal}: {e}")
    
    async def _analyze_performance(self, run_id: str) -> SimpleBacktestResults:
        """Comprehensive performance analysis"""
        if not self.trades:
            logger.warning("No trades executed during backtest")
        
        # Calculate equity curve
        equity_curve = self._calculate_equity_curve()
        daily_returns = equity_curve.pct_change().dropna()
        
        # Performance metrics
        total_return = (equity_curve.iloc[-1] / self.config.initial_capital - 1) * 100 if not equity_curve.empty else 0
        annual_return = self._calculate_annual_return(daily_returns)
        max_drawdown = self._calculate_max_drawdown(equity_curve)
        sharpe_ratio = self._calculate_sharpe_ratio(daily_returns)
        
        # Trade statistics
        profitable_trades = len([t for t in self.trades if t.total_cost < 0])  # Simplified
        win_rate = (profitable_trades / len(self.trades) * 100) if self.trades else 0
        
        # Strategy performance breakdown
        strategy_performance = self._analyze_strategy_performance()
        
        results = SimpleBacktestResults(
            config=self.config,
            start_date=pd.to_datetime(self.config.start_date),
            end_date=pd.to_datetime(self.config.end_date),
            initial_capital=self.config.initial_capital,
            final_capital=equity_curve.iloc[-1] if not equity_curve.empty else self.config.initial_capital,
            total_return=total_return,
            annual_return=annual_return,
            max_drawdown=max_drawdown,
            sharpe_ratio=sharpe_ratio,
            win_rate=win_rate,
            total_trades=len(self.trades),
            trades=self.trades,
            equity_curve=equity_curve,
            strategy_performance=strategy_performance
        )
        
        return results
    
    def _calculate_equity_curve(self) -> pd.Series:
        """Calculate portfolio equity curve over time"""
        if not self.trades:
            return pd.Series([self.config.initial_capital])
        
        # Create DataFrame from trades
        trade_df = pd.DataFrame([asdict(trade) for trade in self.trades])
        trade_df['timestamp'] = pd.to_datetime(trade_df['timestamp'])
        trade_df_sorted = trade_df.sort_values('timestamp')
        
        # Calculate cumulative cash changes
        cash_changes = -trade_df_sorted['total_cost'].cumsum()
        equity_curve = self.config.initial_capital + cash_changes
        
        return equity_curve.reset_index(drop=True)
    
    def _calculate_annual_return(self, daily_returns: pd.Series) -> float:
        """Calculate annualized return"""
        if daily_returns.empty:
            return 0.0
        
        cumulative_return = (1 + daily_returns).prod() - 1
        days = len(daily_returns)
        if days == 0:
            return 0.0
        annual_return = (1 + cumulative_return) ** (252 / days) - 1
        
        return annual_return * 100
    
    def _calculate_max_drawdown(self, equity_curve: pd.Series) -> float:
        """Calculate maximum drawdown"""
        if equity_curve.empty:
            return 0.0
        
        rolling_max = equity_curve.expanding().max()
        drawdown = (equity_curve - rolling_max) / rolling_max
        max_drawdown = drawdown.min()
        
        return abs(max_drawdown) * 100
    
    def _calculate_sharpe_ratio(self, daily_returns: pd.Series) -> float:
        """Calculate Sharpe ratio"""
        if daily_returns.empty or daily_returns.std() == 0:
            return 0.0
        
        excess_returns = daily_returns - 0.02/252  # Assuming 2% risk-free rate
        sharpe = excess_returns.mean() / daily_returns.std() * np.sqrt(252)
        
        return sharpe
    
    def _analyze_strategy_performance(self) -> Dict[str, Dict[str, float]]:
        """Analyze performance by strategy"""
        if not self.trades:
            return {}
        
        trade_df = pd.DataFrame([asdict(trade) for trade in self.trades])
        strategy_stats = {}
        
        for strategy in trade_df['strategy'].unique():
            strategy_trades = trade_df[trade_df['strategy'] == strategy]
            
            strategy_stats[strategy] = {
                'total_trades': len(strategy_trades),
                'avg_confidence': strategy_trades['ai_confidence'].mean(),
                'total_volume': strategy_trades['total_cost'].sum(),
                'win_rate': 0.0,  # Simplified
                'avg_return': 0.0  # Simplified
            }
        
        return strategy_stats
    
    async def _log_to_tensorboard(self, results: SimpleBacktestResults):
        """Log backtest results to TensorBoard"""
        try:
            # Overall performance metrics
            self.tb_writer.add_scalar('Backtest/Total_Return', results.total_return, 0)
            self.tb_writer.add_scalar('Backtest/Annual_Return', results.annual_return, 0)
            self.tb_writer.add_scalar('Backtest/Max_Drawdown', results.max_drawdown, 0)
            self.tb_writer.add_scalar('Backtest/Sharpe_Ratio', results.sharpe_ratio, 0)
            self.tb_writer.add_scalar('Backtest/Win_Rate', results.win_rate, 0)
            self.tb_writer.add_scalar('Backtest/Total_Trades', results.total_trades, 0)
            
            # Strategy performance
            for strategy, stats in results.strategy_performance.items():
                self.tb_writer.add_scalar(f'Strategy/{strategy}/Total_Trades', stats['total_trades'], 0)
                self.tb_writer.add_scalar(f'Strategy/{strategy}/Avg_Confidence', stats['avg_confidence'], 0)
                self.tb_writer.add_scalar(f'Strategy/{strategy}/Win_Rate', stats['win_rate'], 0)
            
            # Equity curve visualization
            if not results.equity_curve.empty:
                fig, ax = plt.subplots(figsize=(12, 6))
                ax.plot(results.equity_curve.index, results.equity_curve.values)
                ax.set_title('Equity Curve')
                ax.set_xlabel('Time')
                ax.set_ylabel('Portfolio Value ($)')
                ax.grid(True)
                
                self.tb_writer.add_figure('Backtest/Equity_Curve', fig, 0)
                plt.close(fig)
            
            logger.info("Backtest results logged to TensorBoard")
            
        except Exception as e:
            logger.error(f"Error logging to TensorBoard: {e}")
    
    def _log_signal_to_tensorboard(self, signal: ShortSignal, trade_number: int):
        """Log individual signals to TensorBoard"""
        try:
            self.tb_writer.add_scalar(f'Signals/{signal.strategy}/Confidence', signal.confidence, trade_number)
            self.tb_writer.add_scalar(f'Signals/{signal.strategy}/Price', signal.price, trade_number)
            self.tb_writer.add_scalar(f'Signals/{signal.strategy}/Volume', signal.volume, trade_number)
            
        except Exception as e:
            logger.error(f"Error logging signal to TensorBoard: {e}")
    
    async def _store_results(self, run_id: str, results: SimpleBacktestResults):
        """Store backtest results in database"""
        try:
            conn = sqlite3.connect(self.db_path)
            
            # Store run summary
            conn.execute('''
                INSERT OR REPLACE INTO backtest_runs 
                (run_id, config, start_time, end_time, results)
                VALUES (?, ?, ?, ?, ?)
            ''', (
                run_id,
                json.dumps(asdict(results.config)),
                datetime.now(),
                datetime.now(),
                json.dumps({
                    'total_return': results.total_return,
                    'sharpe_ratio': results.sharpe_ratio,
                    'max_drawdown': results.max_drawdown,
                    'total_trades': results.total_trades
                })
            ))
            
            # Store individual trades
            for trade in results.trades:
                conn.execute('''
                    INSERT INTO trades 
                    (run_id, timestamp, symbol, strategy, signal_type, quantity, 
                     price, commission, total_cost, ai_confidence, metadata)
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', (
                    run_id,
                    trade.timestamp,
                    trade.symbol,
                    trade.strategy,
                    trade.signal_type,
                    trade.quantity,
                    trade.price,
                    trade.commission,
                    trade.total_cost,
                    trade.ai_confidence,
                    json.dumps(trade.metadata)
                ))
            
            conn.commit()
            conn.close()
            
            logger.info(f"Simple backtest results stored with ID: {run_id}")
            
        except Exception as e:
            logger.error(f"Error storing results: {e}")

def print_results(results: SimpleBacktestResults, title="Simple Backtest Results"):
    """Print backtest results in a formatted way"""
    print(f"\n📈 {title}")
    print("=" * 60)
    print(f"  📊 Performance Metrics:")
    print(f"    Total Return:     {results.total_return:>8.2f}%")
    print(f"    Annual Return:    {results.annual_return:>8.2f}%")
    print(f"    Max Drawdown:     {results.max_drawdown:>8.2f}%")
    print(f"    Sharpe Ratio:     {results.sharpe_ratio:>8.2f}")
    
    print(f"\n  📈 Trading Statistics:")
    print(f"    Total Trades:     {results.total_trades:>8}")
    print(f"    Win Rate:         {results.win_rate:>8.2f}%")
    
    print(f"\n  💰 Capital Analysis:")
    print(f"    Initial Capital:  ${results.initial_capital:>10,.2f}")
    print(f"    Final Capital:    ${results.final_capital:>10,.2f}")
    print(f"    Net P&L:          ${results.final_capital - results.initial_capital:>10,.2f}")
    
    if results.strategy_performance:
        print(f"\n  📊 Strategy Breakdown:")
        for strategy, stats in results.strategy_performance.items():
            print(f"    {strategy[:25]:<25} {stats['total_trades']:>3} trades, "
                  f"{stats['avg_confidence']:.2f} confidence")

async def run_simple_shorting_backtest():
    """Run a simple shorting strategies backtest"""
    # Use recent dates within yfinance limits for intraday data
    end_date = datetime.now()
    start_date = end_date - timedelta(days=30)  # Last 30 days
    
    config = SimpleBacktestConfig(
        start_date=start_date.strftime('%Y-%m-%d'),
        end_date=end_date.strftime('%Y-%m-%d'),
        initial_capital=100000.0,
        commission=0.001,
        slippage=0.0005,
        symbols=['AAPL', 'MSFT', 'TSLA', 'NVDA'],
        timeframes=['1hour', '1day'],  # Use longer timeframes for stability
        enable_shorting=True,
        max_leverage=2.0,
        benchmark='SPY'
    )
    
    engine = SimpleBacktestEngine(config)
    results = await engine.run_backtest()
    
    return results

async def main():
    """Example usage"""
    print("🚀 LynxTrader Simple Backtesting Engine")
    print("=" * 50)
    
    try:
        # Run simple shorting strategies backtest
        print("\n📊 Running Simple Shorting Strategies Backtest...")
        results = await run_simple_shorting_backtest()
        
        print_results(results, "Simple Shorting Strategies Results")
        
        print(f"\n⏱️  Backtest completed successfully")
        print(f"🎯 TensorBoard logs available in: tensorboard_logs/")
        print(f"💾 Results stored in: simple_backtest_results.db")
        
        print(f"\n📋 Next Steps:")
        print(f"  1. Start TensorBoard: python start_tensorboard.py")
        print(f"  2. View visualizations at http://localhost:6006")
        print(f"  3. Analyze results in the database")
        
    except Exception as e:
        print(f"\n❌ Simple backtest failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    asyncio.run(main()) 