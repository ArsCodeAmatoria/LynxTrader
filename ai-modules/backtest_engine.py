"""
LynxTrader Advanced Backtesting Engine
Multi-source data provider with TensorBoard integration for shorting strategies
"""

import os
import time
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from typing import Dict, List, Any, Optional, Tuple, Union
from dataclasses import dataclass, asdict
import asyncio
import logging
from concurrent.futures import ThreadPoolExecutor
import sqlite3
import json

# Data Sources
import yfinance as yf
import alpaca_trade_api as tradeapi
from alpha_vantage.timeseries import TimeSeries
import ccxt

# TensorBoard Integration
from torch.utils.tensorboard import SummaryWriter
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import seaborn as sns

# Our modules
from shorting_signals import ShortingSignalEngine, ShortSignal
from enhanced_tensorboard import EnhancedTensorBoardMonitor
from tensorboard_config import TensorBoardConfig

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class BacktestConfig:
    """Backtesting configuration"""
    start_date: str
    end_date: str
    initial_capital: float
    commission: float = 0.001  # 0.1%
    slippage: float = 0.0005   # 0.05%
    data_sources: List[str] = None  # ['yfinance', 'alpaca', 'polygon']
    symbols: List[str] = None
    timeframes: List[str] = None  # ['1min', '5min', '15min', '1hour', '1day']
    enable_shorting: bool = True
    max_leverage: float = 2.0
    benchmark: str = 'SPY'

@dataclass
class TradeExecution:
    """Individual trade execution record"""
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
class BacktestResults:
    """Comprehensive backtest results"""
    config: BacktestConfig
    start_date: datetime
    end_date: datetime
    initial_capital: float
    final_capital: float
    total_return: float
    annual_return: float
    max_drawdown: float
    sharpe_ratio: float
    sortino_ratio: float
    calmar_ratio: float
    win_rate: float
    profit_factor: float
    total_trades: int
    avg_trade_duration: timedelta
    best_trade: float
    worst_trade: float
    trades: List[TradeExecution]
    daily_returns: pd.Series
    equity_curve: pd.Series
    benchmark_returns: pd.Series
    strategy_performance: Dict[str, Dict[str, float]]

class DataProvider:
    """Multi-source data provider for backtesting"""
    
    def __init__(self):
        self.yf_session = None
        self.alpaca_client = None
        self.alpha_vantage = None
        self.crypto_exchange = None
        self._setup_providers()
    
    def _setup_providers(self):
        """Initialize data providers"""
        try:
            # Alpha Vantage setup
            av_key = os.getenv('ALPHA_VANTAGE_API_KEY')
            if av_key:
                self.alpha_vantage = TimeSeries(key=av_key, output_format='pandas')
            
            # Alpaca setup
            alpaca_key = os.getenv('ALPACA_API_KEY')
            alpaca_secret = os.getenv('ALPACA_SECRET_KEY')
            if alpaca_key and alpaca_secret:
                self.alpaca_client = tradeapi.REST(
                    alpaca_key, 
                    alpaca_secret, 
                    base_url='https://paper-api.alpaca.markets'  # Paper trading
                )
            
            # Crypto exchange setup
            self.crypto_exchange = ccxt.binance({
                'sandbox': True,  # Use testnet
                'enableRateLimit': True,
            })
            
            logger.info("Data providers initialized successfully")
            
        except Exception as e:
            logger.warning(f"Some data providers failed to initialize: {e}")
    
    async def fetch_data(self, symbol: str, timeframe: str, start_date: str, 
                        end_date: str, source: str = 'yfinance') -> pd.DataFrame:
        """Fetch historical data from specified source"""
        try:
            if source == 'yfinance':
                return await self._fetch_yfinance_data(symbol, timeframe, start_date, end_date)
            elif source == 'alpaca' and self.alpaca_client:
                return await self._fetch_alpaca_data(symbol, timeframe, start_date, end_date)
            elif source == 'alpha_vantage' and self.alpha_vantage:
                return await self._fetch_alpha_vantage_data(symbol, timeframe, start_date, end_date)
            elif source == 'crypto' and 'USD' in symbol:
                return await self._fetch_crypto_data(symbol, timeframe, start_date, end_date)
            else:
                logger.warning(f"Source {source} not available, falling back to yfinance")
                return await self._fetch_yfinance_data(symbol, timeframe, start_date, end_date)
                
        except Exception as e:
            logger.error(f"Error fetching data for {symbol} from {source}: {e}")
            return pd.DataFrame()
    
    async def _fetch_yfinance_data(self, symbol: str, timeframe: str, 
                                  start_date: str, end_date: str) -> pd.DataFrame:
        """Fetch data from Yahoo Finance"""
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
    
    async def _fetch_alpaca_data(self, symbol: str, timeframe: str, 
                                start_date: str, end_date: str) -> pd.DataFrame:
        """Fetch data from Alpaca Markets"""
        try:
            timeframe_map = {
                '1min': '1Min', '5min': '5Min', '15min': '15Min',
                '30min': '30Min', '1hour': '1Hour', '1day': '1Day'
            }
            
            tf = timeframe_map.get(timeframe, '5Min')
            
            bars = self.alpaca_client.get_bars(
                symbol,
                tf,
                start=start_date,
                end=end_date,
                adjustment='raw'
            ).df
            
            if bars.empty:
                return pd.DataFrame()
            
            bars = bars.rename(columns={
                'open': 'open', 'high': 'high', 'low': 'low', 
                'close': 'close', 'volume': 'volume'
            })
            
            bars['symbol'] = symbol
            bars['timestamp'] = bars.index
            bars.reset_index(drop=True, inplace=True)
            
            logger.info(f"Fetched {len(bars)} bars for {symbol} from Alpaca")
            return bars
            
        except Exception as e:
            logger.error(f"Alpaca error for {symbol}: {e}")
            return pd.DataFrame()
    
    async def _fetch_crypto_data(self, symbol: str, timeframe: str, 
                                start_date: str, end_date: str) -> pd.DataFrame:
        """Fetch cryptocurrency data"""
        try:
            # Convert symbol format (e.g., BTC-USD -> BTC/USDT)
            crypto_symbol = symbol.replace('-USD', '/USDT')
            
            timeframe_map = {
                '1min': '1m', '5min': '5m', '15min': '15m',
                '30min': '30m', '1hour': '1h', '1day': '1d'
            }
            
            tf = timeframe_map.get(timeframe, '5m')
            
            # Convert dates to milliseconds
            start_ts = int(pd.to_datetime(start_date).timestamp() * 1000)
            end_ts = int(pd.to_datetime(end_date).timestamp() * 1000)
            
            ohlcv = self.crypto_exchange.fetch_ohlcv(
                crypto_symbol, tf, start_ts, limit=1000
            )
            
            df = pd.DataFrame(ohlcv, columns=['timestamp', 'open', 'high', 'low', 'close', 'volume'])
            df['timestamp'] = pd.to_datetime(df['timestamp'], unit='ms')
            df['symbol'] = symbol
            
            logger.info(f"Fetched {len(df)} bars for {symbol} from crypto exchange")
            return df
            
        except Exception as e:
            logger.error(f"Crypto exchange error for {symbol}: {e}")
            return pd.DataFrame()

class BacktestEngine:
    """Advanced backtesting engine with TensorBoard integration"""
    
    def __init__(self, config: BacktestConfig):
        self.config = config
        self.data_provider = DataProvider()
        self.shorting_engine = ShortingSignalEngine()
        self.tb_monitor = EnhancedTensorBoardMonitor()
        
        # Backtest state
        self.cash = config.initial_capital
        self.positions = {}  # symbol -> quantity (negative for short)
        self.trades = []
        self.daily_values = []
        self.current_time = None
        
        # TensorBoard setup
        self.tb_writer = SummaryWriter(
            log_dir=f"tensorboard_logs/backtest_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        )
        
        # Database for results
        self.setup_database()
    
    def setup_database(self):
        """Setup SQLite database for backtest results"""
        self.db_path = 'backtest_results.db'
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
        
        logger.info("Backtest database initialized")
    
    async def run_backtest(self) -> BacktestResults:
        """Execute the complete backtesting process"""
        logger.info("Starting comprehensive backtest...")
        start_time = time.time()
        
        # Generate unique run ID
        run_id = f"backtest_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        try:
            # 1. Data Collection Phase
            logger.info("Phase 1: Collecting market data...")
            market_data = await self._collect_market_data()
            
            if not market_data:
                raise ValueError("No market data collected")
            
            # 2. Strategy Testing Phase
            logger.info("Phase 2: Testing shorting strategies...")
            await self._test_strategies(market_data)
            
            # 3. Performance Analysis Phase
            logger.info("Phase 3: Analyzing performance...")
            results = await self._analyze_performance(run_id)
            
            # 4. TensorBoard Logging Phase
            logger.info("Phase 4: Logging to TensorBoard...")
            await self._log_to_tensorboard(results)
            
            # 5. Database Storage Phase
            logger.info("Phase 5: Storing results...")
            await self._store_results(run_id, results)
            
            execution_time = time.time() - start_time
            logger.info(f"Backtest completed in {execution_time:.2f} seconds")
            
            return results
            
        except Exception as e:
            logger.error(f"Backtest failed: {e}")
            raise
    
    async def _collect_market_data(self) -> Dict[str, pd.DataFrame]:
        """Collect market data from multiple sources"""
        market_data = {}
        
        symbols = self.config.symbols or ['AAPL', 'MSFT', 'TSLA', 'SPY', 'QQQ']
        timeframes = self.config.timeframes or ['5min', '15min']
        sources = self.config.data_sources or ['yfinance']
        
        tasks = []
        with ThreadPoolExecutor(max_workers=10) as executor:
            for symbol in symbols:
                for timeframe in timeframes:
                    for source in sources:
                        task = executor.submit(
                            asyncio.run,
                            self.data_provider.fetch_data(
                                symbol, timeframe, 
                                self.config.start_date, self.config.end_date, 
                                source
                            )
                        )
                        tasks.append((symbol, timeframe, source, task))
        
        for symbol, timeframe, source, task in tasks:
            try:
                df = task.result()
                if not df.empty:
                    key = f"{symbol}_{timeframe}_{source}"
                    market_data[key] = df
                    logger.info(f"Loaded {len(df)} bars for {key}")
            except Exception as e:
                logger.error(f"Failed to load {symbol}_{timeframe}_{source}: {e}")
        
        logger.info(f"Collected data for {len(market_data)} symbol/timeframe/source combinations")
        return market_data
    
    async def _test_strategies(self, market_data: Dict[str, pd.DataFrame]):
        """Test shorting strategies against historical data"""
        for data_key, df in market_data.items():
            symbol, timeframe, source = data_key.split('_')
            
            logger.info(f"Testing strategies on {symbol} {timeframe} from {source}")
            
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
            trade = TradeExecution(
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
    
    async def _analyze_performance(self, run_id: str) -> BacktestResults:
        """Comprehensive performance analysis"""
        if not self.trades:
            logger.warning("No trades executed during backtest")
        
        # Calculate daily returns and equity curve
        trade_df = pd.DataFrame([asdict(trade) for trade in self.trades])
        
        if not trade_df.empty:
            trade_df['timestamp'] = pd.to_datetime(trade_df['timestamp'])
            trade_df['pnl'] = 0  # Will be calculated based on position closes
        
        # Calculate portfolio value over time
        equity_curve = self._calculate_equity_curve(trade_df)
        daily_returns = equity_curve.pct_change().dropna()
        
        # Performance metrics
        total_return = (equity_curve.iloc[-1] / self.config.initial_capital - 1) * 100
        annual_return = self._calculate_annual_return(daily_returns)
        max_drawdown = self._calculate_max_drawdown(equity_curve)
        sharpe_ratio = self._calculate_sharpe_ratio(daily_returns)
        sortino_ratio = self._calculate_sortino_ratio(daily_returns)
        
        # Trade statistics
        profitable_trades = len([t for t in self.trades if t.total_cost < 0])  # Simplified
        win_rate = (profitable_trades / len(self.trades) * 100) if self.trades else 0
        
        # Strategy performance breakdown
        strategy_performance = self._analyze_strategy_performance(trade_df)
        
        results = BacktestResults(
            config=self.config,
            start_date=pd.to_datetime(self.config.start_date),
            end_date=pd.to_datetime(self.config.end_date),
            initial_capital=self.config.initial_capital,
            final_capital=equity_curve.iloc[-1] if not equity_curve.empty else self.config.initial_capital,
            total_return=total_return,
            annual_return=annual_return,
            max_drawdown=max_drawdown,
            sharpe_ratio=sharpe_ratio,
            sortino_ratio=sortino_ratio,
            calmar_ratio=annual_return / max_drawdown if max_drawdown != 0 else 0,
            win_rate=win_rate,
            profit_factor=1.0,  # Simplified
            total_trades=len(self.trades),
            avg_trade_duration=timedelta(hours=2),  # Simplified
            best_trade=0.0,  # Simplified
            worst_trade=0.0,  # Simplified
            trades=self.trades,
            daily_returns=daily_returns,
            equity_curve=equity_curve,
            benchmark_returns=pd.Series(),  # Could add benchmark comparison
            strategy_performance=strategy_performance
        )
        
        return results
    
    def _calculate_equity_curve(self, trade_df: pd.DataFrame) -> pd.Series:
        """Calculate portfolio equity curve over time"""
        if trade_df.empty:
            return pd.Series([self.config.initial_capital])
        
        # Simplified equity curve - just track cash changes
        trade_df_sorted = trade_df.sort_values('timestamp')
        cash_changes = -trade_df_sorted['total_cost'].cumsum()
        equity_curve = self.config.initial_capital + cash_changes
        
        return equity_curve.reset_index(drop=True)
    
    def _calculate_annual_return(self, daily_returns: pd.Series) -> float:
        """Calculate annualized return"""
        if daily_returns.empty:
            return 0.0
        
        cumulative_return = (1 + daily_returns).prod() - 1
        days = len(daily_returns)
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
    
    def _calculate_sortino_ratio(self, daily_returns: pd.Series) -> float:
        """Calculate Sortino ratio"""
        if daily_returns.empty:
            return 0.0
        
        negative_returns = daily_returns[daily_returns < 0]
        if len(negative_returns) == 0:
            return float('inf')
        
        downside_deviation = negative_returns.std() * np.sqrt(252)
        excess_returns = daily_returns - 0.02/252
        sortino = excess_returns.mean() * np.sqrt(252) / downside_deviation
        
        return sortino
    
    def _analyze_strategy_performance(self, trade_df: pd.DataFrame) -> Dict[str, Dict[str, float]]:
        """Analyze performance by strategy"""
        if trade_df.empty:
            return {}
        
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
    
    async def _log_to_tensorboard(self, results: BacktestResults):
        """Log backtest results to TensorBoard"""
        try:
            # Overall performance metrics
            self.tb_writer.add_scalar('Backtest/Total_Return', results.total_return, 0)
            self.tb_writer.add_scalar('Backtest/Annual_Return', results.annual_return, 0)
            self.tb_writer.add_scalar('Backtest/Max_Drawdown', results.max_drawdown, 0)
            self.tb_writer.add_scalar('Backtest/Sharpe_Ratio', results.sharpe_ratio, 0)
            self.tb_writer.add_scalar('Backtest/Sortino_Ratio', results.sortino_ratio, 0)
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
    
    async def _store_results(self, run_id: str, results: BacktestResults):
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
            
            logger.info(f"Backtest results stored with ID: {run_id}")
            
        except Exception as e:
            logger.error(f"Error storing results: {e}")

# Utility functions for running backtests

async def run_shorting_backtest():
    """Run a comprehensive shorting strategies backtest"""
    config = BacktestConfig(
        start_date='2024-01-01',
        end_date='2024-06-01',
        initial_capital=100000.0,
        commission=0.001,
        slippage=0.0005,
        data_sources=['yfinance', 'alpaca'],
        symbols=['AAPL', 'MSFT', 'TSLA', 'NVDA', 'QQQ', 'SPY'],
        timeframes=['5min', '15min'],
        enable_shorting=True,
        max_leverage=2.0,
        benchmark='SPY'
    )
    
    engine = BacktestEngine(config)
    results = await engine.run_backtest()
    
    return results

async def run_crypto_backtest():
    """Run a cryptocurrency shorting backtest"""
    config = BacktestConfig(
        start_date='2024-01-01',
        end_date='2024-06-01',
        initial_capital=50000.0,
        commission=0.001,
        slippage=0.001,  # Higher slippage for crypto
        data_sources=['crypto', 'yfinance'],
        symbols=['BTC-USD', 'ETH-USD', 'SOL-USD'],
        timeframes=['5min', '15min', '1hour'],
        enable_shorting=True,
        max_leverage=3.0,
        benchmark='BTC-USD'
    )
    
    engine = BacktestEngine(config)
    results = await engine.run_backtest()
    
    return results

async def main():
    """Example usage"""
    print("🚀 LynxTrader Advanced Backtesting Engine")
    print("=" * 50)
    
    # Run shorting strategies backtest
    print("\n📊 Running Shorting Strategies Backtest...")
    shorting_results = await run_shorting_backtest()
    
    print(f"\n✅ Shorting Backtest Results:")
    print(f"  Total Return: {shorting_results.total_return:.2f}%")
    print(f"  Sharpe Ratio: {shorting_results.sharpe_ratio:.2f}")
    print(f"  Max Drawdown: {shorting_results.max_drawdown:.2f}%")
    print(f"  Win Rate: {shorting_results.win_rate:.2f}%")
    print(f"  Total Trades: {shorting_results.total_trades}")
    
    print("\n🎯 TensorBoard is running at: http://localhost:6006")
    print("📊 Check the Backtest dashboard for detailed visualizations!")

if __name__ == "__main__":
    asyncio.run(main())