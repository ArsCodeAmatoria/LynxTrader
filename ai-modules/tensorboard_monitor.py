"""
LynxTrader TensorBoard Monitor
Real-time visualization of trading performance, AI model metrics, and market data
"""

import os
import time
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from torch.utils.tensorboard import SummaryWriter
import threading
import asyncio
import websockets
import json
import logging
from typing import Dict, List, Any, Optional
import sqlite3
from dataclasses import dataclass, asdict
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from collections import deque, defaultdict
import yfinance as yf

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class TradingMetrics:
    """Trading performance metrics"""
    timestamp: datetime
    total_pnl: float
    daily_pnl: float
    win_rate: float
    total_trades: int
    active_positions: int
    max_drawdown: float
    sharpe_ratio: float
    roi: float

@dataclass
class ModelMetrics:
    """AI model performance metrics"""
    timestamp: datetime
    model_name: str
    accuracy: float
    precision: float
    recall: float
    f1_score: float
    prediction_confidence: float
    feature_importance: Dict[str, float]

@dataclass
class BotMetrics:
    """Individual bot performance"""
    timestamp: datetime
    bot_id: str
    bot_name: str
    status: str
    strategy: str
    pnl: float
    trades_count: int
    win_rate: float
    avg_trade_duration: float

class TensorBoardMonitor:
    """Main TensorBoard monitoring system for LynxTrader"""
    
    def __init__(self, log_dir: str = "tensorboard_logs", update_interval: int = 30):
        self.log_dir = log_dir
        self.update_interval = update_interval
        self.writer = SummaryWriter(log_dir=f"{log_dir}/lynxtrader_{datetime.now().strftime('%Y%m%d_%H%M%S')}")
        
        # Data storage
        self.trading_metrics_buffer = deque(maxlen=1000)
        self.model_metrics_buffer = deque(maxlen=1000)
        self.bot_metrics_buffer = defaultdict(lambda: deque(maxlen=1000))
        self.market_data_buffer = defaultdict(lambda: deque(maxlen=1000))
        
        # Database for persistence
        self.db_path = "monitoring_data.db"
        self.init_database()
        
        # Monitoring state
        self.is_running = False
        self.monitoring_thread = None
        
        logger.info(f"TensorBoard Monitor initialized. View at: tensorboard --logdir={log_dir}")
    
    def init_database(self):
        """Initialize SQLite database for metrics storage"""
        try:
            with sqlite3.connect(self.db_path) as conn:
                cursor = conn.cursor()
                
                # Trading metrics table
                cursor.execute('''
                    CREATE TABLE IF NOT EXISTS trading_metrics (
                        timestamp TEXT PRIMARY KEY,
                        total_pnl REAL,
                        daily_pnl REAL,
                        win_rate REAL,
                        total_trades INTEGER,
                        active_positions INTEGER,
                        max_drawdown REAL,
                        sharpe_ratio REAL,
                        roi REAL
                    )
                ''')
                
                # Model metrics table
                cursor.execute('''
                    CREATE TABLE IF NOT EXISTS model_metrics (
                        timestamp TEXT,
                        model_name TEXT,
                        accuracy REAL,
                        precision REAL,
                        recall REAL,
                        f1_score REAL,
                        prediction_confidence REAL,
                        feature_importance TEXT,
                        PRIMARY KEY (timestamp, model_name)
                    )
                ''')
                
                # Bot metrics table
                cursor.execute('''
                    CREATE TABLE IF NOT EXISTS bot_metrics (
                        timestamp TEXT,
                        bot_id TEXT,
                        bot_name TEXT,
                        status TEXT,
                        strategy TEXT,
                        pnl REAL,
                        trades_count INTEGER,
                        win_rate REAL,
                        avg_trade_duration REAL,
                        PRIMARY KEY (timestamp, bot_id)
                    )
                ''')
                
                conn.commit()
                logger.info("Database initialized successfully")
                
        except Exception as e:
            logger.error(f"Database initialization failed: {e}")
    
    def start_monitoring(self):
        """Start the monitoring process"""
        if self.is_running:
            logger.warning("Monitor is already running")
            return
        
        self.is_running = True
        self.monitoring_thread = threading.Thread(target=self._monitoring_loop, daemon=True)
        self.monitoring_thread.start()
        logger.info("TensorBoard monitoring started")
    
    def stop_monitoring(self):
        """Stop the monitoring process"""
        self.is_running = False
        if self.monitoring_thread:
            self.monitoring_thread.join()
        self.writer.close()
        logger.info("TensorBoard monitoring stopped")
    
    def _monitoring_loop(self):
        """Main monitoring loop"""
        while self.is_running:
            try:
                # Collect metrics from various sources
                self.collect_trading_metrics()
                self.collect_model_metrics()
                self.collect_bot_metrics()
                self.collect_market_data()
                
                # Update TensorBoard visualizations
                self.update_tensorboard()
                
                # Save to database
                self.save_metrics_to_db()
                
                time.sleep(self.update_interval)
                
            except Exception as e:
                logger.error(f"Error in monitoring loop: {e}")
                time.sleep(5)  # Brief pause before retrying
    
    def collect_trading_metrics(self):
        """Collect overall trading performance metrics"""
        try:
            # In a real implementation, this would fetch from your trading system
            # For now, we'll simulate some metrics
            now = datetime.now()
            
            # Simulate trading metrics (replace with actual data collection)
            metrics = TradingMetrics(
                timestamp=now,
                total_pnl=np.random.normal(156.78, 50),
                daily_pnl=np.random.normal(23.45, 10),
                win_rate=np.random.uniform(55, 75),
                total_trades=np.random.randint(20, 30),
                active_positions=np.random.randint(0, 5),
                max_drawdown=np.random.uniform(2, 8),
                sharpe_ratio=np.random.uniform(1.2, 2.5),
                roi=np.random.uniform(8, 15)
            )
            
            self.trading_metrics_buffer.append(metrics)
            logger.debug(f"Collected trading metrics: PnL={metrics.total_pnl:.2f}")
            
        except Exception as e:
            logger.error(f"Error collecting trading metrics: {e}")
    
    def collect_model_metrics(self):
        """Collect AI model performance metrics"""
        try:
            now = datetime.now()
            models = ["LSTM_Price_Predictor", "CNN_Pattern_Detector", "Transformer_Sentiment"]
            
            for model_name in models:
                metrics = ModelMetrics(
                    timestamp=now,
                    model_name=model_name,
                    accuracy=np.random.uniform(0.7, 0.9),
                    precision=np.random.uniform(0.65, 0.85),
                    recall=np.random.uniform(0.6, 0.8),
                    f1_score=np.random.uniform(0.62, 0.82),
                    prediction_confidence=np.random.uniform(0.7, 0.95),
                    feature_importance={
                        "price": np.random.uniform(0.2, 0.4),
                        "volume": np.random.uniform(0.1, 0.3),
                        "rsi": np.random.uniform(0.1, 0.2),
                        "macd": np.random.uniform(0.1, 0.2),
                        "sentiment": np.random.uniform(0.05, 0.15)
                    }
                )
                
                self.model_metrics_buffer.append(metrics)
            
            logger.debug(f"Collected metrics for {len(models)} models")
            
        except Exception as e:
            logger.error(f"Error collecting model metrics: {e}")
    
    def collect_bot_metrics(self):
        """Collect individual bot performance metrics"""
        try:
            now = datetime.now()
            bots = [
                ("bot_1", "Scalp-Master", "EMA Crossover"),
                ("bot_2", "Swing-Hunter", "RSI + Support"),
                ("bot_3", "Crypto-Flash", "Volume Spike")
            ]
            
            for bot_id, bot_name, strategy in bots:
                status = np.random.choice(["active", "paused", "error"], p=[0.7, 0.2, 0.1])
                
                metrics = BotMetrics(
                    timestamp=now,
                    bot_id=bot_id,
                    bot_name=bot_name,
                    status=status,
                    strategy=strategy,
                    pnl=np.random.normal(20, 15),
                    trades_count=np.random.randint(5, 20),
                    win_rate=np.random.uniform(50, 80),
                    avg_trade_duration=np.random.uniform(15, 120)  # minutes
                )
                
                self.bot_metrics_buffer[bot_id].append(metrics)
            
            logger.debug(f"Collected metrics for {len(bots)} bots")
            
        except Exception as e:
            logger.error(f"Error collecting bot metrics: {e}")
    
    def collect_market_data(self):
        """Collect real-time market data"""
        try:
            symbols = ['AAPL', 'MSFT', 'TSLA', 'SPY', 'QQQ']
            
            for symbol in symbols:
                # In a real implementation, this would fetch real-time data
                # For now, simulate market data
                now = datetime.now()
                base_price = {'AAPL': 150, 'MSFT': 300, 'TSLA': 200, 'SPY': 400, 'QQQ': 350}[symbol]
                
                market_data = {
                    'timestamp': now,
                    'symbol': symbol,
                    'price': base_price + np.random.normal(0, 5),
                    'volume': np.random.randint(1000000, 10000000),
                    'change_percent': np.random.uniform(-3, 3)
                }
                
                self.market_data_buffer[symbol].append(market_data)
            
            logger.debug(f"Collected market data for {len(symbols)} symbols")
            
        except Exception as e:
            logger.error(f"Error collecting market data: {e}")
    
    def update_tensorboard(self):
        """Update TensorBoard with latest metrics"""
        try:
            current_time = int(time.time())
            
            # Trading metrics
            if self.trading_metrics_buffer:
                latest_trading = self.trading_metrics_buffer[-1]
                self.writer.add_scalar('Trading/Total_PnL', latest_trading.total_pnl, current_time)
                self.writer.add_scalar('Trading/Daily_PnL', latest_trading.daily_pnl, current_time)
                self.writer.add_scalar('Trading/Win_Rate', latest_trading.win_rate, current_time)
                self.writer.add_scalar('Trading/Total_Trades', latest_trading.total_trades, current_time)
                self.writer.add_scalar('Trading/Active_Positions', latest_trading.active_positions, current_time)
                self.writer.add_scalar('Trading/Max_Drawdown', latest_trading.max_drawdown, current_time)
                self.writer.add_scalar('Trading/Sharpe_Ratio', latest_trading.sharpe_ratio, current_time)
                self.writer.add_scalar('Trading/ROI', latest_trading.roi, current_time)
            
            # Model metrics
            model_metrics_latest = {}
            for metrics in list(self.model_metrics_buffer)[-3:]:  # Last 3 model updates
                model_name = metrics.model_name
                self.writer.add_scalar(f'Models/{model_name}/Accuracy', metrics.accuracy, current_time)
                self.writer.add_scalar(f'Models/{model_name}/Precision', metrics.precision, current_time)
                self.writer.add_scalar(f'Models/{model_name}/Recall', metrics.recall, current_time)
                self.writer.add_scalar(f'Models/{model_name}/F1_Score', metrics.f1_score, current_time)
                self.writer.add_scalar(f'Models/{model_name}/Confidence', metrics.prediction_confidence, current_time)
                
                # Feature importance
                for feature, importance in metrics.feature_importance.items():
                    self.writer.add_scalar(f'Features/{model_name}/{feature}', importance, current_time)
            
            # Bot metrics
            for bot_id, bot_buffer in self.bot_metrics_buffer.items():
                if bot_buffer:
                    latest_bot = bot_buffer[-1]
                    self.writer.add_scalar(f'Bots/{latest_bot.bot_name}/PnL', latest_bot.pnl, current_time)
                    self.writer.add_scalar(f'Bots/{latest_bot.bot_name}/Trades', latest_bot.trades_count, current_time)
                    self.writer.add_scalar(f'Bots/{latest_bot.bot_name}/Win_Rate', latest_bot.win_rate, current_time)
                    self.writer.add_scalar(f'Bots/{latest_bot.bot_name}/Avg_Duration', latest_bot.avg_trade_duration, current_time)
            
            # Market data
            for symbol, market_buffer in self.market_data_buffer.items():
                if market_buffer:
                    latest_market = market_buffer[-1]
                    self.writer.add_scalar(f'Market/{symbol}/Price', latest_market['price'], current_time)
                    self.writer.add_scalar(f'Market/{symbol}/Volume', latest_market['volume'], current_time)
                    self.writer.add_scalar(f'Market/{symbol}/Change_Percent', latest_market['change_percent'], current_time)
            
            # Create custom plots every 10 updates
            if current_time % (self.update_interval * 10) == 0:
                self.create_custom_plots()
            
            self.writer.flush()
            
        except Exception as e:
            logger.error(f"Error updating TensorBoard: {e}")
    
    def create_custom_plots(self):
        """Create custom matplotlib plots for TensorBoard"""
        try:
            # PnL over time plot
            if len(self.trading_metrics_buffer) > 10:
                timestamps = [m.timestamp for m in self.trading_metrics_buffer]
                pnl_values = [m.total_pnl for m in self.trading_metrics_buffer]
                
                fig, ax = plt.subplots(figsize=(12, 6))
                ax.plot(timestamps, pnl_values, 'b-', linewidth=2, label='Total P&L')
                ax.set_title('Total P&L Over Time')
                ax.set_xlabel('Time')
                ax.set_ylabel('P&L ($)')
                ax.grid(True, alpha=0.3)
                ax.legend()
                
                # Format x-axis
                ax.xaxis.set_major_formatter(mdates.DateFormatter('%H:%M'))
                ax.xaxis.set_major_locator(mdates.MinuteLocator(interval=30))
                plt.xticks(rotation=45)
                plt.tight_layout()
                
                self.writer.add_figure('Custom_Plots/PnL_Timeline', fig, int(time.time()))
                plt.close(fig)
            
            # Bot performance comparison
            if self.bot_metrics_buffer:
                bot_names = []
                bot_pnls = []
                bot_win_rates = []
                
                for bot_id, bot_buffer in self.bot_metrics_buffer.items():
                    if bot_buffer:
                        latest = bot_buffer[-1]
                        bot_names.append(latest.bot_name)
                        bot_pnls.append(latest.pnl)
                        bot_win_rates.append(latest.win_rate)
                
                if bot_names:
                    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(15, 6))
                    
                    # PnL comparison
                    bars1 = ax1.bar(bot_names, bot_pnls, color=['green' if p > 0 else 'red' for p in bot_pnls])
                    ax1.set_title('Bot P&L Comparison')
                    ax1.set_ylabel('P&L ($)')
                    ax1.tick_params(axis='x', rotation=45)
                    
                    # Win rate comparison
                    bars2 = ax2.bar(bot_names, bot_win_rates, color='blue', alpha=0.7)
                    ax2.set_title('Bot Win Rate Comparison')
                    ax2.set_ylabel('Win Rate (%)')
                    ax2.set_ylim(0, 100)
                    ax2.tick_params(axis='x', rotation=45)
                    
                    plt.tight_layout()
                    self.writer.add_figure('Custom_Plots/Bot_Comparison', fig, int(time.time()))
                    plt.close(fig)
            
        except Exception as e:
            logger.error(f"Error creating custom plots: {e}")
    
    def save_metrics_to_db(self):
        """Save metrics to SQLite database for persistence"""
        try:
            with sqlite3.connect(self.db_path) as conn:
                cursor = conn.cursor()
                
                # Save trading metrics
                if self.trading_metrics_buffer:
                    latest_trading = self.trading_metrics_buffer[-1]
                    cursor.execute('''
                        INSERT OR REPLACE INTO trading_metrics VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ''', (
                        latest_trading.timestamp.isoformat(),
                        latest_trading.total_pnl,
                        latest_trading.daily_pnl,
                        latest_trading.win_rate,
                        latest_trading.total_trades,
                        latest_trading.active_positions,
                        latest_trading.max_drawdown,
                        latest_trading.sharpe_ratio,
                        latest_trading.roi
                    ))
                
                # Save model metrics
                for metrics in list(self.model_metrics_buffer)[-3:]:
                    cursor.execute('''
                        INSERT OR REPLACE INTO model_metrics VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                    ''', (
                        metrics.timestamp.isoformat(),
                        metrics.model_name,
                        metrics.accuracy,
                        metrics.precision,
                        metrics.recall,
                        metrics.f1_score,
                        metrics.prediction_confidence,
                        json.dumps(metrics.feature_importance)
                    ))
                
                # Save bot metrics
                for bot_id, bot_buffer in self.bot_metrics_buffer.items():
                    if bot_buffer:
                        latest_bot = bot_buffer[-1]
                        cursor.execute('''
                            INSERT OR REPLACE INTO bot_metrics VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                        ''', (
                            latest_bot.timestamp.isoformat(),
                            latest_bot.bot_id,
                            latest_bot.bot_name,
                            latest_bot.status,
                            latest_bot.strategy,
                            latest_bot.pnl,
                            latest_bot.trades_count,
                            latest_bot.win_rate,
                            latest_bot.avg_trade_duration
                        ))
                
                conn.commit()
                
        except Exception as e:
            logger.error(f"Error saving metrics to database: {e}")
    
    def get_historical_data(self, table_name: str, hours: int = 24) -> pd.DataFrame:
        """Get historical data from database"""
        try:
            cutoff_time = datetime.now() - timedelta(hours=hours)
            
            with sqlite3.connect(self.db_path) as conn:
                query = f'''
                    SELECT * FROM {table_name} 
                    WHERE timestamp > ? 
                    ORDER BY timestamp DESC
                '''
                df = pd.read_sql_query(query, conn, params=[cutoff_time.isoformat()])
                return df
                
        except Exception as e:
            logger.error(f"Error retrieving historical data: {e}")
            return pd.DataFrame()

def main():
    """Main function to run TensorBoard monitoring"""
    monitor = TensorBoardMonitor(
        log_dir="tensorboard_logs/lynxtrader",
        update_interval=30  # Update every 30 seconds
    )
    
    try:
        monitor.start_monitoring()
        
        # Keep the monitor running
        print("TensorBoard Monitor is running...")
        print("View dashboard at: http://localhost:6006")
        print("Start TensorBoard with: tensorboard --logdir=tensorboard_logs")
        print("Press Ctrl+C to stop...")
        
        while True:
            time.sleep(1)
            
    except KeyboardInterrupt:
        print("\nStopping TensorBoard Monitor...")
        monitor.stop_monitoring()
        print("Monitor stopped.")

if __name__ == "__main__":
    main() 