"""
LynxTrader Enhanced TensorBoard Monitor
Advanced visualization system for shorting strategies, AI models, and real-time trading metrics
"""

import os
import time
import numpy as np
import pandas as pd
from datetime import datetime, timedelta
from torch.utils.tensorboard import SummaryWriter
import threading
import asyncio
import json
import logging
from typing import Dict, List, Any, Optional, Tuple
import sqlite3
from dataclasses import dataclass, asdict
import matplotlib
matplotlib.use('Agg')  # Use non-GUI backend for threading compatibility
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from collections import deque, defaultdict
import yfinance as yf
import plotly.graph_objects as go
import plotly.express as px
from plotly.subplots import make_subplots
import seaborn as sns

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class ShortingMetrics:
    """Shorting strategy specific metrics"""
    timestamp: datetime
    strategy_name: str
    signal_type: str  # 'fake_breakout', 'vwap_slap', 'bear_flag', etc.
    entry_price: float
    current_price: float
    unrealized_pnl: float
    volume_confirmation: float
    ai_confidence: float
    liquidity_score: float
    risk_score: float

@dataclass
class AIModelPerformance:
    """Enhanced AI model metrics with confidence intervals"""
    timestamp: datetime
    model_name: str
    strategy_type: str
    accuracy: float
    precision: float
    recall: float
    f1_score: float
    prediction_confidence: float
    feature_importance: Dict[str, float]
    confusion_matrix: List[List[int]]
    confidence_intervals: Dict[str, Tuple[float, float]]
    model_drift_score: float

@dataclass
class RiskMetrics:
    """Real-time risk monitoring"""
    timestamp: datetime
    portfolio_heat: float
    max_drawdown: float
    var_95: float
    var_99: float
    beta: float
    correlation_matrix: Dict[str, Dict[str, float]]
    position_sizing: Dict[str, float]
    kelly_fraction: float

class EnhancedTensorBoardMonitor:
    """Advanced TensorBoard monitoring with shorting strategy focus"""
    
    def __init__(self, log_dir: str = "tensorboard_logs/enhanced", update_interval: int = 15):
        self.log_dir = log_dir
        self.update_interval = update_interval
        self.writer = SummaryWriter(log_dir=f"{log_dir}/lynxtrader_{datetime.now().strftime('%Y%m%d_%H%M%S')}")
        
        # Enhanced data storage
        self.shorting_metrics_buffer = deque(maxlen=2000)
        self.ai_model_buffer = deque(maxlen=1000)
        self.risk_metrics_buffer = deque(maxlen=1000)
        self.market_regime_buffer = deque(maxlen=500)
        self.strategy_performance = defaultdict(lambda: deque(maxlen=500))
        
        # Real-time monitoring flags
        self.is_running = False
        self.monitoring_thread = None
        
        # Database for enhanced persistence
        self.db_path = "enhanced_monitoring.db"
        self.init_enhanced_database()
        
        logger.info(f"Enhanced TensorBoard Monitor initialized. View at: tensorboard --logdir={log_dir}")
    
    def init_enhanced_database(self):
        """Initialize enhanced SQLite database"""
        try:
            with sqlite3.connect(self.db_path) as conn:
                cursor = conn.cursor()
                
                # Shorting metrics table
                cursor.execute('''
                    CREATE TABLE IF NOT EXISTS shorting_metrics (
                        timestamp TEXT,
                        strategy_name TEXT,
                        signal_type TEXT,
                        entry_price REAL,
                        current_price REAL,
                        unrealized_pnl REAL,
                        volume_confirmation REAL,
                        ai_confidence REAL,
                        liquidity_score REAL,
                        risk_score REAL,
                        PRIMARY KEY (timestamp, strategy_name)
                    )
                ''')
                
                # AI model performance table
                cursor.execute('''
                    CREATE TABLE IF NOT EXISTS ai_model_performance (
                        timestamp TEXT,
                        model_name TEXT,
                        strategy_type TEXT,
                        accuracy REAL,
                        precision REAL,
                        recall REAL,
                        f1_score REAL,
                        prediction_confidence REAL,
                        feature_importance TEXT,
                        confusion_matrix TEXT,
                        confidence_intervals TEXT,
                        model_drift_score REAL,
                        PRIMARY KEY (timestamp, model_name)
                    )
                ''')
                
                # Risk metrics table
                cursor.execute('''
                    CREATE TABLE IF NOT EXISTS risk_metrics (
                        timestamp TEXT PRIMARY KEY,
                        portfolio_heat REAL,
                        max_drawdown REAL,
                        var_95 REAL,
                        var_99 REAL,
                        beta REAL,
                        correlation_matrix TEXT,
                        position_sizing TEXT,
                        kelly_fraction REAL
                    )
                ''')
                
                conn.commit()
                logger.info("Enhanced database initialized successfully")
                
        except Exception as e:
            logger.error(f"Enhanced database initialization failed: {e}")
    
    def start_monitoring(self):
        """Start enhanced monitoring with shorting strategy focus"""
        if self.is_running:
            logger.warning("Enhanced monitor is already running")
            return
        
        self.is_running = True
        self.monitoring_thread = threading.Thread(target=self._enhanced_monitoring_loop, daemon=True)
        self.monitoring_thread.start()
        logger.info("Enhanced TensorBoard monitoring started")
    
    def stop_monitoring(self):
        """Stop enhanced monitoring"""
        self.is_running = False
        if self.monitoring_thread:
            self.monitoring_thread.join()
        self.writer.close()
        logger.info("Enhanced TensorBoard monitoring stopped")
    
    def _enhanced_monitoring_loop(self):
        """Enhanced monitoring loop with shorting strategy metrics"""
        while self.is_running:
            try:
                # Collect enhanced metrics
                self.collect_shorting_metrics()
                self.collect_ai_model_performance()
                self.collect_risk_metrics()
                self.collect_market_regime_data()
                
                # Update TensorBoard with enhanced visualizations
                self.update_enhanced_tensorboard()
                
                # Create advanced plots
                self.create_advanced_plots()
                
                # Save to enhanced database
                self.save_enhanced_metrics()
                
                time.sleep(self.update_interval)
                
            except Exception as e:
                logger.error(f"Error in enhanced monitoring loop: {e}")
                time.sleep(5)
    
    def collect_shorting_metrics(self):
        """Collect shorting strategy specific metrics"""
        try:
            now = datetime.now()
            
            # Simulate shorting strategy metrics (replace with actual data)
            shorting_strategies = [
                'Fake Breakout Reversal',
                'VWAP Slap', 
                'Bear Flag Breakdown',
                'Supply Zone Rejection',
                'Liquidity Sweep Trap'
            ]
            
            for strategy in shorting_strategies:
                metrics = ShortingMetrics(
                    timestamp=now,
                    strategy_name=strategy,
                    signal_type=strategy.lower().replace(' ', '_'),
                    entry_price=np.random.uniform(150, 200),
                    current_price=np.random.uniform(145, 205),
                    unrealized_pnl=np.random.normal(50, 100),
                    volume_confirmation=np.random.uniform(0.6, 1.4),
                    ai_confidence=np.random.uniform(0.7, 0.95),
                    liquidity_score=np.random.uniform(0.5, 1.0),
                    risk_score=np.random.uniform(2.0, 4.5)
                )
                
                self.shorting_metrics_buffer.append(metrics)
                self.strategy_performance[strategy].append(metrics)
                
            logger.debug(f"Collected metrics for {len(shorting_strategies)} shorting strategies")
            
        except Exception as e:
            logger.error(f"Error collecting shorting metrics: {e}")
    
    def collect_ai_model_performance(self):
        """Collect enhanced AI model performance metrics"""
        try:
            now = datetime.now()
            
            ai_models = [
                ('Liquidity_Map_Generator', 'smart_money'),
                ('Volume_Divergence_Detector', 'scalping'),
                ('Exhaustion_Pattern_Scanner', 'swing'),
                ('Smart_Money_Flow_Tracker', 'smart_money')
            ]
            
            for model_name, strategy_type in ai_models:
                metrics = AIModelPerformance(
                    timestamp=now,
                    model_name=model_name,
                    strategy_type=strategy_type,
                    accuracy=np.random.uniform(0.75, 0.92),
                    precision=np.random.uniform(0.70, 0.88),
                    recall=np.random.uniform(0.68, 0.85),
                    f1_score=np.random.uniform(0.72, 0.86),
                    prediction_confidence=np.random.uniform(0.80, 0.95),
                    feature_importance={
                        'volume_profile': np.random.uniform(0.2, 0.4),
                        'price_action': np.random.uniform(0.15, 0.35),
                        'market_structure': np.random.uniform(0.1, 0.3),
                        'order_flow': np.random.uniform(0.05, 0.25)
                    },
                    confusion_matrix=[[85, 12], [8, 95]],
                    confidence_intervals={
                        'accuracy': (0.72, 0.89),
                        'precision': (0.68, 0.85)
                    },
                    model_drift_score=np.random.uniform(0.02, 0.15)
                )
                
                self.ai_model_buffer.append(metrics)
                
            logger.debug(f"Collected AI model metrics for {len(ai_models)} models")
            
        except Exception as e:
            logger.error(f"Error collecting AI model metrics: {e}")
    
    def collect_risk_metrics(self):
        """Collect comprehensive risk metrics"""
        try:
            now = datetime.now()
            
            metrics = RiskMetrics(
                timestamp=now,
                portfolio_heat=np.random.uniform(45, 75),
                max_drawdown=np.random.uniform(2, 8),
                var_95=np.random.uniform(1.5, 4.5),
                var_99=np.random.uniform(3.0, 8.0),
                beta=np.random.uniform(0.8, 1.2),
                correlation_matrix={
                    'SPY': {'QQQ': 0.85, 'IWM': 0.72},
                    'QQQ': {'SPY': 0.85, 'IWM': 0.68},
                    'IWM': {'SPY': 0.72, 'QQQ': 0.68}
                },
                position_sizing={
                    'scalping': np.random.uniform(0.1, 0.3),
                    'day_trading': np.random.uniform(0.2, 0.4),
                    'swing': np.random.uniform(0.3, 0.5)
                },
                kelly_fraction=np.random.uniform(0.12, 0.18)
            )
            
            self.risk_metrics_buffer.append(metrics)
            logger.debug("Collected risk metrics")
            
        except Exception as e:
            logger.error(f"Error collecting risk metrics: {e}")
    
    def collect_market_regime_data(self):
        """Collect market regime and volatility data"""
        try:
            now = datetime.now()
            
            # Simulate market regime data
            regime_data = {
                'timestamp': now,
                'volatility_regime': np.random.choice(['Low', 'Medium', 'High']),
                'trend_strength': np.random.uniform(0.3, 0.9),
                'market_breadth': np.random.uniform(0.4, 0.8),
                'fear_greed_index': np.random.uniform(20, 80),
                'vix_level': np.random.uniform(15, 35)
            }
            
            self.market_regime_buffer.append(regime_data)
            logger.debug("Collected market regime data")
            
        except Exception as e:
            logger.error(f"Error collecting market regime data: {e}")
    
    def update_enhanced_tensorboard(self):
        """Update TensorBoard with enhanced visualizations"""
        try:
            current_time = int(time.time())
            
            # Shorting strategy metrics
            if self.shorting_metrics_buffer:
                for strategy_name in ['Fake Breakout Reversal', 'VWAP Slap', 'Bear Flag Breakdown', 
                                    'Supply Zone Rejection', 'Liquidity Sweep Trap']:
                    strategy_metrics = [m for m in self.shorting_metrics_buffer 
                                      if m.strategy_name == strategy_name]
                    
                    if strategy_metrics:
                        latest = strategy_metrics[-1]
                        safe_name = strategy_name.replace(' ', '_')
                        
                        self.writer.add_scalar(f'Shorting/{safe_name}/Unrealized_PnL', 
                                             latest.unrealized_pnl, current_time)
                        self.writer.add_scalar(f'Shorting/{safe_name}/AI_Confidence', 
                                             latest.ai_confidence, current_time)
                        self.writer.add_scalar(f'Shorting/{safe_name}/Volume_Confirmation', 
                                             latest.volume_confirmation, current_time)
                        self.writer.add_scalar(f'Shorting/{safe_name}/Liquidity_Score', 
                                             latest.liquidity_score, current_time)
                        self.writer.add_scalar(f'Shorting/{safe_name}/Risk_Score', 
                                             latest.risk_score, current_time)
            
            # AI model performance
            if self.ai_model_buffer:
                for model_metrics in list(self.ai_model_buffer)[-4:]:
                    model_name = model_metrics.model_name
                    self.writer.add_scalar(f'AI_Models/{model_name}/Accuracy', 
                                         model_metrics.accuracy, current_time)
                    self.writer.add_scalar(f'AI_Models/{model_name}/F1_Score', 
                                         model_metrics.f1_score, current_time)
                    self.writer.add_scalar(f'AI_Models/{model_name}/Confidence', 
                                         model_metrics.prediction_confidence, current_time)
                    self.writer.add_scalar(f'AI_Models/{model_name}/Model_Drift', 
                                         model_metrics.model_drift_score, current_time)
                    
                    # Feature importance
                    for feature, importance in model_metrics.feature_importance.items():
                        self.writer.add_scalar(f'Features/{model_name}/{feature}', 
                                             importance, current_time)
            
            # Risk metrics
            if self.risk_metrics_buffer:
                latest_risk = self.risk_metrics_buffer[-1]
                self.writer.add_scalar('Risk/Portfolio_Heat', latest_risk.portfolio_heat, current_time)
                self.writer.add_scalar('Risk/Max_Drawdown', latest_risk.max_drawdown, current_time)
                self.writer.add_scalar('Risk/VaR_95', latest_risk.var_95, current_time)
                self.writer.add_scalar('Risk/VaR_99', latest_risk.var_99, current_time)
                self.writer.add_scalar('Risk/Beta', latest_risk.beta, current_time)
                self.writer.add_scalar('Risk/Kelly_Fraction', latest_risk.kelly_fraction, current_time)
                
                # Position sizing
                for strategy, size in latest_risk.position_sizing.items():
                    self.writer.add_scalar(f'Position_Sizing/{strategy}', size, current_time)
            
            # Market regime
            if self.market_regime_buffer:
                latest_regime = self.market_regime_buffer[-1]
                self.writer.add_scalar('Market/Trend_Strength', latest_regime['trend_strength'], current_time)
                self.writer.add_scalar('Market/Breadth', latest_regime['market_breadth'], current_time)
                self.writer.add_scalar('Market/Fear_Greed_Index', latest_regime['fear_greed_index'], current_time)
                self.writer.add_scalar('Market/VIX_Level', latest_regime['vix_level'], current_time)
            
            self.writer.flush()
            
        except Exception as e:
            logger.error(f"Error updating enhanced TensorBoard: {e}")
    
    def create_advanced_plots(self):
        """Create advanced matplotlib plots for TensorBoard"""
        try:
            # Shorting strategy performance heatmap
            if len(self.shorting_metrics_buffer) > 20:
                strategy_names = ['Fake Breakout Reversal', 'VWAP Slap', 'Bear Flag Breakdown', 
                                'Supply Zone Rejection', 'Liquidity Sweep Trap']
                
                metrics_matrix = []
                for strategy in strategy_names:
                    strategy_metrics = [m for m in list(self.shorting_metrics_buffer)[-20:] 
                                      if m.strategy_name == strategy]
                    if strategy_metrics:
                        avg_pnl = np.mean([m.unrealized_pnl for m in strategy_metrics])
                        avg_confidence = np.mean([m.ai_confidence for m in strategy_metrics])
                        avg_risk = np.mean([m.risk_score for m in strategy_metrics])
                        metrics_matrix.append([avg_pnl, avg_confidence * 100, avg_risk])
                
                if metrics_matrix:
                    fig, ax = plt.subplots(figsize=(12, 8))
                    
                    # Create heatmap
                    im = ax.imshow(metrics_matrix, cmap='RdYlGn', aspect='auto')
                    
                    # Labels
                    ax.set_xticks(range(3))
                    ax.set_xticklabels(['Avg P&L', 'AI Confidence %', 'Risk Score'])
                    ax.set_yticks(range(len(strategy_names)))
                    ax.set_yticklabels([s.replace(' ', '\n') for s in strategy_names])
                    
                    # Add text annotations
                    for i in range(len(strategy_names)):
                        for j in range(3):
                            if i < len(metrics_matrix):
                                text = ax.text(j, i, f'{metrics_matrix[i][j]:.1f}',
                                             ha="center", va="center", color="black", fontweight='bold')
                    
                    ax.set_title('Shorting Strategy Performance Heatmap')
                    plt.colorbar(im)
                    plt.tight_layout()
                    
                    self.writer.add_figure('Advanced_Plots/Shorting_Strategy_Heatmap', 
                                         fig, int(time.time()))
                    plt.close(fig)
            
            # AI model comparison radar chart
            if len(self.ai_model_buffer) > 4:
                models = list(set([m.model_name for m in list(self.ai_model_buffer)[-8:]]))
                
                if len(models) >= 2:
                    fig, ax = plt.subplots(figsize=(10, 10), subplot_kw=dict(projection='polar'))
                    
                    metrics_names = ['Accuracy', 'Precision', 'Recall', 'F1 Score', 'Confidence']
                    angles = np.linspace(0, 2 * np.pi, len(metrics_names), endpoint=False).tolist()
                    angles += angles[:1]  # Complete the circle
                    
                    for model in models[:3]:  # Show top 3 models
                        model_metrics = [m for m in list(self.ai_model_buffer)[-8:] 
                                       if m.model_name == model]
                        if model_metrics:
                            latest = model_metrics[-1]
                            values = [
                                latest.accuracy,
                                latest.precision, 
                                latest.recall,
                                latest.f1_score,
                                latest.prediction_confidence
                            ]
                            values += values[:1]  # Complete the circle
                            
                            ax.plot(angles, values, 'o-', linewidth=2, label=model.replace('_', ' '))
                            ax.fill(angles, values, alpha=0.25)
                    
                    ax.set_xticks(angles[:-1])
                    ax.set_xticklabels(metrics_names)
                    ax.set_ylim(0, 1)
                    ax.set_title('AI Model Performance Comparison')
                    ax.legend(loc='upper right', bbox_to_anchor=(1.3, 1.1))
                    
                    plt.tight_layout()
                    self.writer.add_figure('Advanced_Plots/AI_Model_Radar', fig, int(time.time()))
                    plt.close(fig)
            
        except Exception as e:
            logger.error(f"Error creating advanced plots: {e}")
    
    def save_enhanced_metrics(self):
        """Save enhanced metrics to database"""
        try:
            with sqlite3.connect(self.db_path) as conn:
                cursor = conn.cursor()
                
                # Save shorting metrics
                for metrics in list(self.shorting_metrics_buffer)[-5:]:
                    cursor.execute('''
                        INSERT OR REPLACE INTO shorting_metrics VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ''', (
                        metrics.timestamp.isoformat(),
                        metrics.strategy_name,
                        metrics.signal_type,
                        metrics.entry_price,
                        metrics.current_price,
                        metrics.unrealized_pnl,
                        metrics.volume_confirmation,
                        metrics.ai_confidence,
                        metrics.liquidity_score,
                        metrics.risk_score
                    ))
                
                # Save AI model performance
                for metrics in list(self.ai_model_buffer)[-4:]:
                    cursor.execute('''
                        INSERT OR REPLACE INTO ai_model_performance VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ''', (
                        metrics.timestamp.isoformat(),
                        metrics.model_name,
                        metrics.strategy_type,
                        metrics.accuracy,
                        metrics.precision,
                        metrics.recall,
                        metrics.f1_score,
                        metrics.prediction_confidence,
                        json.dumps(metrics.feature_importance),
                        json.dumps(metrics.confusion_matrix),
                        json.dumps(metrics.confidence_intervals),
                        metrics.model_drift_score
                    ))
                
                # Save risk metrics
                if self.risk_metrics_buffer:
                    latest_risk = self.risk_metrics_buffer[-1]
                    cursor.execute('''
                        INSERT OR REPLACE INTO risk_metrics VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ''', (
                        latest_risk.timestamp.isoformat(),
                        latest_risk.portfolio_heat,
                        latest_risk.max_drawdown,
                        latest_risk.var_95,
                        latest_risk.var_99,
                        latest_risk.beta,
                        json.dumps(latest_risk.correlation_matrix),
                        json.dumps(latest_risk.position_sizing),
                        latest_risk.kelly_fraction
                    ))
                
                conn.commit()
                
        except Exception as e:
            logger.error(f"Error saving enhanced metrics: {e}")

def main():
    """Main function to run Enhanced TensorBoard monitoring"""
    monitor = EnhancedTensorBoardMonitor(
        log_dir="tensorboard_logs/enhanced_lynxtrader",
        update_interval=15  # Update every 15 seconds for better granularity
    )
    
    try:
        monitor.start_monitoring()
        
        print("🚀 Enhanced TensorBoard Monitor is running...")
        print("📊 View dashboard at: http://localhost:6006")
        print("🔧 Start TensorBoard with: tensorboard --logdir=tensorboard_logs")
        print("🔻 Monitoring shorting strategies and AI models...")
        print("⚡ Press Ctrl+C to stop...")
        
        while True:
            time.sleep(1)
            
    except KeyboardInterrupt:
        print("\n🛑 Stopping Enhanced TensorBoard Monitor...")
        monitor.stop_monitoring()
        print("✅ Enhanced Monitor stopped.")

if __name__ == "__main__":
    main() 