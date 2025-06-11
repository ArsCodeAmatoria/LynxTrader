#!/usr/bin/env python3
"""
Generate clear demo data for TensorBoard visualization
"""

import numpy as np
import time
from datetime import datetime
from torch.utils.tensorboard import SummaryWriter
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

def generate_demo_data():
    """Generate comprehensive demo data for TensorBoard"""
    
    print("🚀 Generating TensorBoard demo data...")
    
    # Create writer with simple path
    log_dir = "tensorboard_logs/live_demo"
    writer = SummaryWriter(log_dir=log_dir)
    
    print(f"📊 Writing to: {log_dir}")
    
    # Generate 50 data points over time
    for step in range(50):
        
        # Shorting Strategies Metrics
        strategies = ['Fake_Breakout_Reversal', 'VWAP_Slap', 'Bear_Flag_Breakdown', 
                     'Supply_Zone_Rejection', 'Liquidity_Sweep_Trap']
        
        for strategy in strategies:
            # Generate realistic trading metrics
            base_pnl = np.random.normal(0, 100)  # Random P&L around 0
            pnl_trend = step * np.random.uniform(-2, 3)  # Overall trend
            unrealized_pnl = base_pnl + pnl_trend
            
            ai_confidence = np.random.uniform(0.7, 0.95)
            volume_conf = np.random.uniform(0.6, 1.4)
            liquidity_score = np.random.uniform(0.5, 1.0)
            risk_score = np.random.uniform(2.0, 4.5)
            
            # Log to TensorBoard
            writer.add_scalar(f'Shorting/{strategy}/Unrealized_PnL', unrealized_pnl, step)
            writer.add_scalar(f'Shorting/{strategy}/AI_Confidence', ai_confidence, step)
            writer.add_scalar(f'Shorting/{strategy}/Volume_Confirmation', volume_conf, step)
            writer.add_scalar(f'Shorting/{strategy}/Liquidity_Score', liquidity_score, step)
            writer.add_scalar(f'Shorting/{strategy}/Risk_Score', risk_score, step)
        
        # AI Models Performance
        ai_models = ['Liquidity_Map_Generator', 'Volume_Divergence_Detector', 
                    'Exhaustion_Pattern_Scanner', 'Smart_Money_Flow_Tracker']
        
        for model in ai_models:
            accuracy = np.random.uniform(0.75, 0.92)
            f1_score = np.random.uniform(0.72, 0.88)
            confidence = np.random.uniform(0.80, 0.95)
            model_drift = np.random.uniform(0.02, 0.15)
            
            writer.add_scalar(f'AI_Models/{model}/Accuracy', accuracy, step)
            writer.add_scalar(f'AI_Models/{model}/F1_Score', f1_score, step)
            writer.add_scalar(f'AI_Models/{model}/Confidence', confidence, step)
            writer.add_scalar(f'AI_Models/{model}/Model_Drift', model_drift, step)
        
        # Risk Management Metrics
        portfolio_heat = np.random.uniform(45, 75)
        max_drawdown = np.random.uniform(2, 8)
        var_95 = np.random.uniform(1.5, 4.5)
        var_99 = np.random.uniform(3.0, 8.0)
        kelly_fraction = np.random.uniform(0.12, 0.18)
        
        writer.add_scalar('Risk/Portfolio_Heat', portfolio_heat, step)
        writer.add_scalar('Risk/Max_Drawdown', max_drawdown, step)
        writer.add_scalar('Risk/VaR_95', var_95, step)
        writer.add_scalar('Risk/VaR_99', var_99, step)
        writer.add_scalar('Risk/Kelly_Fraction', kelly_fraction, step)
        
        # Market Overview
        trend_strength = np.random.uniform(0.3, 0.9)
        market_breadth = np.random.uniform(0.4, 0.8)
        fear_greed = np.random.uniform(20, 80)
        vix_level = np.random.uniform(15, 35)
        
        writer.add_scalar('Market/Trend_Strength', trend_strength, step)
        writer.add_scalar('Market/Breadth', market_breadth, step)
        writer.add_scalar('Market/Fear_Greed_Index', fear_greed, step)
        writer.add_scalar('Market/VIX_Level', vix_level, step)
        
        # Position Sizing
        for pos_type in ['scalping', 'day_trading', 'swing']:
            size = np.random.uniform(0.1, 0.5)
            writer.add_scalar(f'Position_Sizing/{pos_type}', size, step)
        
        # Feature Importance for AI models
        features = ['volume_profile', 'price_action', 'market_structure', 'order_flow']
        for model in ai_models:
            for feature in features:
                importance = np.random.uniform(0.1, 0.4)
                writer.add_scalar(f'Features/{model}/{feature}', importance, step)
        
        # Progress indicator
        if step % 10 == 0:
            print(f"📈 Generated {step}/50 data points...")
        
        # Flush every few steps
        if step % 5 == 0:
            writer.flush()
        
        time.sleep(0.1)  # Small delay for realistic timestamps
    
    # Final flush
    writer.flush()
    writer.close()
    
    print("✅ Demo data generation complete!")
    print(f"📊 Data written to: {log_dir}")
    print("🌐 TensorBoard should now show comprehensive dashboards")
    
    return log_dir

if __name__ == "__main__":
    log_dir = generate_demo_data()
    print(f"\n💡 To view the data:")
    print(f"1. Stop current TensorBoard: pkill -f tensorboard")
    print(f"2. Start new TensorBoard: python3 -m tensorboard.main --logdir={log_dir} --port=6006")
    print(f"3. Open: http://localhost:6006") 