#!/usr/bin/env python3
"""
LynxTrader TensorBoard Demo
Quick demonstration of the enhanced monitoring system
"""

import time
import sys
import os

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from enhanced_tensorboard import EnhancedTensorBoardMonitor

def run_demo():
    """Run a quick demo of the TensorBoard monitoring system"""
    
    print("🦁 LynxTrader TensorBoard Demo")
    print("=" * 50)
    
    # Create monitor instance
    monitor = EnhancedTensorBoardMonitor(
        log_dir="tensorboard_logs/demo",
        update_interval=5  # 5 second updates for demo
    )
    
    print("🚀 Starting enhanced monitoring...")
    monitor.start_monitoring()
    
    print("📊 Generating demo data for 30 seconds...")
    print("📈 Watch metrics at: tensorboard --logdir=tensorboard_logs/demo")
    print("🌐 Then open: http://localhost:6006")
    
    try:
        # Run for 30 seconds to generate sample data
        for i in range(6):  # 6 iterations * 5 seconds = 30 seconds
            print(f"⏱️  Demo running... {(i+1)*5}s / 30s")
            time.sleep(5)
        
        print("✅ Demo data generated successfully!")
        print("\n📊 Generated visualizations:")
        print("   • 🔻 Shorting strategy metrics")
        print("   • 🧠 AI model performance") 
        print("   • ⚠️ Risk management data")
        print("   • 📈 Market overview")
        print("   • 🔥 Strategy heatmaps")
        print("   • 🕸️ AI model radar charts")
        
    except KeyboardInterrupt:
        print("\n🛑 Demo interrupted by user")
    
    finally:
        print("\n🛑 Stopping monitor...")
        monitor.stop_monitoring()
        
        print("\n💡 To view the data:")
        print("1. Run: tensorboard --logdir=tensorboard_logs/demo")
        print("2. Open: http://localhost:6006")
        print("3. Navigate through the dashboard tabs")
        
        print("\n🚀 To start full system:")
        print("python3 start_tensorboard.py")

if __name__ == "__main__":
    run_demo() 