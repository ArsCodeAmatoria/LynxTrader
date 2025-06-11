#!/usr/bin/env python3
"""
LynxTrader TensorBoard Startup Script
Launches enhanced monitoring and TensorBoard web interface
"""

import os
import sys
import subprocess
import threading
import time
import signal
import argparse
from pathlib import Path

# Add the current directory to Python path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from enhanced_tensorboard import EnhancedTensorBoardMonitor
from tensorboard_config import TensorBoardConfig, create_custom_layout_config
import json

class TensorBoardLauncher:
    """Manages TensorBoard monitoring and web interface"""
    
    def __init__(self, port: int = 6006, log_dir: str = "tensorboard_logs", 
                 monitoring_interval: int = 15):
        self.port = port
        self.log_dir = log_dir
        self.monitoring_interval = monitoring_interval
        self.monitor = None
        self.tensorboard_process = None
        self.running = False
        
        # Ensure log directory exists
        os.makedirs(log_dir, exist_ok=True)
        
    def setup_configuration(self):
        """Setup TensorBoard configuration files"""
        try:
            # Create configuration files
            config = TensorBoardConfig()
            config.export_config("tensorboard_config.json")
            
            layout_config = create_custom_layout_config()
            with open("tensorboard_layout.json", 'w') as f:
                json.dump(layout_config, f, indent=2)
                
            print("✅ TensorBoard configuration files created")
            
        except Exception as e:
            print(f"⚠️ Warning: Could not create config files: {e}")
    
    def start_monitoring(self):
        """Start the enhanced monitoring system"""
        try:
            self.monitor = EnhancedTensorBoardMonitor(
                log_dir=self.log_dir,
                update_interval=self.monitoring_interval
            )
            self.monitor.start_monitoring()
            print("🚀 Enhanced monitoring started")
            
        except Exception as e:
            print(f"❌ Error starting monitoring: {e}")
            return False
        
        return True
    
    def start_tensorboard_server(self):
        """Start TensorBoard web server"""
        try:
            cmd = [
                "tensorboard",
                "--logdir", self.log_dir,
                "--port", str(self.port),
                "--reload_interval", "15",
                "--window_title", "LynxTrader AI Dashboard"
            ]
            
            # Add layout config if it exists
            if os.path.exists("tensorboard_layout.json"):
                cmd.extend(["--layout", "tensorboard_layout.json"])
            
            self.tensorboard_process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True
            )
            
            print(f"📊 TensorBoard server started on port {self.port}")
            print(f"🌐 Open: http://localhost:{self.port}")
            
        except Exception as e:
            print(f"❌ Error starting TensorBoard server: {e}")
            return False
        
        return True
    
    def check_dependencies(self):
        """Check if required dependencies are installed"""
        required_packages = ['tensorboard', 'torch', 'matplotlib', 'plotly', 'seaborn']
        missing_packages = []
        
        for package in required_packages:
            try:
                __import__(package)
            except ImportError:
                missing_packages.append(package)
        
        if missing_packages:
            print("❌ Missing required packages:")
            for pkg in missing_packages:
                print(f"   - {pkg}")
            print("\n💡 Install with: pip install " + " ".join(missing_packages))
            return False
        
        # Check if tensorboard command is available
        try:
            subprocess.run(['tensorboard', '--version'], 
                         capture_output=True, check=True)
        except (subprocess.CalledProcessError, FileNotFoundError):
            print("❌ TensorBoard command not found")
            print("💡 Install with: pip install tensorboard")
            return False
        
        return True
    
    def wait_for_tensorboard(self, timeout: int = 30):
        """Wait for TensorBoard to be ready"""
        import urllib.request
        import urllib.error
        
        url = f"http://localhost:{self.port}"
        start_time = time.time()
        
        while time.time() - start_time < timeout:
            try:
                urllib.request.urlopen(url, timeout=1)
                return True
            except (urllib.error.URLError, OSError):
                time.sleep(1)
        
        return False
    
    def start(self):
        """Start the complete TensorBoard system"""
        print("🦁 LynxTrader TensorBoard System")
        print("=" * 50)
        
        # Check dependencies
        if not self.check_dependencies():
            return False
        
        # Setup configuration
        self.setup_configuration()
        
        # Start monitoring
        if not self.start_monitoring():
            return False
        
        # Start TensorBoard server
        if not self.start_tensorboard_server():
            self.stop()
            return False
        
        # Wait for TensorBoard to be ready
        print("⏳ Waiting for TensorBoard to start...")
        if self.wait_for_tensorboard():
            print("✅ TensorBoard is ready!")
        else:
            print("⚠️ TensorBoard may still be starting...")
        
        self.running = True
        return True
    
    def stop(self):
        """Stop the TensorBoard system"""
        print("\n🛑 Stopping TensorBoard system...")
        
        # Stop monitoring
        if self.monitor:
            self.monitor.stop_monitoring()
            print("✅ Monitoring stopped")
        
        # Stop TensorBoard server
        if self.tensorboard_process:
            self.tensorboard_process.terminate()
            try:
                self.tensorboard_process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.tensorboard_process.kill()
            print("✅ TensorBoard server stopped")
        
        self.running = False
        print("✅ TensorBoard system stopped")
    
    def status(self):
        """Check status of TensorBoard system"""
        print("📊 TensorBoard System Status")
        print("-" * 30)
        
        # Check monitoring
        monitor_status = "🟢 Running" if (self.monitor and self.monitor.is_running) else "🔴 Stopped"
        print(f"Monitoring: {monitor_status}")
        
        # Check TensorBoard server
        server_status = "🟢 Running" if (self.tensorboard_process and 
                                       self.tensorboard_process.poll() is None) else "🔴 Stopped"
        print(f"TensorBoard Server: {server_status}")
        
        # Check port availability
        try:
            import urllib.request
            urllib.request.urlopen(f"http://localhost:{self.port}", timeout=1)
            web_status = f"🟢 Available at http://localhost:{self.port}"
        except:
            web_status = "🔴 Not accessible"
        
        print(f"Web Interface: {web_status}")
        
        # Log directory info
        if os.path.exists(self.log_dir):
            log_files = list(Path(self.log_dir).rglob("*"))
            print(f"Log Files: 📁 {len(log_files)} files in {self.log_dir}")
        else:
            print(f"Log Directory: ❌ {self.log_dir} not found")

def signal_handler(signum, frame):
    """Handle Ctrl+C gracefully"""
    print("\n🛑 Received interrupt signal...")
    if 'launcher' in globals():
        launcher.stop()
    sys.exit(0)

def main():
    """Main function with command line interface"""
    parser = argparse.ArgumentParser(description="LynxTrader TensorBoard System")
    parser.add_argument("--port", type=int, default=6006, 
                       help="TensorBoard server port (default: 6006)")
    parser.add_argument("--logdir", type=str, default="tensorboard_logs",
                       help="TensorBoard log directory (default: tensorboard_logs)")
    parser.add_argument("--interval", type=int, default=15,
                       help="Monitoring update interval in seconds (default: 15)")
    parser.add_argument("--status", action="store_true",
                       help="Check system status and exit")
    parser.add_argument("--stop", action="store_true",
                       help="Stop running TensorBoard system")
    
    args = parser.parse_args()
    
    global launcher
    launcher = TensorBoardLauncher(
        port=args.port,
        log_dir=args.logdir,
        monitoring_interval=args.interval
    )
    
    # Handle command line options
    if args.status:
        launcher.status()
        return
    
    if args.stop:
        # Try to stop any running instances
        try:
            subprocess.run(['pkill', '-f', 'tensorboard'], check=False)
            print("✅ Stopped TensorBoard processes")
        except:
            print("⚠️ Could not stop TensorBoard processes")
        return
    
    # Setup signal handler for graceful shutdown
    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)
    
    # Start the system
    if launcher.start():
        print("\n" + "=" * 60)
        print("🚀 LynxTrader TensorBoard System is RUNNING")
        print("=" * 60)
        print(f"📊 Dashboard: http://localhost:{args.port}")
        print("🔻 Monitoring: Shorting strategies + AI models")
        print("⚡ Interval: " + str(args.interval) + " seconds")
        print("📁 Logs: " + args.logdir)
        print("\n💡 Available dashboards:")
        print("   • Shorting Strategies (real-time P&L)")
        print("   • AI Models (accuracy + drift)")
        print("   • Risk Management (VaR + drawdown)")
        print("   • Market Overview (regime detection)")
        print("\n🛑 Press Ctrl+C to stop")
        print("=" * 60)
        
        try:
            # Keep running until interrupted
            while launcher.running:
                time.sleep(1)
        except KeyboardInterrupt:
            pass
        finally:
            launcher.stop()
    else:
        print("❌ Failed to start TensorBoard system")
        sys.exit(1)

if __name__ == "__main__":
    main() 