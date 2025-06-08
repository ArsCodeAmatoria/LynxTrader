"""
LynxTrader TensorBoard Monitor
Real-time visualization of trading performance and AI model metrics
"""

import time
import numpy as np
from datetime import datetime
from torch.utils.tensorboard import SummaryWriter
import threading
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class TensorBoardMonitor:
    def __init__(self, log_dir: str = "tensorboard_logs"):
        self.writer = SummaryWriter(log_dir=f"{log_dir}/lynxtrader_{datetime.now().strftime('%Y%m%d_%H%M%S')}")
        self.is_running = False
        
    def start_monitoring(self):
        self.is_running = True
        thread = threading.Thread(target=self._monitor_loop, daemon=True)
        thread.start()
        logger.info("TensorBoard monitoring started")
        
    def _monitor_loop(self):
        step = 0
        while self.is_running:
            # Simulate trading metrics
            self.writer.add_scalar('Trading/PnL', np.random.normal(100, 20), step)
            self.writer.add_scalar('Trading/Win_Rate', np.random.uniform(60, 80), step)
            self.writer.add_scalar('Models/LSTM/Accuracy', np.random.uniform(0.7, 0.9), step)
            
            step += 1
            time.sleep(30)
            
    def stop(self):
        self.is_running = False
        self.writer.close()

if __name__ == "__main__":
    monitor = TensorBoardMonitor()
    monitor.start_monitoring()
    
    try:
        while True:
            time.sleep(1)
    except KeyboardInterrupt:
        monitor.stop() 