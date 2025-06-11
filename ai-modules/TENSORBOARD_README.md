# 📊 LynxTrader TensorBoard Monitoring System

Advanced AI-powered visualization and monitoring system for trading strategies, with specialized focus on shorting strategies and real-time performance tracking.

## 🚀 Features

### 🔻 **Shorting Strategies Dashboard**
- **Real-time P&L tracking** for 5 shorting strategies
- **AI confidence scoring** with volume confirmation
- **Liquidity analysis** and risk scoring
- **Strategy performance heatmaps**

### 🧠 **AI Models Performance**
- **Model accuracy tracking** with drift detection
- **Feature importance visualization**
- **Confusion matrix monitoring**
- **Confidence interval analysis**

### ⚠️ **Risk Management**
- **Portfolio heat monitoring** (VaR 95/99)
- **Dynamic position sizing** with Kelly Criterion
- **Correlation matrix tracking**
- **Real-time drawdown alerts**

### 📈 **Market Overview**
- **Market regime detection** (trend strength)
- **Volatility monitoring** (VIX tracking)
- **Fear & Greed Index** integration
- **Market breadth analysis**

## 🏗️ Architecture

```
📦 TensorBoard System
├── 🔧 enhanced_tensorboard.py      # Core monitoring engine
├── ⚙️ tensorboard_config.py        # Dashboard configurations
├── 🚀 start_tensorboard.py         # Startup launcher script
├── 📊 tensorboard_monitor.py       # Original monitor (legacy)
├── 🎯 tb_monitor.py                # Simplified monitor
└── 📋 requirements.txt             # Dependencies
```

## 🚀 Quick Start

### 1. **Install Dependencies**
```bash
cd ai-modules
pip install -r requirements.txt
```

### 2. **Launch TensorBoard System**
```bash
# Start with default settings
python start_tensorboard.py

# Custom port and interval
python start_tensorboard.py --port 8080 --interval 10

# Check system status
python start_tensorboard.py --status

# Stop running instances
python start_tensorboard.py --stop
```

### 3. **Access Dashboard**
Open your browser to: **http://localhost:6006**

## 📊 Dashboard Layouts

### 🔻 **Shorting Strategies View**
```
📈 Unrealized P&L Timeline
📊 AI Confidence Levels
🎯 Volume Confirmation Metrics
💰 Risk Score Distribution
🔥 Strategy Performance Heatmap
```

### 🧠 **AI Models View**
```
📈 Model Accuracy Comparison  
📊 Feature Importance Rankings
🎯 Model Drift Detection
💡 Prediction Confidence
🕸️ Performance Radar Chart
```

### ⚠️ **Risk Management View**
```
📈 Portfolio Heat Timeline
📊 VaR 95/99 Monitoring
🎯 Position Sizing Distribution
💰 Kelly Fraction Optimization
🔥 Correlation Matrix
```

## 🎛️ Configuration

### **Custom Dashboard Setup**
```python
from tensorboard_config import TensorBoardConfig

# Load configuration
config = TensorBoardConfig()

# Get shorting strategies config
shorting_config = config.get_dashboard_config('shorting_strategies')

# Customize refresh interval
shorting_config['refresh_interval'] = 10  # 10 seconds
```

### **Layout Customization**
```json
{
  "shorting_strategies": {
    "title": "🔻 Custom Shorting Dashboard",
    "layout": {
      "scalars": ["Shorting/*/Unrealized_PnL"],
      "images": ["Advanced_Plots/Shorting_Strategy_Heatmap"]
    },
    "refresh_interval": 15
  }
}
```

## 📈 Monitored Metrics

### **Shorting Strategies**
| Metric | Description | Update Frequency |
|--------|-------------|------------------|
| `Unrealized_PnL` | Current position P&L | 15s |
| `AI_Confidence` | ML model confidence | 15s |
| `Volume_Confirmation` | Volume vs average | 15s |
| `Liquidity_Score` | Market liquidity rating | 15s |
| `Risk_Score` | Position risk (1-5 scale) | 15s |

### **AI Models**
| Metric | Description | Update Frequency |
|--------|-------------|------------------|
| `Accuracy` | Model prediction accuracy | 30s |
| `F1_Score` | Harmonic mean of precision/recall | 30s |
| `Model_Drift` | Performance degradation | 30s |
| `Confidence` | Average prediction confidence | 30s |

### **Risk Management**
| Metric | Description | Update Frequency |
|--------|-------------|------------------|
| `Portfolio_Heat` | % of capital at risk | 20s |
| `VaR_95/99` | Value at Risk percentiles | 20s |
| `Max_Drawdown` | Maximum portfolio decline | 20s |
| `Kelly_Fraction` | Optimal position sizing | 20s |

## 🔧 Advanced Usage

### **Custom Monitoring Script**
```python
from enhanced_tensorboard import EnhancedTensorBoardMonitor

# Create custom monitor
monitor = EnhancedTensorBoardMonitor(
    log_dir="custom_logs",
    update_interval=10  # 10 second updates
)

# Start monitoring
monitor.start_monitoring()

# Add custom metrics
from datetime import datetime
custom_metrics = ShortingMetrics(
    timestamp=datetime.now(),
    strategy_name="Custom_Strategy",
    signal_type="custom_signal",
    entry_price=150.0,
    current_price=148.5,
    unrealized_pnl=-150.0,
    volume_confirmation=0.8,
    ai_confidence=0.85,
    liquidity_score=0.7,
    risk_score=3.2
)

monitor.shorting_metrics_buffer.append(custom_metrics)
```

### **Database Integration**
```python
# Access historical data
df = monitor.get_historical_data("shorting_metrics", hours=24)

# Query specific strategy
strategy_data = df[df['strategy_name'] == 'Bear Flag Breakdown']

# Calculate performance statistics
avg_pnl = strategy_data['unrealized_pnl'].mean()
win_rate = (strategy_data['unrealized_pnl'] > 0).mean() * 100
```

## 🎨 Visualization Features

### **Real-time Plots**
- **📈 P&L Timeline**: Continuous profit/loss tracking
- **🔥 Strategy Heatmap**: Performance comparison matrix
- **🕸️ AI Model Radar**: Multi-dimensional model comparison
- **📊 Risk Distribution**: Portfolio risk breakdown

### **Interactive Features**
- **🔄 Auto-refresh**: Configurable update intervals
- **⚠️ Threshold Alerts**: Automatic risk warnings
- **🎨 Matrix Styling**: Cyber-futuristic themes
- **📱 Responsive Design**: Works on all devices

## 🚨 Alerts & Thresholds

### **Automatic Alerts**
```javascript
// Risk threshold alerts
VaR_99 > 8.0%          → 🚨 HIGH RISK ALERT
Portfolio_Heat > 75%   → ⚠️ HEAT WARNING
Model_Drift > 15%      → 🤖 MODEL ALERT
Max_Drawdown > 10%     → 📉 DRAWDOWN WARNING
```

### **Custom Alert Setup**
```python
# Set custom thresholds
RISK_THRESHOLDS = {
    'var_99': 8.0,
    'portfolio_heat': 75.0,
    'model_drift': 0.15,
    'max_drawdown': 10.0
}

# Monitor and alert
def check_thresholds(metrics):
    for key, threshold in RISK_THRESHOLDS.items():
        if getattr(metrics, key) > threshold:
            send_alert(f"⚠️ {key.upper()} exceeded: {getattr(metrics, key)}")
```

## 🐛 Troubleshooting

### **Common Issues**

#### ❌ **TensorBoard won't start**
```bash
# Check if port is available
lsof -i :6006

# Kill existing processes
pkill -f tensorboard

# Restart with different port
python start_tensorboard.py --port 8080
```

#### ❌ **No data in dashboard**
```bash
# Check monitoring status
python start_tensorboard.py --status

# Verify log directory
ls -la tensorboard_logs/

# Check for errors
tail -f enhanced_monitoring.log
```

#### ❌ **Missing dependencies**
```bash
# Install missing packages
pip install torch tensorboard matplotlib plotly seaborn

# Verify TensorBoard installation
tensorboard --version
```

### **Performance Optimization**

#### **Reduce Update Frequency**
```python
# For better performance with large datasets
monitor = EnhancedTensorBoardMonitor(
    update_interval=30  # 30 seconds instead of 15
)
```

#### **Limit Buffer Size**
```python
# Reduce memory usage
monitor.shorting_metrics_buffer = deque(maxlen=500)  # Instead of 2000
```

## 📚 References

### **TensorBoard Documentation**
- [TensorBoard Guide](https://www.tensorflow.org/tensorboard)
- [Custom Scalars](https://www.tensorflow.org/tensorboard/scalars_and_keras)
- [Layout API](https://github.com/tensorflow/tensorboard/blob/master/docs/r2/layouts.md)

### **Trading Metrics**
- [VaR Calculation](https://en.wikipedia.org/wiki/Value_at_risk)
- [Kelly Criterion](https://en.wikipedia.org/wiki/Kelly_criterion)
- [Sharpe Ratio](https://en.wikipedia.org/wiki/Sharpe_ratio)

### **AI Model Monitoring**
- [Model Drift Detection](https://docs.seldon.io/projects/alibi-detect/en/stable/)
- [ML Monitoring Best Practices](https://ml-ops.org/content/monitoring)

## 🤝 Contributing

### **Adding New Metrics**
1. Define new dataclass in `enhanced_tensorboard.py`
2. Add collection method
3. Update TensorBoard logging
4. Add to dashboard configuration

### **Custom Visualizations**
1. Add plot function to `create_advanced_plots()`
2. Use matplotlib or plotly
3. Log with `writer.add_figure()`

## 📄 License

This TensorBoard monitoring system is part of the LynxTrader project and follows the same licensing terms.

## 🎯 Roadmap

- [ ] **Real-time alerts** via Slack/Discord
- [ ] **Mobile dashboard** optimization
- [ ] **Multi-timeframe** strategy analysis
- [ ] **Options flow** integration
- [ ] **Sentiment analysis** dashboards
- [ ] **Live trading** integration
- [ ] **Performance** attribution analysis
- [ ] **A/B testing** framework

---

**🦁 LynxTrader TensorBoard - Where AI meets Trading Intelligence** 🚀 