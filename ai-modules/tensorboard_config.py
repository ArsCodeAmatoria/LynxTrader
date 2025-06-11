"""
TensorBoard Configuration for LynxTrader
Custom dashboard layouts and visualization settings
"""

import json
from typing import Dict, List, Any

class TensorBoardConfig:
    """Configuration management for TensorBoard dashboards"""
    
    def __init__(self):
        self.dashboard_configs = self._create_dashboard_configs()
    
    def _create_dashboard_configs(self) -> Dict[str, Any]:
        """Create dashboard configuration layouts"""
        
        return {
            "shorting_strategies": {
                "title": "🔻 Shorting Strategies Dashboard",
                "layout": {
                    "scalars": [
                        "Shorting/*/Unrealized_PnL",
                        "Shorting/*/AI_Confidence", 
                        "Shorting/*/Volume_Confirmation",
                        "Shorting/*/Liquidity_Score",
                        "Shorting/*/Risk_Score"
                    ],
                    "images": [
                        "Advanced_Plots/Shorting_Strategy_Heatmap"
                    ],
                    "histograms": [
                        "Risk/Portfolio_Heat",
                        "Risk/VaR_95"
                    ]
                },
                "refresh_interval": 15,
                "description": "Real-time monitoring of shorting strategy performance"
            },
            
            "ai_models": {
                "title": "🧠 AI Models Performance", 
                "layout": {
                    "scalars": [
                        "AI_Models/*/Accuracy",
                        "AI_Models/*/F1_Score",
                        "AI_Models/*/Confidence",
                        "AI_Models/*/Model_Drift"
                    ],
                    "images": [
                        "Advanced_Plots/AI_Model_Radar"
                    ],
                    "histograms": [
                        "Features/*/volume_profile",
                        "Features/*/price_action",
                        "Features/*/market_structure"
                    ]
                },
                "refresh_interval": 30,
                "description": "AI model accuracy, drift detection, and feature importance"
            },
            
            "risk_management": {
                "title": "⚠️ Risk Management Dashboard",
                "layout": {
                    "scalars": [
                        "Risk/Portfolio_Heat",
                        "Risk/Max_Drawdown", 
                        "Risk/VaR_95",
                        "Risk/VaR_99",
                        "Risk/Beta",
                        "Risk/Kelly_Fraction"
                    ],
                    "histograms": [
                        "Position_Sizing/*"
                    ]
                },
                "refresh_interval": 20,
                "description": "Portfolio risk metrics and position sizing optimization"
            },
            
            "market_overview": {
                "title": "📈 Market Overview",
                "layout": {
                    "scalars": [
                        "Market/Trend_Strength",
                        "Market/Breadth",
                        "Market/Fear_Greed_Index", 
                        "Market/VIX_Level"
                    ]
                },
                "refresh_interval": 60,
                "description": "Market regime and volatility monitoring"
            }
        }
    
    def get_dashboard_config(self, dashboard_name: str) -> Dict[str, Any]:
        """Get configuration for a specific dashboard"""
        return self.dashboard_configs.get(dashboard_name, {})
    
    def get_all_dashboards(self) -> List[str]:
        """Get list of available dashboard names"""
        return list(self.dashboard_configs.keys())
    
    def export_config(self, filepath: str = "tensorboard_config.json"):
        """Export configuration to JSON file"""
        with open(filepath, 'w') as f:
            json.dump(self.dashboard_configs, f, indent=2)
    
    @classmethod
    def load_config(cls, filepath: str = "tensorboard_config.json"):
        """Load configuration from JSON file"""
        try:
            with open(filepath, 'r') as f:
                config_data = json.load(f)
            
            instance = cls()
            instance.dashboard_configs = config_data
            return instance
        except FileNotFoundError:
            return cls()  # Return default config if file not found

# TensorBoard custom CSS styling
CUSTOM_CSS = """
<style>
/* LynxTrader Custom TensorBoard Styling */
.tf-tensorboard {
    background: linear-gradient(135deg, #0f0f1a 0%, #1a1a2e 100%);
    color: #39ff14;
    font-family: 'Fira Code', 'Courier New', monospace;
}

.scalar-card {
    background: rgba(25, 25, 46, 0.8);
    border: 1px solid #39ff14;
    border-radius: 8px;
    box-shadow: 0 0 20px rgba(57, 255, 20, 0.3);
}

.scalar-card .title {
    color: #ff007f;
    font-weight: bold;
    text-transform: uppercase;
}

.line-chart {
    filter: drop-shadow(0 0 5px #39ff14);
}

/* Shorting strategies specific styling */
.shorting-metric {
    border-left: 3px solid #ff4444;
}

.ai-metric {
    border-left: 3px solid #00ffff;
}

.risk-metric {
    border-left: 3px solid #ffaa00;
}

/* Matrix-style text effect */
@keyframes matrix-glow {
    0%, 100% { text-shadow: 0 0 5px #39ff14; }
    50% { text-shadow: 0 0 20px #39ff14, 0 0 30px #39ff14; }
}

.matrix-text {
    animation: matrix-glow 2s ease-in-out infinite;
}
</style>
"""

# Custom JavaScript for enhanced interactivity
CUSTOM_JS = """
<script>
// LynxTrader TensorBoard Enhancements

// Auto-refresh function
function autoRefresh() {
    const refreshInterval = 15000; // 15 seconds
    setInterval(() => {
        location.reload();
    }, refreshInterval);
}

// Add custom alerts for threshold breaches
function monitorThresholds() {
    // Monitor risk metrics
    const riskElements = document.querySelectorAll('[data-metric*="Risk/"]');
    riskElements.forEach(element => {
        const value = parseFloat(element.textContent);
        if (element.getAttribute('data-metric').includes('VaR_99') && value > 8.0) {
            alert('⚠️ HIGH RISK: VaR 99% exceeded threshold!');
        }
    });
    
    // Monitor AI model drift
    const driftElements = document.querySelectorAll('[data-metric*="Model_Drift"]');
    driftElements.forEach(element => {
        const value = parseFloat(element.textContent);
        if (value > 0.15) {
            alert('🤖 AI ALERT: Model drift detected!');
        }
    });
}

// Initialize enhancements
document.addEventListener('DOMContentLoaded', function() {
    autoRefresh();
    setInterval(monitorThresholds, 30000); // Check every 30 seconds
    
    // Add matrix rain effect
    addMatrixRain();
});

function addMatrixRain() {
    const matrixContainer = document.createElement('div');
    matrixContainer.style.position = 'fixed';
    matrixContainer.style.top = '0';
    matrixContainer.style.left = '0';
    matrixContainer.style.width = '100%';
    matrixContainer.style.height = '100%';
    matrixContainer.style.pointerEvents = 'none';
    matrixContainer.style.zIndex = '-1';
    matrixContainer.style.opacity = '0.1';
    
    document.body.appendChild(matrixContainer);
    
    // Matrix rain implementation would go here
    // (simplified for brevity)
}
</script>
"""

def create_custom_layout_config():
    """Create custom layout configuration for TensorBoard"""
    
    layout_config = {
        "version": 1,
        "layouts": [
            {
                "category": "scalars",
                "title": "🔻 Shorting Strategies Overview",
                "charts": [
                    {
                        "title": "Unrealized P&L by Strategy",
                        "multiChart": True,
                        "tag": "Shorting/.*/Unrealized_PnL"
                    },
                    {
                        "title": "AI Confidence Levels", 
                        "multiChart": True,
                        "tag": "Shorting/.*/AI_Confidence"
                    }
                ]
            },
            {
                "category": "scalars",
                "title": "🧠 AI Model Performance",
                "charts": [
                    {
                        "title": "Model Accuracy Comparison",
                        "multiChart": True,
                        "tag": "AI_Models/.*/Accuracy"
                    },
                    {
                        "title": "Model Drift Detection",
                        "multiChart": True,
                        "tag": "AI_Models/.*/Model_Drift"
                    }
                ]
            },
            {
                "category": "scalars", 
                "title": "⚠️ Risk Monitoring",
                "charts": [
                    {
                        "title": "Portfolio Risk Metrics",
                        "multiChart": True,
                        "tag": "Risk/(Portfolio_Heat|Max_Drawdown|VaR_.*)"
                    },
                    {
                        "title": "Position Sizing",
                        "multiChart": True,
                        "tag": "Position_Sizing/.*"
                    }
                ]
            }
        ]
    }
    
    return layout_config

if __name__ == "__main__":
    # Create and export default configuration
    config = TensorBoardConfig()
    config.export_config()
    
    # Create layout configuration
    layout_config = create_custom_layout_config()
    with open("tensorboard_layout.json", 'w') as f:
        json.dump(layout_config, f, indent=2)
    
    print("✅ TensorBoard configuration files created:")
    print("📄 tensorboard_config.json - Dashboard configurations")
    print("🎨 tensorboard_layout.json - Custom layout settings") 