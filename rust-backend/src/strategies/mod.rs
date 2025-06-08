use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum StrategyType {
    Scalping,
    DayTrading,
    SmartMoney,
    SwingTrading,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TimeFrame {
    OneMin,
    FiveMin,
    TenMin,
    FifteenMin,
    ThirtyMin,
    OneHour,
    FourHour,
    OneDay,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskParams {
    pub stop_loss: f64,
    pub take_profit: f64,
    pub position_size: f64,
    pub max_daily_loss: f64,
    pub max_drawdown: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AIRiskConfig {
    pub adaptive_position_sizing: bool,
    pub trailing_stop_ai: bool,
    pub capital_rotation: bool,
    pub correlation_adjustment: bool,
    pub kelly_criterion: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StrategyConfig {
    pub id: String,
    pub name: String,
    pub strategy_type: StrategyType,
    pub description: String,
    pub symbols: Vec<String>,
    pub timeframe: TimeFrame,
    pub risk_params: RiskParams,
    pub ai_config: AIRiskConfig,
    pub enabled: bool,
    pub parameters: HashMap<String, f64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StrategyPerformance {
    pub strategy_id: String,
    pub total_trades: u32,
    pub win_rate: f64,
    pub total_pnl: f64,
    pub daily_pnl: f64,
    pub weekly_pnl: f64,
    pub monthly_pnl: f64,
    pub sharpe_ratio: f64,
    pub max_drawdown: f64,
    pub avg_trade_duration: f64,
    pub best_trade: f64,
    pub worst_trade: f64,
    pub risk_score: f64,
    pub allocation_percent: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TradingSignal {
    pub strategy_id: String,
    pub symbol: String,
    pub signal_type: SignalType,
    pub price: f64,
    pub quantity: f64,
    pub target_price: f64,
    pub stop_loss: f64,
    pub confidence: f64,
    pub reason: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SignalType {
    Buy,
    Sell,
    Hold,
}

// Predefined strategy configurations
impl StrategyConfig {
    pub fn vwap_bounce_scalper() -> Self {
        Self {
            id: "vwap-bounce-scalper".to_string(),
            name: "VWAP Bounce Scalper".to_string(),
            strategy_type: StrategyType::Scalping,
            description: "Price bounces off VWAP with volume confirmation".to_string(),
            symbols: vec!["SPY".to_string(), "TSLA".to_string(), "QQQ".to_string()],
            timeframe: TimeFrame::OneMin,
            risk_params: RiskParams {
                stop_loss: 0.4,
                take_profit: 0.75,
                position_size: 50.0,
                max_daily_loss: 5.0,
                max_drawdown: 15.0,
            },
            ai_config: AIRiskConfig {
                adaptive_position_sizing: true,
                trailing_stop_ai: true,
                capital_rotation: true,
                correlation_adjustment: true,
                kelly_criterion: true,
            },
            enabled: true,
            parameters: [
                ("vwap_period".to_string(), 20.0),
                ("volume_threshold".to_string(), 1.3),
                ("price_tolerance".to_string(), 0.002),
            ].iter().cloned().collect(),
        }
    }

    pub fn opening_range_breakout() -> Self {
        Self {
            id: "opening-range-breakout".to_string(),
            name: "Opening Range Breakout".to_string(),
            strategy_type: StrategyType::DayTrading,
            description: "First 30-min range breakout with volume confirmation".to_string(),
            symbols: vec!["SPY".to_string(), "QQQ".to_string(), "IWM".to_string()],
            timeframe: TimeFrame::FiveMin,
            risk_params: RiskParams {
                stop_loss: 1.0,
                take_profit: 2.0,
                position_size: 100.0,
                max_daily_loss: 5.0,
                max_drawdown: 15.0,
            },
            ai_config: AIRiskConfig {
                adaptive_position_sizing: true,
                trailing_stop_ai: true,
                capital_rotation: true,
                correlation_adjustment: true,
                kelly_criterion: true,
            },
            enabled: true,
            parameters: [
                ("range_minutes".to_string(), 30.0),
                ("volume_multiplier".to_string(), 2.0),
                ("min_risk_reward".to_string(), 1.5),
            ].iter().cloned().collect(),
        }
    }

    pub fn liquidity_grab_reversal() -> Self {
        Self {
            id: "liquidity-grab-reversal".to_string(),
            name: "Liquidity Grab Reversal".to_string(),
            strategy_type: StrategyType::SmartMoney,
            description: "Hunt retail stops then institutional reversal".to_string(),
            symbols: vec!["BTC-USD".to_string(), "ETH-USD".to_string(), "SPY".to_string()],
            timeframe: TimeFrame::FifteenMin,
            risk_params: RiskParams {
                stop_loss: 1.0,
                take_profit: 2.5,
                position_size: 70.0,
                max_daily_loss: 5.0,
                max_drawdown: 15.0,
            },
            ai_config: AIRiskConfig {
                adaptive_position_sizing: true,
                trailing_stop_ai: true,
                capital_rotation: true,
                correlation_adjustment: true,
                kelly_criterion: true,
            },
            enabled: true,
            parameters: [
                ("liquidity_threshold".to_string(), 0.999),
                ("volume_spike".to_string(), 2.0),
                ("order_imbalance".to_string(), 0.6),
            ].iter().cloned().collect(),
        }
    }

    pub fn fair_value_gap_fill() -> Self {
        Self {
            id: "fair-value-gap-fill".to_string(),
            name: "Fair Value Gap Fill".to_string(),
            strategy_type: StrategyType::SmartMoney,
            description: "Market inefficiency fills before trend continuation".to_string(),
            symbols: vec!["SPY".to_string(), "QQQ".to_string(), "BTC-USD".to_string()],
            timeframe: TimeFrame::FiveMin,
            risk_params: RiskParams {
                stop_loss: 0.8,
                take_profit: 2.0,
                position_size: 90.0,
                max_daily_loss: 5.0,
                max_drawdown: 15.0,
            },
            ai_config: AIRiskConfig {
                adaptive_position_sizing: true,
                trailing_stop_ai: true,
                capital_rotation: true,
                correlation_adjustment: true,
                kelly_criterion: true,
            },
            enabled: true,
            parameters: [
                ("gap_fill_threshold".to_string(), 0.5),
                ("trend_strength".to_string(), 0.7),
                ("continuation_target".to_string(), 1.02),
            ].iter().cloned().collect(),
        }
    }

    pub fn get_all_default_strategies() -> Vec<Self> {
        vec![
            Self::vwap_bounce_scalper(),
            Self::opening_range_breakout(),
            Self::liquidity_grab_reversal(),
            Self::fair_value_gap_fill(),
        ]
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PortfolioMetrics {
    pub total_capital: f64,
    pub available_capital: f64,
    pub allocated_capital: f64,
    pub unrealized_pnl: f64,
    pub realized_pnl: f64,
    pub daily_var: f64,
    pub portfolio_heat: f64,
    pub correlation_risk: f64,
    pub max_drawdown: f64,
    pub sharpe_ratio: f64,
    pub total_trades_today: u32,
    pub win_rate_today: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskManagementState {
    pub daily_loss: f64,
    pub max_daily_loss: f64,
    pub current_drawdown: f64,
    pub max_drawdown: f64,
    pub position_heat: HashMap<String, f64>,
    pub correlation_matrix: HashMap<String, HashMap<String, f64>>,
    pub kelly_positions: HashMap<String, f64>,
    pub risk_override: bool,
}

pub struct StrategyEngine {
    pub strategies: HashMap<String, StrategyConfig>,
    pub performances: HashMap<String, StrategyPerformance>,
    pub active_signals: Vec<TradingSignal>,
    pub portfolio: PortfolioMetrics,
    pub risk_state: RiskManagementState,
}

impl StrategyEngine {
    pub fn new() -> Self {
        let default_strategies = StrategyConfig::get_all_default_strategies();
        let mut strategies = HashMap::new();
        let mut performances = HashMap::new();

        for strategy in default_strategies {
            performances.insert(
                strategy.id.clone(),
                StrategyPerformance {
                    strategy_id: strategy.id.clone(),
                    total_trades: 0,
                    win_rate: 0.0,
                    total_pnl: 0.0,
                    daily_pnl: 0.0,
                    weekly_pnl: 0.0,
                    monthly_pnl: 0.0,
                    sharpe_ratio: 0.0,
                    max_drawdown: 0.0,
                    avg_trade_duration: 0.0,
                    best_trade: 0.0,
                    worst_trade: 0.0,
                    risk_score: 3.0,
                    allocation_percent: 25.0,
                },
            );
            strategies.insert(strategy.id.clone(), strategy);
        }

        Self {
            strategies,
            performances,
            active_signals: Vec::new(),
            portfolio: PortfolioMetrics {
                total_capital: 100000.0,
                available_capital: 75000.0,
                allocated_capital: 25000.0,
                unrealized_pnl: 0.0,
                realized_pnl: 0.0,
                daily_var: 2.1,
                portfolio_heat: 67.5,
                correlation_risk: 23.4,
                max_drawdown: 4.2,
                sharpe_ratio: 2.34,
                total_trades_today: 156,
                win_rate_today: 73.2,
            },
            risk_state: RiskManagementState {
                daily_loss: 0.0,
                max_daily_loss: 5000.0,
                current_drawdown: 0.0,
                max_drawdown: 15000.0,
                position_heat: HashMap::new(),
                correlation_matrix: HashMap::new(),
                kelly_positions: HashMap::new(),
                risk_override: false,
            },
        }
    }

    pub fn add_strategy(&mut self, strategy: StrategyConfig) {
        self.strategies.insert(strategy.id.clone(), strategy);
    }

    pub fn enable_strategy(&mut self, strategy_id: &str) -> Result<(), String> {
        if let Some(strategy) = self.strategies.get_mut(strategy_id) {
            strategy.enabled = true;
            Ok(())
        } else {
            Err(format!("Strategy {} not found", strategy_id))
        }
    }

    pub fn disable_strategy(&mut self, strategy_id: &str) -> Result<(), String> {
        if let Some(strategy) = self.strategies.get_mut(strategy_id) {
            strategy.enabled = false;
            Ok(())
        } else {
            Err(format!("Strategy {} not found", strategy_id))
        }
    }

    pub fn get_active_strategies(&self) -> Vec<&StrategyConfig> {
        self.strategies
            .values()
            .filter(|s| s.enabled)
            .collect()
    }

    pub fn calculate_position_size(
        &self,
        strategy_id: &str,
        price: f64,
        account_balance: f64,
    ) -> Result<f64, String> {
        let strategy = self.strategies.get(strategy_id)
            .ok_or_else(|| format!("Strategy {} not found", strategy_id))?;
        
        let performance = self.performances.get(strategy_id)
            .ok_or_else(|| format!("Performance data for {} not found", strategy_id))?;

        if strategy.ai_config.kelly_criterion && performance.win_rate > 0.0 {
            // Kelly Criterion with AI adjustments
            let kelly_fraction = (performance.win_rate * performance.total_pnl - (1.0 - performance.win_rate)) / performance.total_pnl;
            let kelly_adjusted = kelly_fraction.max(0.01).min(0.25); // Cap between 1% and 25%
            
            // Performance multiplier
            let performance_multiplier = if performance.sharpe_ratio > 2.0 { 1.2 }
                else if performance.sharpe_ratio > 1.0 { 1.0 }
                else if performance.sharpe_ratio > 0.5 { 0.8 }
                else { 0.5 };
            
            let base_amount = account_balance * kelly_adjusted;
            let adjusted_amount = base_amount * performance_multiplier;
            
            Ok(adjusted_amount / price)
        } else {
            // Fixed position sizing
            Ok(strategy.risk_params.position_size)
        }
    }
} 