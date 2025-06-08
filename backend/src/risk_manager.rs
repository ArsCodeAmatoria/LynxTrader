use rust_decimal::Decimal;
use std::collections::HashMap;
use chrono::{DateTime, Utc};
use crate::models::{Trade, RiskParameters, TradeType};

#[derive(Debug, Clone)]
pub struct RiskManager {
    parameters: RiskParameters,
    current_positions: HashMap<String, Decimal>, // symbol -> position size
    daily_pnl: Decimal,
    max_drawdown: Decimal,
    equity_peak: Decimal,
}

impl RiskManager {
    pub fn new(parameters: RiskParameters) -> Self {
        Self {
            parameters,
            current_positions: HashMap::new(),
            daily_pnl: Decimal::new(0, 0),
            max_drawdown: Decimal::new(0, 0),
            equity_peak: Decimal::new(1000, 0), // Starting with $1000
        }
    }

    /// Validate if a trade can be executed based on risk parameters
    pub fn validate_trade(&self, symbol: &str, trade_type: &TradeType, quantity: Decimal, price: Decimal) -> Result<(), RiskError> {
        // Check position size limits
        let current_position = self.current_positions.get(symbol).cloned().unwrap_or(Decimal::new(0, 0));
        let new_position = match trade_type {
            TradeType::Buy => current_position + quantity,
            TradeType::Sell => current_position - quantity,
        };

        if new_position.abs() > self.parameters.max_position_size {
            return Err(RiskError::PositionSizeExceeded);
        }

        // Check daily loss limits
        if self.daily_pnl < -self.parameters.max_daily_loss {
            return Err(RiskError::DailyLossLimitExceeded);
        }

        // Check drawdown limits
        if self.max_drawdown > self.parameters.max_drawdown {
            return Err(RiskError::DrawdownLimitExceeded);
        }

        Ok(())
    }

    /// Calculate appropriate position size based on risk parameters
    pub fn calculate_position_size(&self, symbol: &str, entry_price: Decimal, stop_loss_price: Decimal, risk_amount: Decimal) -> Decimal {
        let risk_per_share = (entry_price - stop_loss_price).abs();
        if risk_per_share == Decimal::new(0, 0) {
            return Decimal::new(0, 0);
        }

        let calculated_size = risk_amount / risk_per_share;
        
        // Cap at maximum position size
        if calculated_size > self.parameters.max_position_size {
            self.parameters.max_position_size
        } else {
            calculated_size
        }
    }

    /// Update risk metrics after a trade
    pub fn update_after_trade(&mut self, trade: &Trade) {
        // Update position
        let current_position = self.current_positions.get(&trade.symbol).cloned().unwrap_or(Decimal::new(0, 0));
        let new_position = match trade.trade_type {
            TradeType::Buy => current_position + trade.quantity,
            TradeType::Sell => current_position - trade.quantity,
        };
        
        if new_position == Decimal::new(0, 0) {
            self.current_positions.remove(&trade.symbol);
        } else {
            self.current_positions.insert(trade.symbol.clone(), new_position);
        }

        // Update P&L if available
        if let Some(pnl) = trade.pnl {
            self.daily_pnl += pnl;
            
            // Update equity peak and drawdown
            let current_equity = self.equity_peak + self.daily_pnl;
            if current_equity > self.equity_peak {
                self.equity_peak = current_equity;
            } else {
                let drawdown = self.equity_peak - current_equity;
                if drawdown > self.max_drawdown {
                    self.max_drawdown = drawdown;
                }
            }
        }
    }

    /// Check if emergency stop should be triggered
    pub fn should_emergency_stop(&self) -> bool {
        self.daily_pnl < -self.parameters.max_daily_loss ||
        self.max_drawdown > self.parameters.max_drawdown
    }

    /// Reset daily metrics (call at market open)
    pub fn reset_daily_metrics(&mut self) {
        self.daily_pnl = Decimal::new(0, 0);
    }

    /// Get current risk metrics
    pub fn get_metrics(&self) -> RiskMetrics {
        RiskMetrics {
            daily_pnl: self.daily_pnl,
            max_drawdown: self.max_drawdown,
            equity_peak: self.equity_peak,
            position_count: self.current_positions.len(),
            largest_position: self.current_positions.values()
                .map(|p| p.abs())
                .max()
                .unwrap_or(Decimal::new(0, 0)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct RiskMetrics {
    pub daily_pnl: Decimal,
    pub max_drawdown: Decimal,
    pub equity_peak: Decimal,
    pub position_count: usize,
    pub largest_position: Decimal,
}

#[derive(Debug, thiserror::Error)]
pub enum RiskError {
    #[error("Position size would exceed maximum allowed")]
    PositionSizeExceeded,
    #[error("Daily loss limit exceeded")]
    DailyLossLimitExceeded,
    #[error("Maximum drawdown limit exceeded")]
    DrawdownLimitExceeded,
    #[error("Invalid risk parameters")]
    InvalidParameters,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn default_risk_params() -> RiskParameters {
        RiskParameters {
            max_position_size: Decimal::new(100, 0),
            max_daily_loss: Decimal::new(50, 0),
            max_drawdown: Decimal::new(100, 0),
            stop_loss_pct: 2.0,
            take_profit_pct: 4.0,
        }
    }

    #[test]
    fn test_position_size_calculation() {
        let risk_manager = RiskManager::new(default_risk_params());
        let entry_price = Decimal::new(10000, 2); // $100.00
        let stop_loss_price = Decimal::new(9800, 2); // $98.00
        let risk_amount = Decimal::new(20, 0); // $20

        let position_size = risk_manager.calculate_position_size(
            "AAPL", 
            entry_price, 
            stop_loss_price, 
            risk_amount
        );

        // Risk per share = $2.00, so position size should be $20 / $2 = 10 shares
        assert_eq!(position_size, Decimal::new(10, 0));
    }

    #[test]
    fn test_trade_validation() {
        let risk_manager = RiskManager::new(default_risk_params());
        
        // Valid trade
        let result = risk_manager.validate_trade(
            "AAPL", 
            &TradeType::Buy, 
            Decimal::new(50, 0), 
            Decimal::new(10000, 2)
        );
        assert!(result.is_ok());

        // Invalid trade - exceeds position size
        let result = risk_manager.validate_trade(
            "AAPL", 
            &TradeType::Buy, 
            Decimal::new(150, 0), 
            Decimal::new(10000, 2)
        );
        assert!(matches!(result, Err(RiskError::PositionSizeExceeded)));
    }
} 