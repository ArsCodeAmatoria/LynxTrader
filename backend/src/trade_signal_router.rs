use tokio::sync::mpsc;
use crate::models::{TradingSignal, TradeType};
use crate::order_executor::{OrderExecutor, OrderRequest};
use crate::risk_manager::{RiskManager, RiskError};
use uuid::Uuid;
use chrono::Utc;

#[derive(Debug)]
pub struct TradeSignalRouter {
    signal_receiver: mpsc::UnboundedReceiver<TradingSignal>,
    order_executor: OrderExecutor,
    risk_manager: RiskManager,
}

impl TradeSignalRouter {
    pub fn new(
        signal_receiver: mpsc::UnboundedReceiver<TradingSignal>,
        order_executor: OrderExecutor,
        risk_manager: RiskManager,
    ) -> Self {
        Self {
            signal_receiver,
            order_executor,
            risk_manager,
        }
    }

    /// Start processing signals
    pub async fn start_processing(&mut self) {
        tracing::info!("Trade Signal Router started");
        
        while let Some(signal) = self.signal_receiver.recv().await {
            self.process_signal(signal).await;
        }
    }

    /// Process a single trading signal
    async fn process_signal(&mut self, signal: TradingSignal) {
        tracing::debug!("Processing signal: {:?}", signal);

        // Validate signal strength threshold
        if signal.strength < 0.7 {
            tracing::debug!("Signal strength {} below threshold, ignoring", signal.strength);
            return;
        }

        // Calculate position size based on risk management
        let position_size = self.calculate_position_size(&signal);
        if position_size.is_zero() {
            tracing::warn!("Position size calculated as zero for signal: {:?}", signal);
            return;
        }

        // Validate trade with risk manager
        match self.risk_manager.validate_trade(
            &signal.symbol,
            &signal.signal_type,
            position_size,
            signal.price,
        ) {
            Ok(()) => {
                self.execute_signal_as_order(signal, position_size).await;
            }
            Err(risk_error) => {
                tracing::warn!("Risk validation failed for signal {}: {}", signal.id, risk_error);
            }
        }
    }

    /// Calculate appropriate position size for the signal
    fn calculate_position_size(&self, signal: &TradingSignal) -> rust_decimal::Decimal {
        // For now, use a simple calculation based on signal strength
        // In production, this would consider:
        // - Account balance
        // - Risk per trade
        // - Stop loss levels
        // - Position correlation
        
        let base_size = rust_decimal::Decimal::new(10, 0); // 10 shares/units
        let strength_multiplier = rust_decimal::Decimal::from_f64_retain(signal.strength).unwrap_or(rust_decimal::Decimal::new(1, 0));
        
        base_size * strength_multiplier
    }

    /// Convert signal to order and execute
    async fn execute_signal_as_order(&self, signal: TradingSignal, position_size: rust_decimal::Decimal) {
        let order = OrderRequest {
            id: Uuid::new_v4(),
            symbol: signal.symbol.clone(),
            trade_type: signal.signal_type,
            quantity: position_size,
            price: Some(signal.price), // Limit order at signal price
            stop_loss: None, // TODO: Calculate from risk parameters
            take_profit: None, // TODO: Calculate from risk parameters
            timestamp: Utc::now(),
        };

        match self.order_executor.submit_order(order).await {
            Ok(order_id) => {
                tracing::info!("Order {} submitted for signal {}", order_id, signal.id);
            }
            Err(error) => {
                tracing::error!("Failed to submit order for signal {}: {}", signal.id, error);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::RiskParameters;
    use crate::order_executor::{BrokerClient, PaperClient};

    #[tokio::test]
    async fn test_signal_processing() {
        let (tx, rx) = mpsc::unbounded_channel();
        let paper_client = PaperClient::new(rust_decimal::Decimal::new(1000, 0));
        let broker_client = BrokerClient::Paper(paper_client);
        let order_executor = OrderExecutor::new(broker_client);
        
        let risk_params = RiskParameters {
            max_position_size: rust_decimal::Decimal::new(100, 0),
            max_daily_loss: rust_decimal::Decimal::new(50, 0),
            max_drawdown: rust_decimal::Decimal::new(100, 0),
            stop_loss_pct: 2.0,
            take_profit_pct: 4.0,
        };
        let risk_manager = RiskManager::new(risk_params);

        let mut router = TradeSignalRouter::new(rx, order_executor, risk_manager);

        // Send a test signal
        let signal = TradingSignal {
            id: Uuid::new_v4(),
            bot_id: "test-bot".to_string(),
            symbol: "AAPL".to_string(),
            signal_type: TradeType::Buy,
            strength: 0.8,
            price: rust_decimal::Decimal::new(15000, 2), // $150.00
            timestamp: Utc::now(),
            indicators: serde_json::json!({"ema": 148.5, "rsi": 65.0}),
        };

        tx.send(signal).unwrap();
        drop(tx); // Close the channel

        // Process signals (will exit when channel is closed)
        router.start_processing().await;
    }
} 