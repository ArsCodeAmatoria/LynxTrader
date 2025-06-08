use rust_decimal::Decimal;
use tokio::sync::mpsc;
use uuid::Uuid;
use chrono::{DateTime, Utc};
use crate::models::{Trade, TradeType, OrderStatus, TradingSignal};

#[derive(Debug, Clone)]
pub struct OrderExecutor {
    broker_client: BrokerClient,
    order_queue: mpsc::UnboundedSender<OrderRequest>,
}

#[derive(Debug, Clone)]
pub struct OrderRequest {
    pub id: Uuid,
    pub symbol: String,
    pub trade_type: TradeType,
    pub quantity: Decimal,
    pub price: Option<Decimal>, // None for market orders
    pub stop_loss: Option<Decimal>,
    pub take_profit: Option<Decimal>,
    pub timestamp: DateTime<Utc>,
}

#[derive(Debug, Clone)]
pub enum BrokerClient {
    Alpaca(AlpacaClient),
    InteractiveBrokers(IBClient),
    Paper(PaperClient),
}

#[derive(Debug, Clone)]
pub struct AlpacaClient {
    api_key: String,
    secret_key: String,
    base_url: String,
    paper_trading: bool,
}

#[derive(Debug, Clone)]
pub struct IBClient {
    host: String,
    port: u16,
    client_id: i32,
}

#[derive(Debug, Clone)]
pub struct PaperClient {
    starting_balance: Decimal,
    current_balance: Decimal,
    positions: std::collections::HashMap<String, Decimal>,
}

impl OrderExecutor {
    pub fn new(broker_client: BrokerClient) -> Self {
        let (tx, mut rx) = mpsc::unbounded_channel();
        
        // Spawn order processing task
        let client = broker_client.clone();
        tokio::spawn(async move {
            while let Some(order) = rx.recv().await {
                Self::process_order(&client, order).await;
            }
        });

        Self {
            broker_client,
            order_queue: tx,
        }
    }

    /// Submit an order for execution
    pub async fn submit_order(&self, order: OrderRequest) -> Result<Uuid, OrderError> {
        tracing::info!("Submitting order: {:?}", order);
        
        self.order_queue.send(order.clone())
            .map_err(|_| OrderError::QueueError)?;
        
        Ok(order.id)
    }

    /// Process a single order
    async fn process_order(client: &BrokerClient, order: OrderRequest) {
        match client {
            BrokerClient::Alpaca(alpaca) => {
                Self::execute_alpaca_order(alpaca, &order).await;
            },
            BrokerClient::InteractiveBrokers(ib) => {
                Self::execute_ib_order(ib, &order).await;
            },
            BrokerClient::Paper(paper) => {
                Self::execute_paper_order(paper, &order).await;
            },
        }
    }

    /// Execute order through Alpaca
    async fn execute_alpaca_order(client: &AlpacaClient, order: &OrderRequest) {
        // TODO: Implement Alpaca API integration
        tracing::info!("Executing Alpaca order: {:?} {} {}", 
            order.trade_type, order.quantity, order.symbol);
        
        // Placeholder implementation
        tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
    }

    /// Execute order through Interactive Brokers
    async fn execute_ib_order(client: &IBClient, order: &OrderRequest) {
        // TODO: Implement IB TWS API integration
        tracing::info!("Executing IB order: {:?} {} {}", 
            order.trade_type, order.quantity, order.symbol);
        
        // Placeholder implementation
        tokio::time::sleep(tokio::time::Duration::from_millis(50)).await;
    }

    /// Execute paper trade
    async fn execute_paper_order(client: &PaperClient, order: &OrderRequest) {
        tracing::info!("Executing paper trade: {} {} {} @ market price", 
            match order.trade_type {
                TradeType::Buy => "BUY",
                TradeType::Sell => "SELL",
            },
            order.quantity, 
            order.symbol
        );
        
        // Simulate order fill with random delay (10-200ms)
        let delay = 10 + (order.id.as_u128() % 191) as u64; // Simple pseudo-random delay
        tokio::time::sleep(tokio::time::Duration::from_millis(delay)).await;
        
        // TODO: Update paper positions and balance
    }

    /// Cancel an existing order
    pub async fn cancel_order(&self, order_id: Uuid) -> Result<(), OrderError> {
        tracing::info!("Cancelling order: {}", order_id);
        // TODO: Implement order cancellation
        Ok(())
    }

    /// Get order status
    pub async fn get_order_status(&self, order_id: Uuid) -> Result<OrderStatus, OrderError> {
        // TODO: Implement order status checking
        Ok(OrderStatus::Pending)
    }
}

impl PaperClient {
    pub fn new(starting_balance: Decimal) -> Self {
        Self {
            starting_balance,
            current_balance: starting_balance,
            positions: std::collections::HashMap::new(),
        }
    }

    pub fn get_balance(&self) -> Decimal {
        self.current_balance
    }

    pub fn get_position(&self, symbol: &str) -> Decimal {
        self.positions.get(symbol).cloned().unwrap_or(Decimal::new(0, 0))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum OrderError {
    #[error("Failed to queue order")]
    QueueError,
    #[error("Broker connection error")]
    BrokerError,
    #[error("Insufficient funds")]
    InsufficientFunds,
    #[error("Invalid order parameters")]
    InvalidOrder,
    #[error("Order not found")]
    OrderNotFound,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_paper_order_execution() {
        let paper_client = PaperClient::new(Decimal::new(1000, 0));
        let broker_client = BrokerClient::Paper(paper_client);
        let executor = OrderExecutor::new(broker_client);

        let order = OrderRequest {
            id: Uuid::new_v4(),
            symbol: "AAPL".to_string(),
            trade_type: TradeType::Buy,
            quantity: Decimal::new(10, 0),
            price: Some(Decimal::new(15000, 2)), // $150.00
            stop_loss: None,
            take_profit: None,
            timestamp: Utc::now(),
        };

        let result = executor.submit_order(order).await;
        assert!(result.is_ok());
    }
} 