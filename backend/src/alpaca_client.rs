use reqwest::{Client, Response};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc};
use rust_decimal::Decimal;
use uuid::Uuid;
use crate::models::{Trade, TradeType, OrderStatus, MarketData};

/// Alpaca Markets API client for commission-free trading
/// https://alpaca.markets/
#[derive(Debug, Clone)]
pub struct AlpacaClient {
    base_url: String,
    api_key: String,
    secret_key: String,
    client: Client,
    paper_trading: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct AlpacaOrder {
    pub id: String,
    pub client_order_id: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub submitted_at: DateTime<Utc>,
    pub filled_at: Option<DateTime<Utc>>,
    pub expired_at: Option<DateTime<Utc>>,
    pub cancelled_at: Option<DateTime<Utc>>,
    pub failed_at: Option<DateTime<Utc>>,
    pub replaced_at: Option<DateTime<Utc>>,
    pub replaced_by: Option<String>,
    pub replaces: Option<String>,
    pub asset_id: String,
    pub symbol: String,
    pub asset_class: String,
    pub notional: Option<Decimal>,
    pub qty: Option<Decimal>,
    pub filled_qty: Decimal,
    pub filled_avg_price: Option<Decimal>,
    pub order_class: String,
    pub order_type: String,
    pub side: String,
    pub time_in_force: String,
    pub limit_price: Option<Decimal>,
    pub stop_price: Option<Decimal>,
    pub status: String,
    pub extended_hours: bool,
    pub legs: Option<Vec<AlpacaOrder>>,
    pub trail_percent: Option<Decimal>,
    pub trail_price: Option<Decimal>,
    pub hwm: Option<Decimal>,
}

#[derive(Debug, Serialize)]
pub struct CreateOrderRequest {
    pub symbol: String,
    pub qty: Option<Decimal>,
    pub notional: Option<Decimal>,
    pub side: String, // "buy" or "sell"
    pub r#type: String, // "market", "limit", "stop", "stop_limit", "trailing_stop"
    pub time_in_force: String, // "day", "gtc", "ioc", "fok"
    pub limit_price: Option<Decimal>,
    pub stop_price: Option<Decimal>,
    pub trail_price: Option<Decimal>,
    pub trail_percent: Option<Decimal>,
    pub extended_hours: Option<bool>,
    pub client_order_id: Option<String>,
    pub order_class: Option<String>, // "simple", "bracket", "oco", "oto"
    pub take_profit: Option<TakeProfitRequest>,
    pub stop_loss: Option<StopLossRequest>,
}

#[derive(Debug, Serialize)]
pub struct TakeProfitRequest {
    pub limit_price: Decimal,
}

#[derive(Debug, Serialize)]
pub struct StopLossRequest {
    pub stop_price: Decimal,
    pub limit_price: Option<Decimal>,
}

#[derive(Debug, Deserialize)]
pub struct AlpacaAccount {
    pub id: String,
    pub account_number: String,
    pub status: String,
    pub crypto_status: Option<String>,
    pub currency: String,
    pub buying_power: Decimal,
    pub regt_buying_power: Decimal,
    pub daytrading_buying_power: Decimal,
    pub effective_buying_power: Decimal,
    pub non_marginable_buying_power: Decimal,
    pub bod_dtbp: Decimal,
    pub cash: Decimal,
    pub accrued_fees: Decimal,
    pub pending_transfer_out: Option<Decimal>,
    pub pending_transfer_in: Option<Decimal>,
    pub portfolio_value: Decimal,
    pub pattern_day_trader: bool,
    pub trading_blocked: bool,
    pub transfers_blocked: bool,
    pub account_blocked: bool,
    pub created_at: DateTime<Utc>,
    pub trade_suspended_by_user: bool,
    pub multiplier: Decimal,
    pub shorting_enabled: bool,
    pub equity: Decimal,
    pub last_equity: Decimal,
    pub long_market_value: Decimal,
    pub short_market_value: Decimal,
    pub initial_margin: Decimal,
    pub maintenance_margin: Decimal,
    pub last_maintenance_margin: Decimal,
    pub sma: Decimal,
    pub daytrade_count: i32,
}

#[derive(Debug, Deserialize)]
pub struct AlpacaPosition {
    pub asset_id: String,
    pub symbol: String,
    pub exchange: String,
    pub asset_class: String,
    pub avg_entry_price: Decimal,
    pub qty: Decimal,
    pub side: String,
    pub market_value: Decimal,
    pub cost_basis: Decimal,
    pub unrealized_pl: Decimal,
    pub unrealized_plpc: Decimal,
    pub unrealized_intraday_pl: Decimal,
    pub unrealized_intraday_plpc: Decimal,
    pub current_price: Decimal,
    pub lastday_price: Decimal,
    pub change_today: Decimal,
}

#[derive(Debug, Deserialize)]
pub struct AlpacaBar {
    pub t: DateTime<Utc>,
    pub o: Decimal,
    pub h: Decimal,
    pub l: Decimal,
    pub c: Decimal,
    pub v: u64,
    pub n: u64,
    pub vw: Decimal,
}

#[derive(Debug, thiserror::Error)]
pub enum AlpacaError {
    #[error("HTTP request failed: {0}")]
    HttpError(#[from] reqwest::Error),
    #[error("API error: {code} - {message}")]
    ApiError { code: u16, message: String },
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
    #[error("Authentication failed")]
    AuthenticationError,
    #[error("Insufficient buying power")]
    InsufficientBuyingPower,
    #[error("Order rejected: {0}")]
    OrderRejected(String),
}

impl AlpacaClient {
    /// Create new Alpaca client
    /// For paper trading, use: https://paper-api.alpaca.markets
    /// For live trading, use: https://api.alpaca.markets
    pub fn new(api_key: String, secret_key: String, paper_trading: bool) -> Self {
        let base_url = if paper_trading {
            "https://paper-api.alpaca.markets".to_string()
        } else {
            "https://api.alpaca.markets".to_string()
        };

        Self {
            base_url,
            api_key,
            secret_key,
            client: Client::new(),
            paper_trading,
        }
    }

    /// Get authentication headers for API requests
    fn auth_headers(&self) -> HashMap<String, String> {
        let mut headers = HashMap::new();
        headers.insert("APCA-API-KEY-ID".to_string(), self.api_key.clone());
        headers.insert("APCA-API-SECRET-KEY".to_string(), self.secret_key.clone());
        headers.insert("Content-Type".to_string(), "application/json".to_string());
        headers
    }

    /// Make authenticated GET request
    async fn get<T: for<'de> Deserialize<'de>>(&self, endpoint: &str) -> Result<T, AlpacaError> {
        let url = format!("{}/v2/{}", self.base_url, endpoint);
        let headers = self.auth_headers();

        let mut request = self.client.get(&url);
        for (key, value) in headers {
            request = request.header(&key, &value);
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }

    /// Make authenticated POST request
    async fn post<T: for<'de> Deserialize<'de>, B: Serialize>(
        &self,
        endpoint: &str,
        body: &B,
    ) -> Result<T, AlpacaError> {
        let url = format!("{}/v2/{}", self.base_url, endpoint);
        let headers = self.auth_headers();

        let mut request = self.client.post(&url).json(body);
        for (key, value) in headers {
            request = request.header(&key, &value);
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }

    /// Handle API response and extract data or error
    async fn handle_response<T: for<'de> Deserialize<'de>>(
        &self,
        response: Response,
    ) -> Result<T, AlpacaError> {
        let status = response.status();
        let response_text = response.text().await?;

        if status.is_success() {
            serde_json::from_str(&response_text).map_err(AlpacaError::from)
        } else {
            Err(AlpacaError::ApiError {
                code: status.as_u16(),
                message: response_text,
            })
        }
    }

    /// Get account information
    pub async fn get_account(&self) -> Result<AlpacaAccount, AlpacaError> {
        tracing::info!("Fetching Alpaca account information");
        self.get("account").await
    }

    /// Get all open positions
    pub async fn get_positions(&self) -> Result<Vec<AlpacaPosition>, AlpacaError> {
        tracing::info!("Fetching Alpaca positions");
        self.get("positions").await
    }

    /// Get position for specific symbol
    pub async fn get_position(&self, symbol: &str) -> Result<AlpacaPosition, AlpacaError> {
        tracing::info!("Fetching position for symbol: {}", symbol);
        let endpoint = format!("positions/{}", symbol);
        self.get(&endpoint).await
    }

    /// Create a new order
    pub async fn create_order(&self, request: CreateOrderRequest) -> Result<AlpacaOrder, AlpacaError> {
        tracing::info!("Creating Alpaca order: {} {} {}", request.side, request.qty.unwrap_or(Decimal::ZERO), request.symbol);
        
        // Validate order
        if request.qty.is_none() && request.notional.is_none() {
            return Err(AlpacaError::OrderRejected("Either qty or notional must be specified".to_string()));
        }

        self.post("orders", &request).await
    }

    /// Get all orders (optionally filtered)
    pub async fn get_orders(&self, status: Option<&str>, symbols: Option<Vec<&str>>) -> Result<Vec<AlpacaOrder>, AlpacaError> {
        let mut endpoint = "orders".to_string();
        let mut params = Vec::new();

        if let Some(status) = status {
            params.push(format!("status={}", status));
        }
        if let Some(symbols) = symbols {
            params.push(format!("symbols={}", symbols.join(",")));
        }

        if !params.is_empty() {
            endpoint.push('?');
            endpoint.push_str(&params.join("&"));
        }

        tracing::info!("Fetching orders: {}", endpoint);
        self.get(&endpoint).await
    }

    /// Get specific order by ID
    pub async fn get_order(&self, order_id: &str) -> Result<AlpacaOrder, AlpacaError> {
        tracing::info!("Fetching order: {}", order_id);
        let endpoint = format!("orders/{}", order_id);
        self.get(&endpoint).await
    }

    /// Cancel an order
    pub async fn cancel_order(&self, order_id: &str) -> Result<(), AlpacaError> {
        tracing::info!("Cancelling order: {}", order_id);
        let url = format!("{}/v2/orders/{}", self.base_url, order_id);
        let headers = self.auth_headers();

        let mut request = self.client.delete(&url);
        for (key, value) in headers {
            request = request.header(&key, &value);
        }

        let response = request.send().await?;
        if response.status().is_success() {
            Ok(())
        } else {
            let error_text = response.text().await?;
            Err(AlpacaError::ApiError {
                code: response.status().as_u16(),
                message: error_text,
            })
        }
    }

    /// Cancel all open orders
    pub async fn cancel_all_orders(&self) -> Result<Vec<AlpacaOrder>, AlpacaError> {
        tracing::info!("Cancelling all orders");
        let url = format!("{}/v2/orders", self.base_url);
        let headers = self.auth_headers();

        let mut request = self.client.delete(&url);
        for (key, value) in headers {
            request = request.header(&key, &value);
        }

        let response = request.send().await?;
        self.handle_response(response).await
    }

    /// Get market data bars for a symbol
    pub async fn get_bars(
        &self,
        symbol: &str,
        timeframe: &str,
        start: Option<DateTime<Utc>>,
        end: Option<DateTime<Utc>>,
        limit: Option<u32>,
    ) -> Result<Vec<AlpacaBar>, AlpacaError> {
        let mut endpoint = format!("stocks/{}/bars", symbol);
        let mut params = vec![format!("timeframe={}", timeframe)];

        if let Some(start) = start {
            params.push(format!("start={}", start.format("%Y-%m-%dT%H:%M:%SZ")));
        }
        if let Some(end) = end {
            params.push(format!("end={}", end.format("%Y-%m-%dT%H:%M:%SZ")));
        }
        if let Some(limit) = limit {
            params.push(format!("limit={}", limit));
        }

        endpoint.push('?');
        endpoint.push_str(&params.join("&"));

        tracing::info!("Fetching bars for {}: {}", symbol, endpoint);
        
        // Market data endpoint is different
        let url = format!("https://data.alpaca.markets/v2/{}", endpoint);
        let headers = self.auth_headers();

        let mut request = self.client.get(&url);
        for (key, value) in headers {
            request = request.header(&key, &value);
        }

        let response = request.send().await?;
        let response_data: serde_json::Value = self.handle_response(response).await?;
        
        // Extract bars from response
        if let Some(bars) = response_data.get("bars").and_then(|b| b.get(symbol)) {
            serde_json::from_value(bars.clone()).map_err(AlpacaError::from)
        } else {
            Ok(Vec::new())
        }
    }

    /// Helper method to create a simple market buy order
    pub async fn market_buy(&self, symbol: &str, qty: Decimal) -> Result<AlpacaOrder, AlpacaError> {
        let request = CreateOrderRequest {
            symbol: symbol.to_string(),
            qty: Some(qty),
            notional: None,
            side: "buy".to_string(),
            r#type: "market".to_string(),
            time_in_force: "day".to_string(),
            limit_price: None,
            stop_price: None,
            trail_price: None,
            trail_percent: None,
            extended_hours: Some(false),
            client_order_id: Some(Uuid::new_v4().to_string()),
            order_class: Some("simple".to_string()),
            take_profit: None,
            stop_loss: None,
        };

        self.create_order(request).await
    }

    /// Helper method to create a simple market sell order
    pub async fn market_sell(&self, symbol: &str, qty: Decimal) -> Result<AlpacaOrder, AlpacaError> {
        let request = CreateOrderRequest {
            symbol: symbol.to_string(),
            qty: Some(qty),
            notional: None,
            side: "sell".to_string(),
            r#type: "market".to_string(),
            time_in_force: "day".to_string(),
            limit_price: None,
            stop_price: None,
            trail_price: None,
            trail_percent: None,
            extended_hours: Some(false),
            client_order_id: Some(Uuid::new_v4().to_string()),
            order_class: Some("simple".to_string()),
            take_profit: None,
            stop_loss: None,
        };

        self.create_order(request).await
    }

    /// Helper method to create a bracket order (with stop loss and take profit)
    pub async fn bracket_order(
        &self,
        symbol: &str,
        qty: Decimal,
        side: &str,
        limit_price: Decimal,
        stop_loss_price: Decimal,
        take_profit_price: Decimal,
    ) -> Result<AlpacaOrder, AlpacaError> {
        let request = CreateOrderRequest {
            symbol: symbol.to_string(),
            qty: Some(qty),
            notional: None,
            side: side.to_string(),
            r#type: "limit".to_string(),
            time_in_force: "day".to_string(),
            limit_price: Some(limit_price),
            stop_price: None,
            trail_price: None,
            trail_percent: None,
            extended_hours: Some(false),
            client_order_id: Some(Uuid::new_v4().to_string()),
            order_class: Some("bracket".to_string()),
            take_profit: Some(TakeProfitRequest {
                limit_price: take_profit_price,
            }),
            stop_loss: Some(StopLossRequest {
                stop_price: stop_loss_price,
                limit_price: None,
            }),
        };

        self.create_order(request).await
    }

    /// Convert Alpaca order to internal Trade format
    pub fn alpaca_order_to_trade(&self, order: &AlpacaOrder) -> Trade {
        Trade {
            id: Uuid::new_v4(), // Generate new UUID for internal tracking
            bot_id: "alpaca".to_string(),
            symbol: order.symbol.clone(),
            trade_type: if order.side == "buy" { TradeType::Buy } else { TradeType::Sell },
            quantity: order.qty.unwrap_or(Decimal::ZERO),
            price: order.filled_avg_price.unwrap_or(Decimal::ZERO),
            timestamp: order.created_at,
            status: match order.status.as_str() {
                "new" | "accepted" | "pending_new" => OrderStatus::Pending,
                "filled" => OrderStatus::Filled,
                "partially_filled" => OrderStatus::PartiallyFilled,
                "canceled" | "cancelled" => OrderStatus::Cancelled,
                "rejected" | "expired" => OrderStatus::Rejected,
                _ => OrderStatus::Pending,
            },
            pnl: None, // Calculate separately
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_alpaca_client_creation() {
        let client = AlpacaClient::new(
            "test_key".to_string(),
            "test_secret".to_string(),
            true, // paper trading
        );

        assert!(client.paper_trading);
        assert_eq!(client.base_url, "https://paper-api.alpaca.markets");
    }

    #[test]
    fn test_order_conversion() {
        let client = AlpacaClient::new(
            "test_key".to_string(),
            "test_secret".to_string(),
            true,
        );

        let alpaca_order = AlpacaOrder {
            id: "order_123".to_string(),
            client_order_id: "client_123".to_string(),
            created_at: Utc::now(),
            updated_at: Utc::now(),
            submitted_at: Utc::now(),
            filled_at: None,
            expired_at: None,
            cancelled_at: None,
            failed_at: None,
            replaced_at: None,
            replaced_by: None,
            replaces: None,
            asset_id: "asset_123".to_string(),
            symbol: "AAPL".to_string(),
            asset_class: "us_equity".to_string(),
            notional: None,
            qty: Some(Decimal::new(10, 0)),
            filled_qty: Decimal::new(0, 0),
            filled_avg_price: None,
            order_class: "simple".to_string(),
            order_type: "market".to_string(),
            side: "buy".to_string(),
            time_in_force: "day".to_string(),
            limit_price: None,
            stop_price: None,
            status: "new".to_string(),
            extended_hours: false,
            legs: None,
            trail_percent: None,
            trail_price: None,
            hwm: None,
        };

        let trade = client.alpaca_order_to_trade(&alpaca_order);
        assert_eq!(trade.symbol, "AAPL");
        assert_eq!(trade.trade_type, TradeType::Buy);
        assert_eq!(trade.quantity, Decimal::new(10, 0));
    }
} 