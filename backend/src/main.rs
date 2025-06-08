use axum::{
    extract::WebSocketUpgrade,
    response::Response,
    routing::{get, post},
    Router,
};
use tower_http::cors::CorsLayer;
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

mod order_executor;
mod trade_signal_router;
mod risk_manager;
mod models;
mod websocket;
mod alpaca_client;

#[tokio::main]
async fn main() {
    tracing_subscriber::registry()
        .with(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "lynxtrader_backend=debug,tower_http=debug".into()),
        )
        .with(tracing_subscriber::fmt::layer())
        .init();

    let app = Router::new()
        .route("/", get(root))
        .route("/api/bots", get(get_bots).post(create_bot))
        .route("/api/trades", get(get_trades))
        .route("/api/pnl", get(get_pnl))
        .route("/ws", get(websocket_handler))
        .layer(CorsLayer::permissive());

    let listener = tokio::net::TcpListener::bind("0.0.0.0:3001").await.unwrap();
    tracing::info!("LynxTrader Backend listening on {}", listener.local_addr().unwrap());
    
    axum::serve(listener, app).await.unwrap();
}

async fn root() -> &'static str {
    "LynxTrader Backend - Agile. Smart. Precise."
}

async fn get_bots() -> Result<axum::Json<Vec<models::BotStatus>>, axum::http::StatusCode> {
    // TODO: Implement bot status retrieval
    Ok(axum::Json(vec![
        models::BotStatus {
            id: "1".to_string(),
            name: "Scalp-Master".to_string(),
            status: models::BotState::Active,
            strategy: "EMA Crossover".to_string(),
            pnl: rust_decimal::Decimal::new(6723, 2), // 67.23
            trades: 15,
        }
    ]))
}

async fn create_bot() -> Result<axum::Json<models::BotStatus>, axum::http::StatusCode> {
    // TODO: Implement bot creation
    Err(axum::http::StatusCode::NOT_IMPLEMENTED)
}

async fn get_trades() -> Result<axum::Json<Vec<models::Trade>>, axum::http::StatusCode> {
    // TODO: Implement trade history retrieval
    Ok(axum::Json(vec![]))
}

async fn get_pnl() -> Result<axum::Json<models::PnLSummary>, axum::http::StatusCode> {
    // TODO: Implement P&L calculation
    Ok(axum::Json(models::PnLSummary {
        total_pnl: rust_decimal::Decimal::new(15678, 2), // 156.78
        daily_pnl: rust_decimal::Decimal::new(2345, 2),  // 23.45
        weekly_pnl: rust_decimal::Decimal::new(8912, 2), // 89.12
        total_trades: 24,
        win_rate: 67.5,
    }))
}

async fn websocket_handler(ws: WebSocketUpgrade) -> Response {
    ws.on_upgrade(websocket::handle_socket)
} 