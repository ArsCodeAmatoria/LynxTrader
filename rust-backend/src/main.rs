use axum::{
    http::{
        header::{ACCEPT, AUTHORIZATION, CONTENT_TYPE},
        HeaderValue, Method,
    },
    routing::get,
    Router,
};
use std::sync::{Arc, Mutex};
use tower::ServiceBuilder;
use tower_http::{
    cors::CorsLayer,
    trace::TraceLayer,
};
use tracing::{info, Level};
use tracing_subscriber;

mod api;
mod strategies;

use api::strategies::{health_check, routes as strategy_routes, StrategyEngineState};
use strategies::StrategyEngine;

#[tokio::main]
async fn main() {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .init();

    info!("Starting LynxTrader Backend Server");

    // Initialize strategy engine
    let strategy_engine = Arc::new(Mutex::new(StrategyEngine::new()));
    
    info!("Strategy engine initialized with {} default strategies", 
          strategy_engine.lock().unwrap().strategies.len());

    // Configure CORS for frontend
    let cors = CorsLayer::new()
        .allow_origin("http://localhost:3000".parse::<HeaderValue>().unwrap())
        .allow_origin("http://127.0.0.1:3000".parse::<HeaderValue>().unwrap())
        .allow_methods([Method::GET, Method::POST, Method::PUT, Method::DELETE, Method::OPTIONS])
        .allow_headers([AUTHORIZATION, ACCEPT, CONTENT_TYPE]);

    // Build the application router
    let app = Router::new()
        .route("/", get(root))
        .route("/health", get(health_check))
        .nest("/api", strategy_routes())
        .layer(
            ServiceBuilder::new()
                .layer(TraceLayer::new_for_http())
                .layer(cors)
        )
        .with_state(strategy_engine);

    info!("Starting server on http://0.0.0.0:8080");

    // Start the server
    let listener = tokio::net::TcpListener::bind("0.0.0.0:8080")
        .await
        .expect("Failed to bind to address");

    info!("LynxTrader Backend Server running on port 8080");
    info!("API Documentation available at: http://localhost:8080/api/strategies");

    axum::serve(listener, app)
        .await
        .expect("Server failed to start");
}

async fn root() -> &'static str {
    "LynxTrader Backend API - Agile. Smart. Precise."
}

#[cfg(test)]
mod tests {
    use super::*;
    use axum::{
        body::Body,
        http::{Request, StatusCode},
    };
    use tower::ServiceExt;

    #[tokio::test]
    async fn test_health_endpoint() {
        let strategy_engine = Arc::new(Mutex::new(StrategyEngine::new()));
        let app = Router::new()
            .route("/health", get(health_check))
            .with_state(strategy_engine);

        let response = app
            .oneshot(Request::builder().uri("/health").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_root_endpoint() {
        let strategy_engine = Arc::new(Mutex::new(StrategyEngine::new()));
        let app = Router::new()
            .route("/", get(root))
            .with_state(strategy_engine);

        let response = app
            .oneshot(Request::builder().uri("/").body(Body::empty()).unwrap())
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }

    #[tokio::test]
    async fn test_strategies_endpoint() {
        let strategy_engine = Arc::new(Mutex::new(StrategyEngine::new()));
        let app = Router::new()
            .nest("/api", strategy_routes())
            .with_state(strategy_engine);

        let response = app
            .oneshot(
                Request::builder()
                    .uri("/api/strategies")
                    .body(Body::empty())
                    .unwrap(),
            )
            .await
            .unwrap();

        assert_eq!(response.status(), StatusCode::OK);
    }
} 