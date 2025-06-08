use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::Json,
    routing::{delete, get, post, put},
    Router,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::strategies::{
    StrategyConfig, StrategyEngine, StrategyPerformance, TradingSignal, PortfolioMetrics,
    RiskManagementState, StrategyType,
};

#[derive(Serialize)]
pub struct StrategiesResponse {
    pub strategies: Vec<StrategyConfig>,
    pub total: usize,
}

#[derive(Serialize)]
pub struct PerformanceResponse {
    pub performances: Vec<StrategyPerformance>,
    pub portfolio: PortfolioMetrics,
    pub risk_state: RiskManagementState,
}

#[derive(Serialize)]
pub struct SignalsResponse {
    pub signals: Vec<TradingSignal>,
    pub active_count: usize,
}

#[derive(Deserialize)]
pub struct StrategyUpdateRequest {
    pub enabled: Option<bool>,
    pub parameters: Option<HashMap<String, f64>>,
}

#[derive(Deserialize)]
pub struct QueryFilters {
    pub strategy_type: Option<String>,
    pub enabled: Option<bool>,
    pub symbol: Option<String>,
}

pub type StrategyEngineState = Arc<Mutex<StrategyEngine>>;

pub fn routes() -> Router<StrategyEngineState> {
    Router::new()
        .route("/strategies", get(list_strategies).post(create_strategy))
        .route("/strategies/:id", get(get_strategy).put(update_strategy).delete(delete_strategy))
        .route("/strategies/:id/enable", post(enable_strategy))
        .route("/strategies/:id/disable", post(disable_strategy))
        .route("/performance", get(get_performance))
        .route("/performance/:id", get(get_strategy_performance))
        .route("/signals", get(get_signals))
        .route("/signals/active", get(get_active_signals))
        .route("/portfolio", get(get_portfolio_metrics))
        .route("/risk", get(get_risk_metrics))
        .route("/risk/override", post(toggle_risk_override))
}

// List all strategies with optional filtering
async fn list_strategies(
    Query(filters): Query<QueryFilters>,
    State(engine): State<StrategyEngineState>,
) -> Result<Json<StrategiesResponse>, StatusCode> {
    let engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    let mut strategies: Vec<StrategyConfig> = engine.strategies.values().cloned().collect();
    
    // Apply filters
    if let Some(strategy_type) = filters.strategy_type {
        let filter_type = match strategy_type.to_lowercase().as_str() {
            "scalping" => Some(StrategyType::Scalping),
            "day-trading" | "daytrading" => Some(StrategyType::DayTrading),
            "smart-money" | "smartmoney" => Some(StrategyType::SmartMoney),
            "swing-trading" | "swingtrading" => Some(StrategyType::SwingTrading),
            _ => None,
        };
        
        if let Some(filter_type) = filter_type {
            strategies.retain(|s| std::mem::discriminant(&s.strategy_type) == std::mem::discriminant(&filter_type));
        }
    }
    
    if let Some(enabled) = filters.enabled {
        strategies.retain(|s| s.enabled == enabled);
    }
    
    if let Some(symbol) = filters.symbol {
        strategies.retain(|s| s.symbols.contains(&symbol));
    }
    
    let total = strategies.len();
    
    Ok(Json(StrategiesResponse { strategies, total }))
}

// Get specific strategy by ID
async fn get_strategy(
    Path(id): Path<String>,
    State(engine): State<StrategyEngineState>,
) -> Result<Json<StrategyConfig>, StatusCode> {
    let engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    let strategy = engine.strategies.get(&id)
        .ok_or(StatusCode::NOT_FOUND)?;
    
    Ok(Json(strategy.clone()))
}

// Create new strategy
async fn create_strategy(
    State(engine): State<StrategyEngineState>,
    Json(strategy): Json<StrategyConfig>,
) -> Result<Json<StrategyConfig>, StatusCode> {
    let mut engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    // Check if strategy ID already exists
    if engine.strategies.contains_key(&strategy.id) {
        return Err(StatusCode::CONFLICT);
    }
    
    engine.add_strategy(strategy.clone());
    
    Ok(Json(strategy))
}

// Update strategy configuration
async fn update_strategy(
    Path(id): Path<String>,
    State(engine): State<StrategyEngineState>,
    Json(update): Json<StrategyUpdateRequest>,
) -> Result<Json<StrategyConfig>, StatusCode> {
    let mut engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    let strategy = engine.strategies.get_mut(&id)
        .ok_or(StatusCode::NOT_FOUND)?;
    
    if let Some(enabled) = update.enabled {
        strategy.enabled = enabled;
    }
    
    if let Some(parameters) = update.parameters {
        strategy.parameters.extend(parameters);
    }
    
    Ok(Json(strategy.clone()))
}

// Delete strategy
async fn delete_strategy(
    Path(id): Path<String>,
    State(engine): State<StrategyEngineState>,
) -> Result<StatusCode, StatusCode> {
    let mut engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    engine.strategies.remove(&id)
        .ok_or(StatusCode::NOT_FOUND)?;
    
    Ok(StatusCode::NO_CONTENT)
}

// Enable strategy
async fn enable_strategy(
    Path(id): Path<String>,
    State(engine): State<StrategyEngineState>,
) -> Result<Json<StrategyConfig>, StatusCode> {
    let mut engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    engine.enable_strategy(&id)
        .map_err(|_| StatusCode::NOT_FOUND)?;
    
    let strategy = engine.strategies.get(&id).unwrap();
    Ok(Json(strategy.clone()))
}

// Disable strategy
async fn disable_strategy(
    Path(id): Path<String>,
    State(engine): State<StrategyEngineState>,
) -> Result<Json<StrategyConfig>, StatusCode> {
    let mut engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    engine.disable_strategy(&id)
        .map_err(|_| StatusCode::NOT_FOUND)?;
    
    let strategy = engine.strategies.get(&id).unwrap();
    Ok(Json(strategy.clone()))
}

// Get all performance metrics
async fn get_performance(
    State(engine): State<StrategyEngineState>,
) -> Result<Json<PerformanceResponse>, StatusCode> {
    let engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    let performances: Vec<StrategyPerformance> = engine.performances.values().cloned().collect();
    
    Ok(Json(PerformanceResponse {
        performances,
        portfolio: engine.portfolio.clone(),
        risk_state: engine.risk_state.clone(),
    }))
}

// Get performance for specific strategy
async fn get_strategy_performance(
    Path(id): Path<String>,
    State(engine): State<StrategyEngineState>,
) -> Result<Json<StrategyPerformance>, StatusCode> {
    let engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    let performance = engine.performances.get(&id)
        .ok_or(StatusCode::NOT_FOUND)?;
    
    Ok(Json(performance.clone()))
}

// Get all signals
async fn get_signals(
    State(engine): State<StrategyEngineState>,
) -> Result<Json<SignalsResponse>, StatusCode> {
    let engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    let signals = engine.active_signals.clone();
    let active_count = signals.len();
    
    Ok(Json(SignalsResponse { signals, active_count }))
}

// Get active signals only
async fn get_active_signals(
    State(engine): State<StrategyEngineState>,
) -> Result<Json<Vec<TradingSignal>>, StatusCode> {
    let engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    // Filter for recent signals (last 24 hours)
    let now = chrono::Utc::now();
    let one_day_ago = now - chrono::Duration::days(1);
    
    let active_signals: Vec<TradingSignal> = engine.active_signals
        .iter()
        .filter(|signal| signal.timestamp > one_day_ago)
        .cloned()
        .collect();
    
    Ok(Json(active_signals))
}

// Get portfolio metrics
async fn get_portfolio_metrics(
    State(engine): State<StrategyEngineState>,
) -> Result<Json<PortfolioMetrics>, StatusCode> {
    let engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    Ok(Json(engine.portfolio.clone()))
}

// Get risk management metrics
async fn get_risk_metrics(
    State(engine): State<StrategyEngineState>,
) -> Result<Json<RiskManagementState>, StatusCode> {
    let engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    Ok(Json(engine.risk_state.clone()))
}

// Toggle risk override
async fn toggle_risk_override(
    State(engine): State<StrategyEngineState>,
) -> Result<Json<RiskManagementState>, StatusCode> {
    let mut engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    engine.risk_state.risk_override = !engine.risk_state.risk_override;
    
    Ok(Json(engine.risk_state.clone()))
}

#[derive(Serialize)]
pub struct HealthResponse {
    pub status: String,
    pub active_strategies: usize,
    pub total_trades_today: u32,
    pub portfolio_health: f64,
}

// Health check endpoint
pub async fn health_check(
    State(engine): State<StrategyEngineState>,
) -> Result<Json<HealthResponse>, StatusCode> {
    let engine = engine.lock().map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;
    
    let active_strategies = engine.get_active_strategies().len();
    let portfolio_health = if engine.portfolio.max_drawdown < 10.0 && engine.portfolio.sharpe_ratio > 1.0 {
        100.0
    } else if engine.portfolio.max_drawdown < 20.0 && engine.portfolio.sharpe_ratio > 0.5 {
        75.0
    } else if engine.portfolio.max_drawdown < 30.0 {
        50.0
    } else {
        25.0
    };
    
    Ok(Json(HealthResponse {
        status: "healthy".to_string(),
        active_strategies,
        total_trades_today: engine.portfolio.total_trades_today,
        portfolio_health,
    }))
} 