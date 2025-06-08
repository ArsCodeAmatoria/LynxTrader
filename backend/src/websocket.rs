use axum::extract::ws::{Message, WebSocket};
use futures_util::{SinkExt, StreamExt};
use serde_json::{json, Value};
use tokio::sync::broadcast;

/// Handle a single WebSocket connection
pub async fn handle_socket(socket: WebSocket) {
    let (mut sender, mut receiver) = socket.split();
    
    // Create a broadcast channel for real-time updates
    let (tx, mut rx) = broadcast::channel(100);
    
    // Spawn task to send updates to client
    let send_task = tokio::spawn(async move {
        while let Ok(message) = rx.recv().await {
            if sender.send(Message::Text(message)).await.is_err() {
                break;
            }
        }
    });

    // Spawn task to handle incoming messages
    let tx_clone = tx.clone();
    let receive_task = tokio::spawn(async move {
        while let Some(msg) = receiver.next().await {
            if let Ok(msg) = msg {
                if let Message::Text(text) = msg {
                    handle_client_message(&text, &tx_clone).await;
                }
            } else {
                break;
            }
        }
    });

    // Send initial connection message
    let welcome_msg = json!({
        "type": "connection",
        "status": "connected",
        "message": "LynxTrader WebSocket connection established"
    });
    
    if tx.send(welcome_msg.to_string()).is_err() {
        tracing::warn!("Failed to send welcome message");
    }

    // Start sending periodic updates
    let tx_updates = tx.clone();
    let update_task = tokio::spawn(async move {
        let mut interval = tokio::time::interval(tokio::time::Duration::from_secs(5));
        loop {
            interval.tick().await;
            send_market_update(&tx_updates).await;
        }
    });

    // Wait for either task to complete
    tokio::select! {
        _ = send_task => {},
        _ = receive_task => {},
        _ = update_task => {},
    }
}

/// Handle incoming client messages
async fn handle_client_message(text: &str, tx: &broadcast::Sender<String>) {
    match serde_json::from_str::<Value>(text) {
        Ok(json) => {
            if let Some(msg_type) = json.get("type").and_then(|v| v.as_str()) {
                match msg_type {
                    "ping" => {
                        let pong = json!({
                            "type": "pong",
                            "timestamp": chrono::Utc::now().timestamp()
                        });
                        let _ = tx.send(pong.to_string());
                    }
                    "subscribe" => {
                        if let Some(channel) = json.get("channel").and_then(|v| v.as_str()) {
                            tracing::info!("Client subscribed to channel: {}", channel);
                            let response = json!({
                                "type": "subscription_confirmed",
                                "channel": channel
                            });
                            let _ = tx.send(response.to_string());
                        }
                    }
                    "bot_command" => {
                        handle_bot_command(&json, tx).await;
                    }
                    _ => {
                        tracing::warn!("Unknown message type: {}", msg_type);
                    }
                }
            }
        }
        Err(e) => {
            tracing::warn!("Failed to parse client message: {}", e);
        }
    }
}

/// Handle bot control commands
async fn handle_bot_command(json: &Value, tx: &broadcast::Sender<String>) {
    if let (Some(action), Some(bot_id)) = (
        json.get("action").and_then(|v| v.as_str()),
        json.get("bot_id").and_then(|v| v.as_str())
    ) {
        tracing::info!("Bot command: {} for bot {}", action, bot_id);
        
        let response = match action {
            "start" => {
                // TODO: Start the specified bot
                json!({
                    "type": "bot_status_update",
                    "bot_id": bot_id,
                    "status": "active",
                    "message": format!("Bot {} started", bot_id)
                })
            }
            "stop" => {
                // TODO: Stop the specified bot
                json!({
                    "type": "bot_status_update", 
                    "bot_id": bot_id,
                    "status": "stopped",
                    "message": format!("Bot {} stopped", bot_id)
                })
            }
            "pause" => {
                // TODO: Pause the specified bot
                json!({
                    "type": "bot_status_update",
                    "bot_id": bot_id, 
                    "status": "paused",
                    "message": format!("Bot {} paused", bot_id)
                })
            }
            _ => {
                json!({
                    "type": "error",
                    "message": format!("Unknown bot command: {}", action)
                })
            }
        };
        
        let _ = tx.send(response.to_string());
    }
}

/// Send periodic market updates
async fn send_market_update(tx: &broadcast::Sender<String>) {
    // Generate mock market data
    let symbols = ["AAPL", "MSFT", "TSLA", "SPY", "QQQ"];
    
    for symbol in symbols {
        let price = 100.0 + (std::ptr::addr_of!(symbol) as usize % 100) as f64;
        let change = (price * 0.02) - 1.0; // Mock price change
        
        let update = json!({
            "type": "market_data",
            "symbol": symbol,
            "price": price,
            "change": change,
            "change_percent": (change / price) * 100.0,
            "timestamp": chrono::Utc::now().timestamp()
        });
        
        let _ = tx.send(update.to_string());
    }
    
    // Send P&L update
    let pnl_update = json!({
        "type": "pnl_update",
        "daily_pnl": 23.45,
        "total_pnl": 156.78,
        "unrealized_pnl": 12.34,
        "timestamp": chrono::Utc::now().timestamp()
    });
    
    let _ = tx.send(pnl_update.to_string());
} 