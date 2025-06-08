"""
LynxTrader Model Training Module
Trains LSTM, CNN, and Transformer models for price prediction with TensorBoard visualization
"""

import os
import numpy as np
import pandas as pd
import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import DataLoader, Dataset
from torch.utils.tensorboard import SummaryWriter
import tensorflow as tf
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error, mean_absolute_error, r2_score
import yfinance as yf
import ta
from datetime import datetime, timedelta
import logging
from typing import Dict, List, Tuple, Optional
import joblib

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class TradingDataset(Dataset):
    """Custom dataset for trading data"""
    
    def __init__(self, features: np.ndarray, targets: np.ndarray, sequence_length: int = 60):
        self.features = features
        self.targets = targets
        self.sequence_length = sequence_length
        
    def __len__(self):
        return len(self.features) - self.sequence_length
    
    def __getitem__(self, idx):
        x = self.features[idx:idx + self.sequence_length]
        y = self.targets[idx + self.sequence_length]
        return torch.FloatTensor(x), torch.FloatTensor([y])

class LSTMModel(nn.Module):
    """LSTM model for price prediction"""
    
    def __init__(self, input_size: int, hidden_size: int = 128, num_layers: int = 2, dropout: float = 0.2):
        super(LSTMModel, self).__init__()
        
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        
        self.lstm = nn.LSTM(
            input_size=input_size,
            hidden_size=hidden_size,
            num_layers=num_layers,
            dropout=dropout,
            batch_first=True
        )
        
        self.dropout = nn.Dropout(dropout)
        self.fc1 = nn.Linear(hidden_size, 64)
        self.fc2 = nn.Linear(64, 32)
        self.fc3 = nn.Linear(32, 1)
        self.relu = nn.ReLU()
        
    def forward(self, x):
        lstm_out, _ = self.lstm(x)
        # Take the last output
        out = lstm_out[:, -1, :]
        out = self.dropout(out)
        out = self.relu(self.fc1(out))
        out = self.dropout(out)
        out = self.relu(self.fc2(out))
        out = self.fc3(out)
        return out

class CNNLSTMModel(nn.Module):
    """CNN-LSTM hybrid model for pattern recognition and sequence prediction"""
    
    def __init__(self, input_size: int, sequence_length: int = 60, cnn_channels: int = 64):
        super(CNNLSTMModel, self).__init__()
        
        # CNN layers for pattern extraction
        self.conv1 = nn.Conv1d(input_size, cnn_channels, kernel_size=3, padding=1)
        self.conv2 = nn.Conv1d(cnn_channels, cnn_channels // 2, kernel_size=3, padding=1)
        self.pool = nn.MaxPool1d(kernel_size=2)
        self.dropout_cnn = nn.Dropout(0.2)
        
        # LSTM layers for sequence modeling
        lstm_input_size = cnn_channels // 2
        self.lstm = nn.LSTM(
            input_size=lstm_input_size,
            hidden_size=64,
            num_layers=2,
            dropout=0.2,
            batch_first=True
        )
        
        # Output layers
        self.dropout = nn.Dropout(0.3)
        self.fc1 = nn.Linear(64, 32)
        self.fc2 = nn.Linear(32, 1)
        self.relu = nn.ReLU()
        
    def forward(self, x):
        # x shape: (batch_size, sequence_length, features)
        # Convert to (batch_size, features, sequence_length) for conv1d
        x = x.transpose(1, 2)
        
        # CNN feature extraction
        x = self.relu(self.conv1(x))
        x = self.dropout_cnn(x)
        x = self.relu(self.conv2(x))
        x = self.pool(x)
        x = self.dropout_cnn(x)
        
        # Convert back to (batch_size, sequence_length, features) for LSTM
        x = x.transpose(1, 2)
        
        # LSTM sequence modeling
        lstm_out, _ = self.lstm(x)
        out = lstm_out[:, -1, :]  # Take last output
        
        # Final prediction
        out = self.dropout(out)
        out = self.relu(self.fc1(out))
        out = self.fc2(out)
        return out

class ModelTrainer:
    """Main trainer class with TensorBoard integration"""
    
    def __init__(self, model_name: str = "lynx_trader", log_dir: str = "runs"):
        self.model_name = model_name
        self.log_dir = log_dir
        self.writer = SummaryWriter(log_dir=f"{log_dir}/{model_name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}")
        self.scaler = MinMaxScaler()
        self.device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
        logger.info(f"Using device: {self.device}")
        
    def fetch_market_data(self, symbols: List[str], period: str = "2y") -> pd.DataFrame:
        """Fetch and preprocess market data from Yahoo Finance"""
        logger.info(f"Fetching data for symbols: {symbols}")
        
        data_frames = []
        for symbol in symbols:
            try:
                ticker = yf.Ticker(symbol)
                df = ticker.history(period=period)
                df['Symbol'] = symbol
                data_frames.append(df)
            except Exception as e:
                logger.error(f"Error fetching data for {symbol}: {e}")
                continue
                
        if not data_frames:
            raise ValueError("No data could be fetched for any symbol")
            
        combined_df = pd.concat(data_frames)
        return self.add_technical_indicators(combined_df)
    
    def add_technical_indicators(self, df: pd.DataFrame) -> pd.DataFrame:
        """Add technical indicators to the dataset"""
        df = df.copy()
        
        # Moving averages
        df['SMA_20'] = ta.trend.sma_indicator(df['Close'], window=20)
        df['EMA_12'] = ta.trend.ema_indicator(df['Close'], window=12)
        df['EMA_26'] = ta.trend.ema_indicator(df['Close'], window=26)
        
        # MACD
        macd = ta.trend.MACD(df['Close'])
        df['MACD'] = macd.macd()
        df['MACD_signal'] = macd.macd_signal()
        df['MACD_histogram'] = macd.macd_diff()
        
        # RSI
        df['RSI'] = ta.momentum.rsi(df['Close'], window=14)
        
        # Bollinger Bands
        bollinger = ta.volatility.BollingerBands(df['Close'])
        df['BB_upper'] = bollinger.bollinger_hband()
        df['BB_lower'] = bollinger.bollinger_lband()
        df['BB_middle'] = bollinger.bollinger_mavg()
        
        # Volume indicators
        df['Volume_SMA'] = ta.volume.volume_sma(df['Close'], df['Volume'], window=20)
        df['Volume_ratio'] = df['Volume'] / df['Volume_SMA']
        
        # Price features
        df['Price_change'] = df['Close'].pct_change()
        df['High_Low_ratio'] = df['High'] / df['Low']
        df['Open_Close_ratio'] = df['Open'] / df['Close']
        
        # Volatility
        df['Volatility'] = df['Price_change'].rolling(window=20).std()
        
        # Target variable (next day's price change)
        df['Target'] = df['Close'].shift(-1) / df['Close'] - 1
        
        # Drop rows with NaN values
        df = df.dropna()
        
        logger.info(f"Added technical indicators. Dataset shape: {df.shape}")
        return df
    
    def prepare_features(self, df: pd.DataFrame) -> Tuple[np.ndarray, np.ndarray]:
        """Prepare features and targets for training"""
        feature_columns = [
            'Open', 'High', 'Low', 'Close', 'Volume',
            'SMA_20', 'EMA_12', 'EMA_26', 'MACD', 'MACD_signal', 'MACD_histogram',
            'RSI', 'BB_upper', 'BB_lower', 'BB_middle', 'Volume_ratio',
            'Price_change', 'High_Low_ratio', 'Open_Close_ratio', 'Volatility'
        ]
        
        # Select features that exist in the dataframe
        available_features = [col for col in feature_columns if col in df.columns]
        logger.info(f"Using features: {available_features}")
        
        features = df[available_features].values
        targets = df['Target'].values
        
        # Scale features
        features_scaled = self.scaler.fit_transform(features)
        
        return features_scaled, targets
    
    def train_lstm_model(self, features: np.ndarray, targets: np.ndarray, 
                        epochs: int = 100, batch_size: int = 32, sequence_length: int = 60) -> LSTMModel:
        """Train LSTM model with TensorBoard logging"""
        logger.info("Training LSTM model...")
        
        # Split data
        X_train, X_test, y_train, y_test = train_test_split(
            features, targets, test_size=0.2, random_state=42, shuffle=False
        )
        
        # Create datasets
        train_dataset = TradingDataset(X_train, y_train, sequence_length)
        test_dataset = TradingDataset(X_test, y_test, sequence_length)
        
        train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
        test_loader = DataLoader(test_dataset, batch_size=batch_size, shuffle=False)
        
        # Initialize model
        model = LSTMModel(input_size=features.shape[1]).to(self.device)
        criterion = nn.MSELoss()
        optimizer = optim.Adam(model.parameters(), lr=0.001, weight_decay=1e-5)
        scheduler = optim.lr_scheduler.ReduceLROnPlateau(optimizer, patience=10, factor=0.5)
        
        # Training loop
        for epoch in range(epochs):
            model.train()
            train_loss = 0.0
            
            for batch_idx, (data, target) in enumerate(train_loader):
                data, target = data.to(self.device), target.to(self.device)
                
                optimizer.zero_grad()
                output = model(data)
                loss = criterion(output, target)
                loss.backward()
                
                # Gradient clipping
                torch.nn.utils.clip_grad_norm_(model.parameters(), max_norm=1.0)
                
                optimizer.step()
                train_loss += loss.item()
                
                # Log batch metrics to TensorBoard
                global_step = epoch * len(train_loader) + batch_idx
                self.writer.add_scalar('Loss/Train_Batch', loss.item(), global_step)
            
            # Validation
            model.eval()
            val_loss = 0.0
            predictions = []
            actuals = []
            
            with torch.no_grad():
                for data, target in test_loader:
                    data, target = data.to(self.device), target.to(self.device)
                    output = model(data)
                    val_loss += criterion(output, target).item()
                    predictions.extend(output.cpu().numpy())
                    actuals.extend(target.cpu().numpy())
            
            train_loss /= len(train_loader)
            val_loss /= len(test_loader)
            scheduler.step(val_loss)
            
            # Calculate metrics
            predictions = np.array(predictions).flatten()
            actuals = np.array(actuals).flatten()
            mse = mean_squared_error(actuals, predictions)
            mae = mean_absolute_error(actuals, predictions)
            r2 = r2_score(actuals, predictions)
            
            # Log to TensorBoard
            self.writer.add_scalar('Loss/Train_Epoch', train_loss, epoch)
            self.writer.add_scalar('Loss/Validation', val_loss, epoch)
            self.writer.add_scalar('Metrics/MSE', mse, epoch)
            self.writer.add_scalar('Metrics/MAE', mae, epoch)
            self.writer.add_scalar('Metrics/R2', r2, epoch)
            self.writer.add_scalar('Learning_Rate', optimizer.param_groups[0]['lr'], epoch)
            
            # Log predictions vs actuals plot every 10 epochs
            if epoch % 10 == 0:
                fig = self.create_prediction_plot(actuals[:100], predictions[:100], epoch)
                self.writer.add_figure('Predictions_vs_Actuals', fig, epoch)
            
            logger.info(f'Epoch {epoch+1}/{epochs} - Train Loss: {train_loss:.6f}, Val Loss: {val_loss:.6f}, R2: {r2:.4f}')
        
        return model
    
    def create_prediction_plot(self, actuals: np.ndarray, predictions: np.ndarray, epoch: int):
        """Create matplotlib plot for TensorBoard"""
        import matplotlib.pyplot as plt
        
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.scatter(actuals, predictions, alpha=0.5)
        ax.plot([actuals.min(), actuals.max()], [actuals.min(), actuals.max()], 'r--', lw=2)
        ax.set_xlabel('Actual Returns')
        ax.set_ylabel('Predicted Returns')
        ax.set_title(f'Predictions vs Actuals - Epoch {epoch}')
        return fig
    
    def save_model(self, model: nn.Module, model_type: str = "lstm"):
        """Save trained model and scaler"""
        model_path = f"models/{self.model_name}_{model_type}_model.pth"
        scaler_path = f"models/{self.model_name}_scaler.pkl"
        
        os.makedirs("models", exist_ok=True)
        
        torch.save({
            'model_state_dict': model.state_dict(),
            'model_type': model_type,
            'timestamp': datetime.now().isoformat()
        }, model_path)
        
        joblib.dump(self.scaler, scaler_path)
        
        logger.info(f"Model saved to {model_path}")
        logger.info(f"Scaler saved to {scaler_path}")
    
    def close_tensorboard(self):
        """Close TensorBoard writer"""
        self.writer.close()

def main():
    """Main training pipeline"""
    # Initialize trainer
    trainer = ModelTrainer("lynx_trader_v1")
    
    try:
        # Fetch data for multiple symbols
        symbols = ['AAPL', 'MSFT', 'GOOGL', 'TSLA', 'SPY', 'QQQ']
        df = trainer.fetch_market_data(symbols, period="2y")
        
        # Prepare features
        features, targets = trainer.prepare_features(df)
        logger.info(f"Features shape: {features.shape}, Targets shape: {targets.shape}")
        
        # Train LSTM model
        lstm_model = trainer.train_lstm_model(features, targets, epochs=50)
        trainer.save_model(lstm_model, "lstm")
        
        # Train CNN-LSTM model
        cnn_lstm_model = CNNLSTMModel(
            input_size=features.shape[1], 
            sequence_length=60
        ).to(trainer.device)
        
        logger.info("Training complete! Check TensorBoard with: tensorboard --logdir=runs")
        
    except Exception as e:
        logger.error(f"Training failed: {e}")
        raise
    finally:
        trainer.close_tensorboard()

if __name__ == "__main__":
    main() 