"""
LynxTrader Shorting Signals AI Module
AI-enhanced signal detection for shorting strategies with smart money insights
"""

import numpy as np
import pandas as pd
import torch
import torch.nn as nn
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass
from datetime import datetime, timedelta
import logging

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class ShortSignal:
    """Data class for shorting signals"""
    timestamp: datetime
    symbol: str
    strategy: str
    signal_type: str  # 'ENTRY', 'EXIT', 'MONITOR'
    confidence: float
    price: float
    volume: float
    metadata: Dict[str, float]

class LiquidityMapGenerator:
    """Detects liquidity grab levels and stop hunt zones"""
    
    def __init__(self, lookback_periods: int = 50):
        self.lookback_periods = lookback_periods
        
    def detect_liquidity_zones(self, df: pd.DataFrame) -> List[Dict]:
        """Identify potential liquidity grab zones"""
        zones = []
        
        # Find swing highs and lows
        swing_highs = self._find_swing_points(df['high'], mode='highs')
        swing_lows = self._find_swing_points(df['low'], mode='lows')
        
        # Analyze volume at key levels
        for high_idx in swing_highs:
            if high_idx >= len(df) - 1:
                continue
                
            high_price = df.iloc[high_idx]['high']
            volume_context = df.iloc[max(0, high_idx-5):high_idx+5]['volume'].mean()
            
            # Check for potential stop hunt setup
            recent_volume = df.iloc[high_idx]['volume']
            volume_ratio = recent_volume / volume_context if volume_context > 0 else 1
            
            if volume_ratio < 0.8:  # Low volume breakout = potential trap
                zones.append({
                    'level': high_price,
                    'type': 'liquidity_grab',
                    'confidence': min(1.0, (0.8 - volume_ratio) * 2),
                    'volume_ratio': volume_ratio,
                    'timestamp': df.iloc[high_idx]['timestamp'] if 'timestamp' in df.columns else high_idx
                })
        
        return zones
    
    def _find_swing_points(self, series: pd.Series, mode: str = 'highs', window: int = 5) -> List[int]:
        """Find swing highs or lows"""
        points = []
        
        for i in range(window, len(series) - window):
            if mode == 'highs':
                if all(series.iloc[i] >= series.iloc[i-j] for j in range(1, window+1)) and \
                   all(series.iloc[i] >= series.iloc[i+j] for j in range(1, window+1)):
                    points.append(i)
            else:  # lows
                if all(series.iloc[i] <= series.iloc[i-j] for j in range(1, window+1)) and \
                   all(series.iloc[i] <= series.iloc[i+j] for j in range(1, window+1)):
                    points.append(i)
        
        return points

class VolumeDivergenceDetector:
    """Identifies price moves without volume confirmation"""
    
    def __init__(self, volume_window: int = 20):
        self.volume_window = volume_window
        
    def detect_divergence(self, df: pd.DataFrame) -> Dict[str, float]:
        """Calculate volume divergence signals"""
        if len(df) < self.volume_window:
            return {'divergence_score': 0.0, 'volume_ratio': 1.0}
        
        # Calculate volume averages
        recent_volume = df['volume'].iloc[-5:].mean()
        avg_volume = df['volume'].iloc[-self.volume_window:].mean()
        
        # Calculate price movement
        price_change = abs(df['close'].iloc[-1] - df['close'].iloc[-5]) / df['close'].iloc[-5]
        
        # Volume ratio
        volume_ratio = recent_volume / avg_volume if avg_volume > 0 else 1.0
        
        # Divergence score: high price movement with low volume = suspicious
        divergence_score = price_change / max(volume_ratio, 0.1)
        
        return {
            'divergence_score': min(divergence_score, 2.0),
            'volume_ratio': volume_ratio,
            'price_change': price_change
        }

class ExhaustionPatternScanner:
    """Detects exhaustion patterns in bull markets"""
    
    def __init__(self, rsi_period: int = 14):
        self.rsi_period = rsi_period
        
    def calculate_rsi(self, prices: pd.Series) -> pd.Series:
        """Calculate RSI"""
        delta = prices.diff()
        gain = (delta.where(delta > 0, 0)).rolling(window=self.rsi_period).mean()
        loss = (-delta.where(delta < 0, 0)).rolling(window=self.rsi_period).mean()
        rs = gain / loss
        rsi = 100 - (100 / (1 + rs))
        return rsi
        
    def detect_exhaustion(self, df: pd.DataFrame) -> Dict[str, float]:
        """Detect exhaustion patterns"""
        if len(df) < self.rsi_period + 5:
            return {'exhaustion_score': 0.0, 'rsi': 50.0}
        
        # Calculate RSI
        rsi = self.calculate_rsi(df['close'])
        current_rsi = rsi.iloc[-1] if not pd.isna(rsi.iloc[-1]) else 50.0
        
        # Check for bearish candle patterns
        current_candle = df.iloc[-1]
        is_bearish = current_candle['close'] < current_candle['open']
        
        # Volume confirmation
        volume_spike = df['volume'].iloc[-1] > df['volume'].iloc[-5:].mean() * 1.5
        
        # Exhaustion score
        exhaustion_score = 0.0
        
        if current_rsi > 70:  # Overbought
            exhaustion_score += (current_rsi - 70) / 30  # Scale 0-1
            
        if is_bearish:
            exhaustion_score += 0.3
            
        if volume_spike:
            exhaustion_score += 0.2
            
        return {
            'exhaustion_score': min(exhaustion_score, 1.0),
            'rsi': current_rsi,
            'is_bearish': is_bearish,
            'volume_spike': volume_spike
        }

class SmartMoneyFlowTracker:
    """Tracks institutional money flow patterns"""
    
    def __init__(self, flow_window: int = 30):
        self.flow_window = flow_window
        
    def calculate_money_flow(self, df: pd.DataFrame) -> Dict[str, float]:
        """Calculate smart money flow indicators"""
        if len(df) < self.flow_window:
            return {'money_flow_score': 0.0, 'institutional_pressure': 0.0}
        
        # Calculate typical price and money flow
        typical_price = (df['high'] + df['low'] + df['close']) / 3
        money_flow = typical_price * df['volume']
        
        # Separate positive and negative money flow
        positive_flow = money_flow.where(typical_price.diff() > 0, 0).rolling(self.flow_window).sum()
        negative_flow = money_flow.where(typical_price.diff() < 0, 0).rolling(self.flow_window).sum()
        
        # Money Flow Index
        mfi = 100 - (100 / (1 + (positive_flow / negative_flow.abs())))
        current_mfi = mfi.iloc[-1] if not pd.isna(mfi.iloc[-1]) else 50.0
        
        # Institutional pressure (large volume moves)
        volume_threshold = df['volume'].rolling(self.flow_window).quantile(0.8)
        large_volume_moves = df['volume'] > volume_threshold.iloc[-1]
        
        institutional_pressure = large_volume_moves.iloc[-5:].sum() / 5  # Recent pressure
        
        return {
            'money_flow_score': (100 - current_mfi) / 100,  # Invert for short bias
            'institutional_pressure': institutional_pressure,
            'mfi': current_mfi
        }

class ShortingSignalEngine:
    """Main engine that combines all AI modules for shorting signals"""
    
    def __init__(self):
        self.liquidity_detector = LiquidityMapGenerator()
        self.volume_analyzer = VolumeDivergenceDetector()
        self.exhaustion_scanner = ExhaustionPatternScanner()
        self.flow_tracker = SmartMoneyFlowTracker()
        
    def generate_signals(self, symbol: str, df: pd.DataFrame) -> List[ShortSignal]:
        """Generate comprehensive shorting signals"""
        signals = []
        
        if len(df) < 50:  # Need sufficient data
            return signals
        
        try:
            # Get all AI insights
            liquidity_zones = self.liquidity_detector.detect_liquidity_zones(df)
            volume_analysis = self.volume_analyzer.detect_divergence(df)
            exhaustion_analysis = self.exhaustion_scanner.detect_exhaustion(df)
            flow_analysis = self.flow_tracker.calculate_money_flow(df)
            
            # Generate signals based on strategy combinations
            signals.extend(self._fake_breakout_signals(symbol, df, liquidity_zones, volume_analysis))
            signals.extend(self._exhaustion_signals(symbol, df, exhaustion_analysis, flow_analysis))
            signals.extend(self._smart_money_signals(symbol, df, liquidity_zones, flow_analysis))
            
        except Exception as e:
            logger.error(f"Error generating signals for {symbol}: {e}")
            
        return signals
    
    def _fake_breakout_signals(self, symbol: str, df: pd.DataFrame, 
                              liquidity_zones: List[Dict], volume_analysis: Dict) -> List[ShortSignal]:
        """Generate fake breakout reversal signals"""
        signals = []
        
        current_price = df['close'].iloc[-1]
        current_high = df['high'].iloc[-1]
        
        for zone in liquidity_zones:
            if zone['type'] == 'liquidity_grab' and current_high > zone['level']:
                # Price broke above liquidity level
                confidence = zone['confidence'] * 0.7 + volume_analysis.get('divergence_score', 0) * 0.3
                
                if confidence > 0.6:  # Minimum confidence threshold
                    signal = ShortSignal(
                        timestamp=datetime.now(),
                        symbol=symbol,
                        strategy='fake_breakout_reversal',
                        signal_type='ENTRY',
                        confidence=min(confidence, 1.0),
                        price=current_price,
                        volume=df['volume'].iloc[-1],
                        metadata={
                            'liquidity_level': zone['level'],
                            'volume_ratio': volume_analysis.get('volume_ratio', 1.0),
                            'divergence_score': volume_analysis.get('divergence_score', 0)
                        }
                    )
                    signals.append(signal)
        
        return signals
    
    def _exhaustion_signals(self, symbol: str, df: pd.DataFrame,
                           exhaustion_analysis: Dict, flow_analysis: Dict) -> List[ShortSignal]:
        """Generate exhaustion-based signals"""
        signals = []
        
        exhaustion_score = exhaustion_analysis.get('exhaustion_score', 0)
        money_flow_score = flow_analysis.get('money_flow_score', 0)
        
        combined_score = exhaustion_score * 0.6 + money_flow_score * 0.4
        
        if combined_score > 0.7:
            signal = ShortSignal(
                timestamp=datetime.now(),
                symbol=symbol,
                strategy='exhaustion_pattern',
                signal_type='ENTRY',
                confidence=combined_score,
                price=df['close'].iloc[-1],
                volume=df['volume'].iloc[-1],
                metadata={
                    'rsi': exhaustion_analysis.get('rsi', 50),
                    'exhaustion_score': exhaustion_score,
                    'money_flow_score': money_flow_score,
                    'institutional_pressure': flow_analysis.get('institutional_pressure', 0)
                }
            )
            signals.append(signal)
        
        return signals
    
    def _smart_money_signals(self, symbol: str, df: pd.DataFrame,
                            liquidity_zones: List[Dict], flow_analysis: Dict) -> List[ShortSignal]:
        """Generate smart money flow signals"""
        signals = []
        
        institutional_pressure = flow_analysis.get('institutional_pressure', 0)
        money_flow_score = flow_analysis.get('money_flow_score', 0)
        
        # Look for institutional selling pressure
        if institutional_pressure > 0.6 and money_flow_score > 0.5:
            signal = ShortSignal(
                timestamp=datetime.now(),
                symbol=symbol,
                strategy='smart_money_flow',
                signal_type='MONITOR',  # Conservative approach
                confidence=min(institutional_pressure + money_flow_score, 1.0),
                price=df['close'].iloc[-1],
                volume=df['volume'].iloc[-1],
                metadata={
                    'institutional_pressure': institutional_pressure,
                    'money_flow_index': flow_analysis.get('mfi', 50),
                    'flow_score': money_flow_score
                }
            )
            signals.append(signal)
        
        return signals

def main():
    """Example usage of the shorting signals engine"""
    engine = ShortingSignalEngine()
    
    # Example data (in practice, this would come from your data feed)
    dates = pd.date_range(start='2024-01-01', periods=100, freq='1H')
    np.random.seed(42)
    
    sample_data = pd.DataFrame({
        'timestamp': dates,
        'open': 100 + np.cumsum(np.random.randn(100) * 0.5),
        'high': 0,
        'low': 0,
        'close': 0,
        'volume': np.random.randint(10000, 100000, 100)
    })
    
    # Generate realistic OHLC data
    for i in range(len(sample_data)):
        open_price = sample_data.iloc[i]['open']
        close_price = open_price + np.random.randn() * 2
        high_price = max(open_price, close_price) + abs(np.random.randn() * 1)
        low_price = min(open_price, close_price) - abs(np.random.randn() * 1)
        
        sample_data.iloc[i, sample_data.columns.get_loc('high')] = high_price
        sample_data.iloc[i, sample_data.columns.get_loc('low')] = low_price
        sample_data.iloc[i, sample_data.columns.get_loc('close')] = close_price
    
    # Generate signals
    signals = engine.generate_signals('AAPL', sample_data)
    
    print(f"Generated {len(signals)} signals:")
    for signal in signals:
        print(f"  {signal.strategy}: {signal.signal_type} at ${signal.price:.2f} (confidence: {signal.confidence:.2f})")

if __name__ == "__main__":
    main() 