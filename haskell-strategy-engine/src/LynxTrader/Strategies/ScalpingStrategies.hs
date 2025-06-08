{-# LANGUAGE OverloadedStrings #-}

module LynxTrader.Strategies.ScalpingStrategies where

import LynxTrader.LynxDSL
import LynxTrader.Types

-- | VWAP Bounce Scalper Strategy
-- Entry: Price bounces off VWAP with volume confirmation
-- Exit: 0.75% target, 0.4% stop loss
vwapBounceScalper :: Strategy
vwapBounceScalper = 
  newStrategy "VWAP-Bounce-Scalper"
    & withSymbols ["SPY", "TSLA", "BTC-USD", "QQQ"]
    & withTimeFrame OneMin
    & withRisk (RiskParams 0.4 0.75 50)
    & addIndicator (vwap 20)
    & addIndicator (volume 10)
    & setCondition scalperLogic
  where
    scalperLogic price volume indicators = do
      let vwapValue = getIndicator "VWAP" indicators
          avgVolume = getIndicator "Volume_MA" indicators
          nearVWAP = abs (price - vwapValue) < (price * 0.002) -- within 0.2%
          volumeSpike = volume > (avgVolume * 1.3)
          bullishCandle = True -- placeholder for candle pattern detection
      
      if nearVWAP && volumeSpike && bullishCandle
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 50
               , signal_target = price * 1.0075    -- 0.75% target
               , signal_stop = price * 0.996       -- 0.4% stop
               , signal_reason = "VWAP bounce with volume spike"
               }
        else Nothing

-- | Micro Breakout Trap Strategy  
-- Entry: Break above consolidation range with volume
-- Exit: Immediate momentum fade or 3-candle timeout
microBreakoutTrap :: Strategy
microBreakoutTrap = 
  newStrategy "Micro-Breakout-Trap"
    & withSymbols ["AAPL", "MSFT", "NVDA", "AMZN"]
    & withTimeFrame OneMin
    & withRisk (RiskParams 0.3 0.8 75)
    & addIndicator (bollingerBands 20 2.0)
    & addIndicator (volume 5)
    & setCondition breakoutLogic
  where
    breakoutLogic price volume indicators = do
      let bbUpper = getIndicator "BB_Upper" indicators
          bbLower = getIndicator "BB_Lower" indicators
          avgVolume = getIndicator "Volume_MA" indicators
          rangeWidth = bbUpper - bbLower
          tightRange = rangeWidth < (price * 0.01) -- less than 1% width
          breakoutUp = price > bbUpper
          volumeConfirm = volume > (avgVolume * 1.5)
      
      if tightRange && breakoutUp && volumeConfirm
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 75
               , signal_target = price * 1.008     -- 0.8% target
               , signal_stop = price * 0.997       -- 0.3% stop
               , signal_reason = "Micro breakout with volume"
               }
        else Nothing

-- | Fibonacci Reversal Scalper
-- Entry: Price hits 0.618 fib with reversal pattern
-- Exit: 0.7-1% scalp target
fibonacciReversalScalper :: Strategy
fibonacciReversalScalper = 
  newStrategy "Fibonacci-Reversal-Scalper"
    & withSymbols ["EUR/USD", "GBP/USD", "USD/JPY"]
    & withTimeFrame FiveMin
    & withRisk (RiskParams 0.5 1.0 40)
    & addIndicator (rsi 14)
    & addIndicator (fibonacciRetracement 50)
    & setCondition fibReversalLogic
  where
    fibReversalLogic price volume indicators = do
      let rsiValue = getIndicator "RSI" indicators
          fib618 = getIndicator "Fib_0618" indicators
          nearFib = abs (price - fib618) < (price * 0.001) -- within 0.1%
          oversold = rsiValue < 35
          hammerCandle = True -- placeholder for hammer pattern detection
      
      if nearFib && oversold && hammerCandle
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 40
               , signal_target = price * 1.007     -- 0.7% target
               , signal_stop = price * 0.995       -- 0.5% stop
               , signal_reason = "Fibonacci 0.618 reversal setup"
               }
        else Nothing 