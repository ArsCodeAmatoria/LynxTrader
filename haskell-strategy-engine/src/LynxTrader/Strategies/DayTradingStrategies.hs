{-# LANGUAGE OverloadedStrings #-}

module LynxTrader.Strategies.DayTradingStrategies where

import LynxTrader.LynxDSL
import LynxTrader.Types
import Data.Time

-- | Opening Range Breakout (ORB) Strategy
-- Entry: Break of first 30-min range with volume
-- Time Filter: Only 9:30-10:30 AM EST
openingRangeBreakout :: Strategy
openingRangeBreakout = 
  newStrategy "Opening-Range-Breakout"
    & withSymbols ["SPY", "QQQ", "IWM", "AAPL", "TSLA"]
    & withTimeFrame FiveMin
    & withRisk (RiskParams 1.0 2.0 100)
    & addIndicator (openingRange 30) -- 30-minute opening range
    & addIndicator (volume 20)
    & addIndicator (vwap 50)
    & setCondition orbLogic
    & withTimeFilter (TimeFilter (TimeOfDay 9 30 0) (TimeOfDay 10 30 0))
  where
    orbLogic price volume indicators = do
      let orHigh = getIndicator "OR_High" indicators
          orLow = getIndicator "OR_Low" indicators
          avgVolume = getIndicator "Volume_MA" indicators
          vwapValue = getIndicator "VWAP" indicators
          
          -- Breakout conditions
          bullishBreakout = price > orHigh && volume > (avgVolume * 2.0)
          bearishBreakout = price < orLow && volume > (avgVolume * 2.0)
          
          -- Risk management
          riskReward = if bullishBreakout 
                      then (vwapValue - orHigh) / (orHigh - orLow) > 1.5
                      else (orLow - vwapValue) / (orHigh - orLow) > 1.5
      
      if bullishBreakout && riskReward
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 100
               , signal_target = price + (2 * (orHigh - orLow)) -- 2:1 R:R
               , signal_stop = orLow
               , signal_reason = "ORB bullish breakout with volume"
               }
        else if bearishBreakout && riskReward
        then Just $ SellSignal
               { signal_price = price
               , signal_quantity = 100
               , signal_target = price - (2 * (orHigh - orLow)) -- 2:1 R:R
               , signal_stop = orHigh
               , signal_reason = "ORB bearish breakout with volume"
               }
        else Nothing

-- | Trendline Break + Retest Strategy
-- Entry: Trendline break with retest confirmation
-- AI Enhancement: Auto-detect trendlines using price clusters
trendlineBreakRetest :: Strategy
trendlineBreakRetest = 
  newStrategy "Trendline-Break-Retest"
    & withSymbols ["NVDA", "AMD", "GOOGL", "META"]
    & withTimeFrame FifteenMin
    & withRisk (RiskParams 1.5 3.0 80)
    & addIndicator (trendlineDetector 50) -- AI-enhanced trendline detection
    & addIndicator (volume 14)
    & addIndicator (rsi 14)
    & setCondition trendlineLogic
  where
    trendlineLogic price volume indicators = do
      let trendlinePrice = getIndicator "Trendline_Price" indicators
          trendlineSlope = getIndicator "Trendline_Slope" indicators
          avgVolume = getIndicator "Volume_MA" indicators
          rsiValue = getIndicator "RSI" indicators
          
          -- Break and retest conditions
          brokeAbove = price > trendlinePrice * 1.002 -- 0.2% buffer
          retestSuccessful = price > trendlinePrice && price < (trendlinePrice * 1.01)
          volumeConfirmation = volume > (avgVolume * 1.4)
          momentum = rsiValue > 55 && rsiValue < 80
          engulfingPattern = True -- placeholder for pattern detection
      
      if brokeAbove && retestSuccessful && volumeConfirmation && momentum && engulfingPattern
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 80
               , signal_target = price * 1.03      -- 3% target
               , signal_stop = trendlinePrice * 0.985 -- below trendline
               , signal_reason = "Trendline break and retest confirmed"
               }
        else Nothing

-- | News Momentum Trade Strategy
-- Entry: After news catalyst forms flag/range
-- AI Enhancement: NLP sentiment analysis
newsMomentumTrade :: Strategy
newsMomentumTrade = 
  newStrategy "News-Momentum-Trade"
    & withSymbols ["TSLA", "AAPL", "NVDA", "META", "AMZN"]
    & withTimeFrame TenMin
    & withRisk (RiskParams 2.0 4.0 60)
    & addIndicator (newsImpactScore 30) -- AI news sentiment
    & addIndicator (volatilityFilter 20)
    & addIndicator (flagPattern 15)
    & setCondition newsLogic
  where
    newsLogic price volume indicators = do
      let newsScore = getIndicator "News_Impact" indicators
          volatility = getIndicator "Volatility" indicators
          flagBreakout = getIndicator "Flag_Breakout" indicators
          
          -- News momentum conditions
          highImpactNews = newsScore > 0.7 -- Bullish sentiment > 70%
          lowVolatility = volatility < 0.05 -- Consolidation phase
          flagBreak = flagBreakout > price * 0.98
          
          -- Historical reaction analysis (AI-learned)
          expectedMove = price * (newsScore * 0.06) -- Dynamic target based on sentiment
          
      if highImpactNews && lowVolatility && flagBreak
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 60
               , signal_target = price + expectedMove
               , signal_stop = price * 0.98        -- 2% stop
               , signal_reason = "News momentum with flag breakout"
               }
        else Nothing 