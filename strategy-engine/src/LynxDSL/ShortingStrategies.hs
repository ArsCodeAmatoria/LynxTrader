{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : LynxDSL.ShortingStrategies
-- Description : Comprehensive shorting strategies for scalping, day trading, and swing trading
-- Copyright   : (c) LynxTrader Team, 2024
-- License     : MIT
-- Maintainer  : team@lynxtrader.com
-- Stability   : experimental
--
-- This module implements a curated collection of shorting strategies optimized for
-- different timeframes and market conditions, with smart money insights and AI enhancements.

module LynxDSL.ShortingStrategies
  ( -- * Scalping Shorts (1-5m candles)
    fakeBreakoutReversal
  , vwapSlap
  , tapeBlowoffTrap
    -- * Day Trading Shorts (5m-30m)
  , bearFlagBreakdown
  , trendlineRejectionShort
  , openingReversalTrap
    -- * Swing Trading Shorts (1H-1D)
  , lowerHighConfirmation
  , exhaustionGapShort
  , supplyZoneRejection
    -- * Smart Money Shorts
  , liquiditySweepAboveHigh
  , bearishOrderBlockRetest
  , equalHighLiquidityTrap
    -- * AI-Enhanced Shorting Conditions
  , liquidityGrabCondition
  , volumeDivergenceCondition
  , exhaustionPatternCondition
  , blowoffTopCondition
    -- * Risk Management for Shorts
  , shortingRisk
  , conservativeShortRisk
  , aggressiveShortRisk
    -- * Helper Functions
  , vwap
  , highOfDay
  , lowOfDay
  , openingPrice
  , supplyZone
  , demandZone
  , orderBlock
  , fairValueGap
    -- * Complete Strategy Builders
  , scalpingShortStrategy
  , dayTradingShortStrategy
  , swingTradingShortStrategy
  ) where

import LynxDSL.Types
import LynxDSL.Builder
import Data.Text (Text)
import qualified Data.Text as T

-- | VWAP indicator reference
vwap :: PriceRef
vwap = Indicator (SMAConfig 20) -- Simplified VWAP approximation

-- | High of day reference
highOfDay :: PriceRef
highOfDay = High

-- | Low of day reference
lowOfDay :: PriceRef
lowOfDay = Low

-- | Opening price reference
openingPrice :: PriceRef
openingPrice = Open

-- | Supply zone (resistance) level
supplyZone :: Double -> PriceRef -> Condition
supplyZone level priceRef = priceRef .>= level

-- | Demand zone (support) level
demandZone :: Double -> PriceRef -> Condition
demandZone level priceRef = priceRef .<=  level

-- | Order block condition (large candle rejection area)
orderBlock :: PriceRef -> PriceRef -> Condition
orderBlock high low = (price Close .> high) .&& (price Close .< low)

-- | Fair Value Gap condition
fairValueGap :: Double -> Double -> Condition
fairValueGap gapHigh gapLow = (price Close .> gapLow) .&& (price Close .< gapHigh)

-- AI-Enhanced Conditions

-- | Liquidity grab condition - detects fake breakouts above highs
liquidityGrabCondition :: Double -> Condition
liquidityGrabCondition dayHigh = 
  (price High .> dayHigh) .&& 
  (price Volume .< 50000) .&& 
  (price Close .< dayHigh)

-- | Volume divergence condition - price moves without volume confirmation
volumeDivergenceCondition :: Condition
volumeDivergenceCondition = 
  (price High .> 200.0) .&& 
  (price Volume .< (Indicator (VolumeConfig 20)))

-- | Exhaustion pattern condition - identifies tired bulls
exhaustionPatternCondition :: Condition
exhaustionPatternCondition = 
  (rsi 14 .> 70) .&& 
  (price Close .< price Open) .&& 
  (price Volume .> (Indicator (VolumeConfig 10)))

-- | Blowoff top condition - parabolic move exhaustion
blowoffTopCondition :: Condition
blowoffTopCondition = 
  (rsi 14 .> 80) .&& 
  (price Close .> (ema 20 * 1.1)) .&& 
  (price Volume .> (Indicator (VolumeConfig 20) * 2.0))

-- Risk Management Profiles

-- | Conservative shorting risk parameters
conservativeShortRisk :: RiskParams
conservativeShortRisk = RiskParams
  { stopLossPercent = Just 1.0    -- Tight stop above trap wick
  , takeProfitPercent = Just 2.0  -- Conservative target
  , maxRiskPercent = 1.0          -- Low risk per trade
  , maxPositions = Just 2         -- Limited exposure
  }

-- | Standard shorting risk parameters
shortingRisk :: RiskParams
shortingRisk = RiskParams
  { stopLossPercent = Just 2.0    -- Standard stop above key resistance
  , takeProfitPercent = Just 4.0  -- 2:1 risk/reward
  , maxRiskPercent = 2.0          -- Moderate risk
  , maxPositions = Just 3         -- Balanced exposure
  }

-- | Aggressive shorting risk parameters
aggressiveShortRisk :: RiskParams
aggressiveShortRisk = RiskParams
  { stopLossPercent = Just 3.0    -- Wider stop for volatile moves
  , takeProfitPercent = Just 6.0  -- Higher targets
  , maxRiskPercent = 3.0          -- Higher risk tolerance
  , maxPositions = Just 5         -- More positions
  }

-- SCALPING SHORTS (1-5m candles)

-- | 1. Fake Breakout Reversal
-- Setup: Price breaks new intraday high on low volume
-- Entry: Wick above high + reversal candle (inverted hammer)
-- Exit: VWAP or previous low
fakeBreakoutReversal :: Text -> [Text] -> Strategy
fakeBreakoutReversal name symbols = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Fake breakout reversal on low volume") symbols) Minute5) conservativeShortRisk
      condition = liquidityGrabCondition 200.0 .&& (price Close .< highOfDay)
      action = sell (percent 5) market conservativeShortRisk
  in rule "Fake Breakout Short" condition action baseStrategy

-- | 2. VWAP Slap
-- Setup: Price spikes into VWAP from below
-- Entry: Rejection candle + volume confirmation
-- Exit: Return to previous low
vwapSlap :: Text -> [Text] -> Strategy
vwapSlap name symbols = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "VWAP rejection short") symbols) Minute5) shortingRisk
      condition = (price Close .> vwap) .&& 
                  (price High .> vwap * 1.005) .&& 
                  (price Close .< price Open) .&&
                  volumeDivergenceCondition
      action = sell (percent 8) market shortingRisk
  in rule "VWAP Slap Short" condition action baseStrategy

-- | 3. Tape Blowoff Trap
-- Setup: Large green candle with high volume = final push
-- Entry: Short first lower high after candle
-- Exit: 0.5–1% scalp
tapeBlowoffTrap :: Text -> [Text] -> Strategy
tapeBlowoffTrap name symbols = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Blowoff top tape trap") symbols) Minute1) aggressiveShortRisk
      condition = blowoffTopCondition .&& 
                  (price Close .< price High * 0.998) -- First lower high
      action = sell (percent 10) market aggressiveShortRisk
  in rule "Blowoff Trap Short" condition action baseStrategy

-- DAY TRADING SHORTS (5m-30m)

-- | 4. Bear Flag Breakdown
-- Setup: Sharp drop → weak rising wedge
-- Entry: Break of lower trendline
-- Exit: Measured move (size of flagpole)
bearFlagBreakdown :: Text -> [Text] -> Double -> Strategy
bearFlagBreakdown name symbols trendlineLevel = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Bear flag breakdown pattern") symbols) Minute15) shortingRisk
      condition = (price Close .< trendlineLevel) .&& 
                  (rsi 14 .< 50) .&& 
                  (price Volume .> (Indicator (VolumeConfig 20)))
      action = sell (percent 15) market shortingRisk
  in rule "Bear Flag Breakdown" condition action baseStrategy

-- | 5. Trendline Rejection Short
-- Setup: Touches downward trendline (drawn from swing highs)
-- Entry: Wick rejection + bearish engulfing
-- Exit: Next demand zone or ATR x 1.5
trendlineRejectionShort :: Text -> [Text] -> Double -> Strategy
trendlineRejectionShort name symbols trendlineLevel = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Downward trendline rejection") symbols) Minute15) shortingRisk
      condition = (price High .>= trendlineLevel) .&& 
                  (price Close .< price Open) .&& 
                  (price Close .< trendlineLevel * 0.995)
      action = sell (percent 12) market shortingRisk
  in rule "Trendline Rejection Short" condition action baseStrategy

-- | 6. Opening Reversal Trap
-- Setup: Gap up + early rally fails
-- Entry: Break below opening price (after failed high)
-- Exit: VWAP or low of day
openingReversalTrap :: Text -> [Text] -> Strategy
openingReversalTrap name symbols = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Opening gap reversal trap") symbols) Minute30) shortingRisk
      condition = (price Close .< openingPrice) .&& 
                  (price High .> openingPrice * 1.02) .&& 
                  (rsi 14 .< 45)
      action = sell (percent 10) market shortingRisk
  in rule "Opening Reversal Trap" condition action baseStrategy

-- SWING TRADING SHORTS (1H-1D)

-- | 7. Lower High Confirmation
-- Setup: After breakdown, price retests broken support
-- Entry: Bearish candle rejection at old support
-- Exit: New low or Fib extension (1.272)
lowerHighConfirmation :: Text -> [Text] -> Double -> Strategy
lowerHighConfirmation name symbols oldSupport = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Lower high confirmation pattern") symbols) Hour1) shortingRisk
      condition = (price High .>= oldSupport * 0.98) .&& 
                  (price Close .< oldSupport) .&& 
                  (price Close .< price Open)
      action = sell (percent 20) market shortingRisk
  in rule "Lower High Confirmation" condition action baseStrategy

-- | 8. Exhaustion Gap Short
-- Setup: Multi-day runner gaps up >10%, open rejection
-- Entry: First red daily candle post-gap
-- Exit: Return to previous base
exhaustionGapShort :: Text -> [Text] -> Strategy
exhaustionGapShort name symbols = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Exhaustion gap reversal") symbols) Day1) aggressiveShortRisk
      condition = (price Open .> price Close * 1.1) .&& 
                  (price Close .< price Open) .&& 
                  exhaustionPatternCondition
      action = sell (percent 25) market aggressiveShortRisk
  in rule "Exhaustion Gap Short" condition action baseStrategy

-- | 9. Supply Zone Rejection
-- Setup: Price enters historical sell zone (identified by AI or manual draw)
-- Entry: Pin bar/inverted hammer rejection
-- Exit: Mid-channel or demand block
supplyZoneRejection :: Text -> [Text] -> Double -> Double -> Strategy
supplyZoneRejection name symbols supplyHigh supplyLow = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Supply zone rejection pattern") symbols) Hour4) shortingRisk
      condition = supplyZone supplyHigh (price High) .&& 
                  (price Close .< supplyLow) .&& 
                  (price Close .< price Open)
      action = sell (percent 18) market shortingRisk
  in rule "Supply Zone Rejection" condition action baseStrategy

-- SMART MONEY SHORTS

-- | 10. Liquidity Sweep Above High
-- Setup: Price makes new high just to trap longs
-- Entry: Reclaims old high with big wick + volume divergence
-- Exit: Fair value gap or VWAP
liquiditySweepAboveHigh :: Text -> [Text] -> Double -> Strategy
liquiditySweepAboveHigh name symbols oldHigh = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Liquidity sweep trap") symbols) Minute15) shortingRisk
      condition = liquidityGrabCondition oldHigh .&& 
                  volumeDivergenceCondition .&& 
                  (price Close .< oldHigh)
      action = sell (percent 15) market shortingRisk
  in rule "Liquidity Sweep Short" condition action baseStrategy

-- | 11. Bearish Order Block Retest
-- Setup: Large bearish candle → price returns to top of candle
-- Entry: Rejects top of order block
-- Exit: Mid-structure or last support zone
bearishOrderBlockRetest :: Text -> [Text] -> Double -> Double -> Strategy
bearishOrderBlockRetest name symbols blockHigh blockLow = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Bearish order block retest") symbols) Minute30) shortingRisk
      condition = orderBlock (price High) (price Low * blockHigh) .&& 
                  (price Close .< blockHigh * 0.995) .&& 
                  (price Close .< price Open)
      action = sell (percent 12) market shortingRisk
  in rule "Order Block Retest Short" condition action baseStrategy

-- | 12. Equal High Liquidity Trap
-- Setup: Multiple highs form same level → false breakout above
-- Entry: Breaks above → fails → engulfing candle
-- Exit: Drop to last demand or measured leg
equalHighLiquidityTrap :: Text -> [Text] -> Double -> Strategy
equalHighLiquidityTrap name symbols equalHighLevel = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Equal highs liquidity trap") symbols) Hour1) shortingRisk
      condition = (price High .> equalHighLevel) .&& 
                  (price Close .< equalHighLevel) .&& 
                  volumeDivergenceCondition .&& 
                  (price Close .< price Open * 0.99) -- Engulfing pattern
      action = sell (percent 20) market shortingRisk
  in rule "Equal High Trap Short" condition action baseStrategy

-- COMPLETE STRATEGY BUILDERS

-- | Comprehensive scalping short strategy combining multiple patterns
scalpingShortStrategy :: Text -> [Text] -> Strategy
scalpingShortStrategy name symbols = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Comprehensive scalping shorts") symbols) Minute5) conservativeShortRisk
      
      -- Fake breakout condition
      fakeBreakout = liquidityGrabCondition 200.0 .&& (price Close .< highOfDay)
      
      -- VWAP rejection condition
      vwapReject = (price Close .> vwap) .&& (price Close .< price Open) .&& volumeDivergenceCondition
      
      -- Blowoff condition
      blowoff = blowoffTopCondition .&& (price Close .< price High * 0.998)
      
      -- Combined scalping condition
      scalpCondition = fakeBreakout .|| vwapReject .|| blowoff
      
      action = sell (percent 5) market conservativeShortRisk
  in rule "Scalping Short Combo" scalpCondition action baseStrategy

-- | Comprehensive day trading short strategy
dayTradingShortStrategy :: Text -> [Text] -> Double -> Strategy
dayTradingShortStrategy name symbols trendlineLevel = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Comprehensive day trading shorts") symbols) Minute15) shortingRisk
      
      -- Bear flag breakdown
      bearFlag = (price Close .< trendlineLevel) .&& (rsi 14 .< 50)
      
      -- Trendline rejection
      trendlineReject = (price High .>= trendlineLevel) .&& (price Close .< price Open)
      
      -- Opening reversal
      openingReversal = (price Close .< openingPrice) .&& (price High .> openingPrice * 1.02)
      
      -- Combined day trading condition
      dayTradingCondition = bearFlag .|| trendlineReject .|| openingReversal
      
      action = sell (percent 12) market shortingRisk
  in rule "Day Trading Short Combo" dayTradingCondition action baseStrategy

-- | Comprehensive swing trading short strategy
swingTradingShortStrategy :: Text -> [Text] -> Double -> Double -> Strategy
swingTradingShortStrategy name symbols oldSupport supplyLevel = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy name "Comprehensive swing trading shorts") symbols) Hour1) shortingRisk
      
      -- Lower high confirmation
      lowerHigh = (price High .>= oldSupport * 0.98) .&& (price Close .< oldSupport)
      
      -- Supply zone rejection
      supplyReject = supplyZone supplyLevel (price High) .&& (price Close .< price Open)
      
      -- Exhaustion pattern
      exhaustion = exhaustionPatternCondition .&& (rsi 14 .> 75)
      
      -- Combined swing condition
      swingCondition = lowerHigh .|| supplyReject .|| exhaustion
      
      action = sell (percent 20) market shortingRisk
  in rule "Swing Trading Short Combo" swingCondition action baseStrategy 