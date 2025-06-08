{-# LANGUAGE OverloadedStrings #-}

module LynxTrader.Strategies.SmartMoneyStrategies where

import LynxTrader.LynxDSL
import LynxTrader.Types

-- | Liquidity Grab + Reversal Strategy
-- Entry: Wick below previous low, then reclaim with volume
-- Concept: Hunt retail stop losses, then reverse
liquidityGrabReversal :: Strategy
liquidityGrabReversal = 
  newStrategy "Liquidity-Grab-Reversal"
    & withSymbols ["BTC-USD", "ETH-USD", "SPY", "EURUSD"]
    & withTimeFrame FifteenMin
    & withRisk (RiskParams 1.0 2.5 70)
    & addIndicator (liquidityLevels 100) -- Track key liquidity zones
    & addIndicator (volumeProfile 50)
    & addIndicator (orderFlow 20)
    & setCondition liquidityLogic
  where
    liquidityLogic price volume indicators = do
      let prevLow = getIndicator "Previous_Low" indicators
          liquidityLevel = getIndicator "Liquidity_Zone" indicators
          volumeAtLevel = getIndicator "Volume_At_Price" indicators
          orderImbalance = getIndicator "Order_Imbalance" indicators
          
          -- Liquidity grab conditions
          wickedBelow = price < prevLow * 0.999 -- Briefly broke below
          quickReclaim = price > prevLow * 1.001 -- Reclaimed level
          volumeSpike = volume > volumeAtLevel * 2.0
          bullishImbalance = orderImbalance > 0.6 -- More buy orders
          
          -- Fair value calculation
          fairValue = liquidityLevel * 1.015 -- Expected return to mid-range
      
      if wickedBelow && quickReclaim && volumeSpike && bullishImbalance
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 70
               , signal_target = fairValue
               , signal_stop = prevLow * 0.995     -- Below liquidity level
               , signal_reason = "Liquidity grab with institutional reversal"
               }
        else Nothing

-- | Order Block Retest Strategy
-- Entry: Strong bullish candle creates order block, price returns to test
-- Concept: Institutional orders left unfilled create support/resistance
orderBlockRetest :: Strategy
orderBlockRetest = 
  newStrategy "Order-Block-Retest"
    & withSymbols ["AAPL", "TSLA", "GOOGL", "AMZN", "NVDA"]
    & withTimeFrame TenMin
    & withRisk (RiskParams 1.2 2.4 85)
    & addIndicator (orderBlockDetector 30) -- Identify institutional blocks
    & addIndicator (structureBreak 20)
    & addIndicator (volumeCluster 25)
    & setCondition orderBlockLogic
  where
    orderBlockLogic price volume indicators = do
      let orderBlockHigh = getIndicator "OB_High" indicators
          orderBlockLow = getIndicator "OB_Low" indicators
          structureChange = getIndicator "Structure_Break" indicators
          institutionalVolume = getIndicator "Volume_Cluster" indicators
          
          -- Order block conditions
          withinBlock = price >= orderBlockLow && price <= orderBlockHigh
          structureBroken = structureChange > 0 -- Bullish structure break
          institutionalActivity = institutionalVolume > 1.5 -- Above average
          retestConfirmation = price > orderBlockLow * 1.002
          
          -- Risk calculation based on block size
          blockSize = orderBlockHigh - orderBlockLow
          targetDistance = blockSize * 2.0 -- 2x block size target
      
      if withinBlock && structureBroken && institutionalActivity && retestConfirmation
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 85
               , signal_target = price + targetDistance
               , signal_stop = orderBlockLow * 0.998
               , signal_reason = "Order block retest with structure break"
               }
        else Nothing

-- | Fair Value Gap (FVG) Fill Strategy
-- Entry: Price creates imbalance gap, then fills it with continuation
-- Concept: Market inefficiencies get filled before trend continuation
fairValueGapFill :: Strategy
fairValueGapFill = 
  newStrategy "Fair-Value-Gap-Fill"
    & withSymbols ["SPY", "QQQ", "BTC-USD", "EURUSD"]
    & withTimeFrame FiveMin
    & withRisk (RiskParams 0.8 2.0 90)
    & addIndicator (fairValueGap 40) -- Detect price imbalances
    & addIndicator (gapFillConfirmation 15)
    & addIndicator (trendContinuation 30)
    & setCondition fvgLogic
  where
    fvgLogic price volume indicators = do
      let gapHigh = getIndicator "FVG_High" indicators
          gapLow = getIndicator "FVG_Low" indicators
          gapFilled = getIndicator "Gap_Fill_Percent" indicators
          trendStrength = getIndicator "Trend_Strength" indicators
          
          -- FVG fill conditions
          withinGap = price >= gapLow && price <= gapHigh
          partialFill = gapFilled > 0.5 && gapFilled < 0.9 -- 50-90% filled
          findingSupport = price > gapLow * 1.001
          strongTrend = trendStrength > 0.7 -- Continuation likely
          
          -- Target: Return to trend continuation
          trendTarget = if trendStrength > 0
                       then price * 1.02 -- Bullish continuation
                       else price * 0.98 -- Bearish continuation
      
      if withinGap && partialFill && findingSupport && strongTrend
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 90
               , signal_target = trendTarget
               , signal_stop = gapLow * 0.996
               , signal_reason = "Fair value gap partial fill with trend continuation"
               }
        else Nothing

-- | Volume Profile Institutional Strategy  
-- Entry: Price interacts with high volume nodes (HVN) or low volume nodes (LVN)
-- Concept: Institutional activity creates key support/resistance levels
volumeProfileInstitutional :: Strategy
volumeProfileInstitutional = 
  newStrategy "Volume-Profile-Institutional"
    & withSymbols ["SPY", "QQQ", "AAPL", "TSLA", "NVDA"]
    & withTimeFrame ThirtyMin
    & withRisk (RiskParams 1.5 3.0 60)
    & addIndicator (volumeProfile 100)
    & addIndicator (pointOfControl 50) -- Highest volume price level
    & addIndicator (valueAreaHigh 50)
    & addIndicator (valueAreaLow 50)
    & setCondition volumeProfileLogic
  where
    volumeProfileLogic price volume indicators = do
      let poc = getIndicator "Point_Of_Control" indicators
          vah = getIndicator "Value_Area_High" indicators
          val = getIndicator "Value_Area_Low" indicators
          hvnLevel = getIndicator "High_Volume_Node" indicators
          
          -- Volume profile conditions
          nearPOC = abs (price - poc) < (price * 0.005) -- Within 0.5% of POC
          bounceOffVAL = price > val && price < (val * 1.01)
          rejectAtVAH = price < vah && price > (vah * 0.99)
          institutionalSupport = hvnLevel > volume * 1.5
          
      if nearPOC && bounceOffVAL && institutionalSupport
        then Just $ BuySignal
               { signal_price = price
               , signal_quantity = 60
               , signal_target = vah -- Target value area high
               , signal_stop = val * 0.995 -- Below value area low
               , signal_reason = "POC bounce with institutional volume support"
               }
        else if nearPOC && rejectAtVAH && institutionalSupport
        then Just $ SellSignal
               { signal_price = price
               , signal_quantity = 60
               , signal_target = val -- Target value area low
               , signal_stop = vah * 1.005 -- Above value area high
               , signal_reason = "VAH rejection with institutional volume"
               }
        else Nothing 