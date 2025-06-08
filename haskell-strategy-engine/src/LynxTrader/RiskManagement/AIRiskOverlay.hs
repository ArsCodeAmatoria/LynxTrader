{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module LynxTrader.RiskManagement.AIRiskOverlay where

import LynxTrader.Types
import Data.Time
import qualified Data.Map as Map

-- | AI-Enhanced Risk Management Configuration
data AIRiskConfig = AIRiskConfig
  { maxDailyLoss :: Double          -- Maximum daily loss percentage (5%)
  , maxDrawdown :: Double           -- Maximum portfolio drawdown (15%)
  , adaptivePositionSizing :: Bool  -- Enable AI position sizing
  , trailingStopAI :: Bool         -- Enable AI trailing stops
  , capitalRotation :: Bool        -- Enable capital rotation to best strategies
  , correlationAdjustment :: Bool  -- Adjust for position correlation
  } deriving (Show, Eq)

-- | Strategy Performance Metrics for AI Learning
data StrategyMetrics = StrategyMetrics
  { strategyName :: String
  , totalTrades :: Int
  , winRate :: Double
  , avgReturn :: Double
  , maxDrawdownStrat :: Double
  , sharpeRatio :: Double
  , recentPerformance :: [Double]   -- Last 20 trade returns
  , volatilityScore :: Double
  , correlationMatrix :: Map.Map String Double
  } deriving (Show, Eq)

-- | Dynamic Position Sizing based on AI analysis
calculateAIPositionSize :: AIRiskConfig -> StrategyMetrics -> Double -> Double -> Double
calculateAIPositionSize AIRiskConfig{..} StrategyMetrics{..} accountBalance currentPrice =
  let -- Base position size (Kelly Criterion enhanced)
      kellyFraction = (winRate * avgReturn - (1 - winRate)) / avgReturn
      kellyAdjusted = max 0.01 (min 0.25 kellyFraction) -- Cap between 1% and 25%
      
      -- AI Performance Adjustment
      performanceMultiplier = 
        if sharpeRatio > 2.0 then 1.2
        else if sharpeRatio > 1.0 then 1.0
        else if sharpeRatio > 0.5 then 0.8
        else 0.5
      
      -- Volatility Adjustment
      volatilityAdjustment = 1.0 / (1.0 + volatilityScore)
      
      -- Recent Performance Trend
      recentTrend = if length recentPerformance >= 5
                   then sum (take 5 recentPerformance) / 5.0
                   else 0.0
      trendMultiplier = if recentTrend > 0.02 then 1.1
                       else if recentTrend < -0.02 then 0.7
                       else 1.0
      
      -- Final position size calculation
      baseDollarAmount = accountBalance * kellyAdjusted
      adjustedAmount = baseDollarAmount * performanceMultiplier * volatilityAdjustment * trendMultiplier
      
  in adjustedAmount / currentPrice

-- | AI Trailing Stop Loss System
data TrailingStopState = TrailingStopState
  { initialStop :: Double
  , currentStop :: Double
  , highWaterMark :: Double
  , atrMultiplier :: Double
  , patternRecognition :: String    -- Detected reversal patterns
  } deriving (Show, Eq)

-- | Update trailing stop based on AI analysis
updateAITrailingStop :: TrailingStopState -> Double -> Double -> Double -> TrailingStopState
updateAITrailingStop state@TrailingStopState{..} currentPrice atr volume =
  let -- Dynamic ATR multiplier based on volatility regime
      volatilityRegime = if atr > (atr * 1.5) then "High" else if atr < (atr * 0.7) then "Low" else "Normal"
      dynamicMultiplier = case volatilityRegime of
        "High" -> atrMultiplier * 1.3  -- Wider stops in high volatility
        "Low"  -> atrMultiplier * 0.8  -- Tighter stops in low volatility  
        _      -> atrMultiplier
      
      -- Update high water mark
      newHighWaterMark = max highWaterMark currentPrice
      
      -- Calculate new trailing stop
      trailingDistance = atr * dynamicMultiplier
      potentialNewStop = newHighWaterMark - trailingDistance
      
      -- AI Pattern Recognition (simplified - would use ML model)
      reversalPattern = detectReversalPattern currentPrice volume atr
      patternAdjustment = if reversalPattern then 0.5 else 1.0
      
      finalNewStop = potentialNewStop * patternAdjustment
      
  in state { currentStop = max currentStop finalNewStop
           , highWaterMark = newHighWaterMark
           , patternRecognition = if reversalPattern then "Reversal Detected" else "Trend Continuation"
           }

-- | Simple reversal pattern detection (placeholder for ML model)
detectReversalPattern :: Double -> Double -> Double -> Bool
detectReversalPattern price volume atr =
  -- Placeholder logic - in reality would use trained ML model
  volume > (volume * 1.8) && atr > (atr * 1.4)

-- | Capital Rotation System - Move funds to best performing strategies
data CapitalAllocation = CapitalAllocation
  { strategyAllocations :: Map.Map String Double  -- Strategy name -> allocation %
  , totalCapital :: Double
  , rebalanceThreshold :: Double                  -- Rebalance when allocation drifts > 5%
  } deriving (Show, Eq)

-- | AI-driven capital rotation based on strategy performance
rebalanceCapital :: [StrategyMetrics] -> CapitalAllocation -> CapitalAllocation
rebalanceCapital strategies allocation@CapitalAllocation{..} =
  let -- Calculate strategy scores (composite of multiple factors)
      strategyScores = map calculateStrategyScore strategies
      totalScore = sum strategyScores
      
      -- Calculate optimal allocations
      optimalAllocations = Map.fromList $ zipWith 
        (\StrategyMetrics{..} score -> (strategyName, score / totalScore))
        strategies strategyScores
      
      -- Apply minimum and maximum allocation constraints
      constrainedAllocations = Map.map (max 0.05 . min 0.40) optimalAllocations
      normalizedTotal = Map.foldr (+) 0 constrainedAllocations
      finalAllocations = Map.map (/ normalizedTotal) constrainedAllocations
      
  in allocation { strategyAllocations = finalAllocations }

-- | Calculate composite strategy score for capital allocation
calculateStrategyScore :: StrategyMetrics -> Double
calculateStrategyScore StrategyMetrics{..} =
  let -- Weighted scoring system
      winRateScore = winRate * 0.3
      returnScore = (max 0 avgReturn) * 0.25
      sharpeScore = (max 0 sharpeRatio / 3.0) * 0.2  -- Normalize Sharpe ratio
      drawdownScore = (max 0 (1.0 - maxDrawdownStrat)) * 0.15
      consistencyScore = calculateConsistency recentPerformance * 0.1
      
  in winRateScore + returnScore + sharpeScore + drawdownScore + consistencyScore

-- | Calculate consistency score from recent performance
calculateConsistency :: [Double] -> Double
calculateConsistency returns =
  if length returns < 3 then 0.5
  else let variance = calculateVariance returns
           consistency = 1.0 / (1.0 + variance)
       in max 0 (min 1 consistency)

-- | Calculate variance of returns
calculateVariance :: [Double] -> Double
calculateVariance xs =
  let n = fromIntegral (length xs)
      mean = sum xs / n
      squaredDiffs = map (\x -> (x - mean) ** 2) xs
  in sum squaredDiffs / n

-- | Risk Check - Prevent overexposure and correlations
riskCheck :: AIRiskConfig -> [Position] -> StrategyMetrics -> Bool
riskCheck AIRiskConfig{..} positions StrategyMetrics{..} =
  let -- Check daily loss limit
      dailyPnL = sum $ map positionPnL positions
      dailyLossOk = dailyPnL > (-maxDailyLoss)
      
      -- Check position correlation (simplified)
      correlationRisk = any (> 0.7) $ Map.elems correlationMatrix
      correlationOk = not correlationAdjustment || not correlationRisk
      
      -- Check maximum drawdown
      currentDrawdown = calculateCurrentDrawdown positions
      drawdownOk = currentDrawdown < maxDrawdown
      
  in dailyLossOk && correlationOk && drawdownOk

-- | Helper functions (placeholders)
data Position = Position 
  { positionSymbol :: String
  , positionSize :: Double
  , positionPrice :: Double
  , positionPnL :: Double
  } deriving (Show, Eq)

calculateCurrentDrawdown :: [Position] -> Double
calculateCurrentDrawdown positions = 
  let totalPnL = sum $ map positionPnL positions
  in if totalPnL < 0 then abs totalPnL else 0.0

-- | Default AI Risk Configuration
defaultAIRiskConfig :: AIRiskConfig
defaultAIRiskConfig = AIRiskConfig
  { maxDailyLoss = 0.05         -- 5% max daily loss
  , maxDrawdown = 0.15          -- 15% max drawdown
  , adaptivePositionSizing = True
  , trailingStopAI = True
  , capitalRotation = True
  , correlationAdjustment = True
  } 