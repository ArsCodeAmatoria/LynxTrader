{-# LANGUAGE OverloadedStrings #-}

module Main where

import LynxTrader.LynxDSL
import LynxTrader.Types
import LynxTrader.Strategies.ScalpingStrategies
import LynxTrader.Strategies.DayTradingStrategies
import LynxTrader.Strategies.SmartMoneyStrategies
import LynxTrader.RiskManagement.AIRiskOverlay
import qualified Data.Map as Map

main :: IO ()
main = do
  putStrLn "🦁 LynxTrader - Advanced Strategy Engine Demo"
  putStrLn "Agile. Smart. Precise."
  putStrLn "=========================================="
  
  -- Demonstrate Scalping Strategies
  putStrLn "\n⚡ SCALPING STRATEGIES (1-5min)"
  putStrLn "------------------------------"
  testScalpingStrategies
  
  -- Demonstrate Day Trading Strategies
  putStrLn "\n☀️ DAY TRADING STRATEGIES (5-15min)"
  putStrLn "-----------------------------------"
  testDayTradingStrategies
  
  -- Demonstrate Smart Money Strategies
  putStrLn "\n🌒 SMART MONEY STRATEGIES (Institutional)"
  putStrLn "----------------------------------------"
  testSmartMoneyStrategies
  
  -- Demonstrate AI Risk Management
  putStrLn "\n🧠 AI RISK MANAGEMENT OVERLAY"
  putStrLn "-----------------------------"
  testAIRiskManagement

testScalpingStrategies :: IO ()
testScalpingStrategies = do
  putStrLn "Testing VWAP Bounce Scalper..."
  let vwapSignal = runStrategy vwapBounceScalper mockMarketData
  case vwapSignal of
    Just signal -> putStrLn $ "✅ VWAP Signal: " ++ show signal
    Nothing -> putStrLn "❌ No VWAP signal generated"
  
  putStrLn "\nTesting Micro Breakout Trap..."
  let breakoutSignal = runStrategy microBreakoutTrap mockMarketData
  case breakoutSignal of
    Just signal -> putStrLn $ "✅ Breakout Signal: " ++ show signal
    Nothing -> putStrLn "❌ No breakout signal generated"
  
  putStrLn "\nTesting Fibonacci Reversal Scalper..."
  let fibSignal = runStrategy fibonacciReversalScalper mockMarketData
  case fibSignal of
    Just signal -> putStrLn $ "✅ Fibonacci Signal: " ++ show signal
    Nothing -> putStrLn "❌ No fibonacci signal generated"

testDayTradingStrategies :: IO ()
testDayTradingStrategies = do
  putStrLn "Testing Opening Range Breakout..."
  let orbSignal = runStrategy openingRangeBreakout mockMarketData
  case orbSignal of
    Just signal -> putStrLn $ "✅ ORB Signal: " ++ show signal
    Nothing -> putStrLn "❌ No ORB signal generated"
  
  putStrLn "\nTesting Trendline Break + Retest..."
  let trendlineSignal = runStrategy trendlineBreakRetest mockMarketData
  case trendlineSignal of
    Just signal -> putStrLn $ "✅ Trendline Signal: " ++ show signal
    Nothing -> putStrLn "❌ No trendline signal generated"
  
  putStrLn "\nTesting News Momentum Trade..."
  let newsSignal = runStrategy newsMomentumTrade mockMarketData
  case newsSignal of
    Just signal -> putStrLn $ "✅ News Signal: " ++ show signal
    Nothing -> putStrLn "❌ No news signal generated"

testSmartMoneyStrategies :: IO ()
testSmartMoneyStrategies = do
  putStrLn "Testing Liquidity Grab + Reversal..."
  let liquiditySignal = runStrategy liquidityGrabReversal mockMarketData
  case liquiditySignal of
    Just signal -> putStrLn $ "✅ Liquidity Signal: " ++ show signal
    Nothing -> putStrLn "❌ No liquidity signal generated"
  
  putStrLn "\nTesting Order Block Retest..."
  let orderBlockSignal = runStrategy orderBlockRetest mockMarketData
  case orderBlockSignal of
    Just signal -> putStrLn $ "✅ Order Block Signal: " ++ show signal
    Nothing -> putStrLn "❌ No order block signal generated"
  
  putStrLn "\nTesting Fair Value Gap Fill..."
  let fvgSignal = runStrategy fairValueGapFill mockMarketData
  case fvgSignal of
    Just signal -> putStrLn $ "✅ FVG Signal: " ++ show signal
    Nothing -> putStrLn "❌ No FVG signal generated"
  
  putStrLn "\nTesting Volume Profile Institutional..."
  let vpSignal = runStrategy volumeProfileInstitutional mockMarketData
  case vpSignal of
    Just signal -> putStrLn $ "✅ Volume Profile Signal: " ++ show signal
    Nothing -> putStrLn "❌ No volume profile signal generated"

testAIRiskManagement :: IO ()
testAIRiskManagement = do
  let -- Mock strategy performance data
      strategyMetrics = [
        StrategyMetrics "VWAP-Bounce-Scalper" 150 0.68 0.0125 0.08 1.85 
                       [0.012, 0.015, -0.008, 0.022, 0.009] 0.15 Map.empty,
        StrategyMetrics "Opening-Range-Breakout" 89 0.72 0.0185 0.12 2.1
                       [0.018, 0.025, 0.011, -0.015, 0.019] 0.22 Map.empty,
        StrategyMetrics "Liquidity-Grab-Reversal" 67 0.75 0.021 0.09 2.45
                       [0.025, 0.018, 0.032, -0.008, 0.015] 0.18 Map.empty
      ]
      
      initialAllocation = CapitalAllocation Map.empty 100000.0 0.05
      
  putStrLn "AI Position Sizing Analysis:"
  mapM_ (\metrics -> do
    let positionSize = calculateAIPositionSize defaultAIRiskConfig metrics 100000.0 150.0
    putStrLn $ "  " ++ strategyName metrics ++ ": " ++ show (round positionSize) ++ " shares"
    ) strategyMetrics
  
  putStrLn "\nCapital Allocation Optimization:"
  let newAllocation = rebalanceCapital strategyMetrics initialAllocation
  putStrLn $ "  Optimized Allocations: " ++ show (strategyAllocations newAllocation)
  
  putStrLn "\nAI Trailing Stop Example:"
  let trailingState = TrailingStopState 148.50 148.50 150.25 2.0 "Initial"
      updatedState = updateAITrailingStop trailingState 151.80 0.75 1250000
  putStrLn $ "  Updated Stop: $" ++ show (currentStop updatedState)
  putStrLn $ "  Pattern: " ++ patternRecognition updatedState

-- Mock market data for testing
mockMarketData :: MarketData
mockMarketData = MarketData
  { price = 150.25
  , volume = 1200000
  , timestamp = "2024-01-15T10:30:00Z"
  , indicators = Map.fromList 
      [ ("VWAP", 150.18)
      , ("Volume_MA", 980000)
      , ("BB_Upper", 151.50)
      , ("BB_Lower", 149.00)
      , ("RSI", 32.5)
      , ("Fib_0618", 150.22)
      , ("OR_High", 151.20)
      , ("OR_Low", 149.80)
      , ("Previous_Low", 149.75)
      , ("Liquidity_Zone", 150.00)
      , ("Volume_At_Price", 850000)
      , ("Order_Imbalance", 0.65)
      , ("OB_High", 150.80)
      , ("OB_Low", 150.20)
      , ("Structure_Break", 1.0)
      , ("Volume_Cluster", 1.8)
      , ("FVG_High", 150.60)
      , ("FVG_Low", 150.10)
      , ("Gap_Fill_Percent", 0.7)
      , ("Trend_Strength", 0.8)
      , ("Point_Of_Control", 150.25)
      , ("Value_Area_High", 150.95)
      , ("Value_Area_Low", 149.55)
      , ("High_Volume_Node", 1650000)
      ]
  }

-- Mock data types for compilation
data MarketData = MarketData
  { price :: Double
  , volume :: Double
  , timestamp :: String
  , indicators :: Map.Map String Double
  } deriving (Show)

data SignalType = BuySignal 
  { signal_price :: Double
  , signal_quantity :: Double  
  , signal_target :: Double
  , signal_stop :: Double
  , signal_reason :: String
  } | SellSignal
  { signal_price :: Double
  , signal_quantity :: Double
  , signal_target :: Double
  , signal_stop :: Double
  , signal_reason :: String
  } deriving (Show)

-- Simplified strategy runner for demo
runStrategy :: Strategy -> MarketData -> Maybe SignalType
runStrategy strategy marketData = 
  -- This would normally execute the strategy logic
  -- For demo purposes, we'll simulate signals
  case strategyName strategy of
    "VWAP-Bounce-Scalper" -> Just $ BuySignal 150.25 50 150.95 149.65 "VWAP bounce with volume spike"
    "Opening-Range-Breakout" -> Just $ BuySignal 151.25 100 153.50 149.80 "ORB bullish breakout with volume"
    "Liquidity-Grab-Reversal" -> Just $ BuySignal 150.05 70 152.30 149.50 "Liquidity grab with institutional reversal"
    _ -> Nothing

strategyName :: Strategy -> String
strategyName _ = "Mock-Strategy" -- Placeholder 