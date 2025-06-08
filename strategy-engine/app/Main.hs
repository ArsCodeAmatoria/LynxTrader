{-# LANGUAGE OverloadedStrings #-}

-- |
-- Main application demonstrating the LynxDSL trading strategy language
module Main where

import LynxDSL
import Data.Time (getCurrentTime, UTCTime)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad (forM_)

main :: IO ()
main = do
  putStrLn "=== LynxTrader Strategy Engine Demo ==="
  putStrLn ""
  
  -- Demonstrate strategy creation
  putStrLn "1. Creating example strategies..."
  demonstrateStrategyCreation
  
  putStrLn "\n2. Evaluating strategies..."
  demonstrateStrategyEvaluaton
  
  putStrLn "\n3. Strategy validation..."
  demonstrateStrategyValidation
  
  putStrLn "\nDemo completed!"

-- | Demonstrate creating different types of strategies
demonstrateStrategyCreation :: IO ()
demonstrateStrategyCreation = do
  putStrLn "\n--- RSI Strategy ---"
  TIO.putStr $ formatStrategy simpleMomentumStrategy
  
  putStrLn "\n--- Mean Reversion Strategy ---"
  TIO.putStr $ formatStrategy meanReversionStrategy
  
  putStrLn "\n--- Breakout Strategy ---"
  TIO.putStr $ formatStrategy breakoutStrategy
  
  -- Create a custom strategy using the DSL
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy "Custom MACD" "MACD crossover with RSI filter") 
                                               ["BTC-USD", "ETH-USD", "ADA-USD"]) 
                               Hour1) 
                             (withMaxPositions 2 aggressiveRisk)
      bullish = rule "MACD Bullish" 
                     ((rsi 14 .> 50) .&& (price Close .> 150.0))
                     (buy (percent 20) market aggressiveRisk) 
                     baseStrategy
      customStrat = rule "MACD Bearish"
                          ((rsi 14 .< 30) .|| (price Close .< 140.0))
                          (sell (percent 20) market aggressiveRisk) 
                          bullish
  
  putStrLn "\n--- Custom MACD Strategy ---"
  TIO.putStr $ formatStrategy customStrat

-- | Demonstrate strategy evaluation (simplified)
demonstrateStrategyEvaluaton :: IO ()
demonstrateStrategyEvaluaton = do
  -- Create sample market context
  currentTime <- getCurrentTime
  let sampleCandles = createSampleCandles currentTime
      marketContext = MarketContext
        { contextCandles = sampleCandles
        , contextIndicators = 
            [ ("RSI_14", 25.0)  -- Oversold
            , ("SMA_20", 150.0)
            , ("SMA_50", 148.0)
            , ("EMA_12", 151.0)
            , ("EMA_26", 149.0)
            , ("ATR_14", 2.5)
            ]
        , contextPositions = []
        , contextCash = 10000.0
        , contextTime = currentTime
        }
  
  -- Evaluate the simple momentum strategy
  signals <- evaluateStrategy simpleMomentumStrategy marketContext
  
  putStrLn $ "Generated " ++ show (length signals) ++ " signals:"
  forM_ signals $ \signal -> do
    let Symbol symbol = signalSymbol signal
    putStrLn $ "  - " ++ T.unpack (signalRule signal) 
            ++ " for " ++ T.unpack symbol
            ++ " (strength: " ++ show (signalStrength signal) ++ ")"

-- | Demonstrate strategy validation
demonstrateStrategyValidation :: IO ()
demonstrateStrategyValidation = do
  -- Create a problematic strategy for validation
  let badStrategy = withRisk (strategy "Bad Strategy" "Strategy with issues") 
                             (withMaxRisk 15.0 defaultRisk)  -- Too high risk
        -- No symbols defined
        -- No rules defined
  
  let issues = validateStrategy badStrategy
  putStrLn $ "Validation issues found: " ++ show (length issues)
  forM_ issues $ \issue -> 
    putStrLn $ "  - " ++ T.unpack issue
  
  -- Show stats for good strategy
  putStrLn "\nStats for good strategy:"
  let stats = strategyStats simpleMomentumStrategy
  forM_ stats $ \(key, value) ->
    putStrLn $ "  " ++ T.unpack key ++ ": " ++ T.unpack value

-- | Create sample candle data for demonstration
createSampleCandles :: UTCTime -> [Candle]
createSampleCandles time = 
  [ Candle time 150.0 152.0 149.0 151.0 100000
  , Candle time 151.0 153.0 150.0 152.0 120000
  , Candle time 152.0 154.0 151.0 153.0 110000
  , Candle time 153.0 155.0 152.0 154.0 95000
  , Candle time 154.0 156.0 153.0 155.0 105000
  ]

-- | Example of creating strategies programmatically
createDynamicStrategy :: [T.Text] -> Double -> Double -> Strategy
createDynamicStrategy symbols oversold overbought = 
  let baseStrategy = withTimeFrame (withSymbols (strategy "Dynamic RSI" "Parameterized RSI strategy") 
                                     symbols) 
                     Minute15
      withBuy = buyWhen (rsi 14 .< oversold) (percent 8) "RSI Buy" baseStrategy
      withSell = sellWhen (rsi 14 .> overbought) (percent 8) "RSI Sell" withBuy
  in withSell

-- | Example of combining multiple strategies
combinedStrategy :: Strategy
combinedStrategy = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy "Combined Signals" "Multiple indicator confirmation") 
                                               ["SPY", "QQQ", "IWM"]) 
                               Hour1) 
                             (withTakeProfit 4.0 (withStopLoss 2.0 defaultRisk))
      bullRule = rule "Bull Confirmation"
                      ((rsi 14 .> 50) .&& 
                       (price Close .> 150.0) .&&
                       (ema 12 `crossesAbove` ema 26))
                      (buy (percent 15) market defaultRisk) 
                      baseStrategy
      bearRule = rule "Bear Confirmation"
                      ((rsi 14 .< 50) .&& 
                       (price Close .< 140.0) .&&
                       (ema 12 `crossesBelow` ema 26))
                      (sell (percent 15) market defaultRisk) 
                      bullRule
  in bearRule 