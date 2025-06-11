{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : LynxDSL
-- Description : Main module for the LynxTrader strategy DSL
-- Copyright   : (c) LynxTrader Team, 2024
-- License     : MIT
-- Maintainer  : team@lynxtrader.com
-- Stability   : experimental
--
-- This module provides a domain-specific language for creating algorithmic
-- trading strategies. It includes types for market data, indicators, conditions,
-- and actions, along with an evaluation engine and builder functions.
--
-- = Quick Start
--
-- > import LynxDSL
-- > 
-- > myStrategy = strategy "RSI Mean Reversion" "Buy oversold, sell overbought"
-- >   `withSymbols` ["AAPL", "MSFT"]
-- >   `withTimeFrame` Hour1
-- >   `buyWhen` (rsi 14 .< 30) (percent 10) "Oversold Buy"
-- >   `sellWhen` (rsi 14 .> 70) (percent 10) "Overbought Sell"
--
-- = Shorting Strategies
--
-- The DSL now includes comprehensive shorting strategies:
--
-- > shortStrategy = fakeBreakoutReversal "SPY Scalp" ["SPY", "QQQ"]
-- > dayShorts = dayTradingShortStrategy "Day Shorts" ["TSLA"] 150.0
-- > swingShorts = swingTradingShortStrategy "Swing Shorts" ["AAPL"] 180.0 185.0
--
-- = Strategy Evaluation
--
-- > signals <- evaluateStrategy myStrategy marketContext
-- > mapM_ processSignal signals
--
-- = Custom Indicators
--
-- The DSL supports common technical indicators:
--
-- * Moving averages: 'sma', 'ema'
-- * Momentum: 'rsi', 'macd'  
-- * Volatility: 'bollinger', 'atr'
-- * Volume: 'volumeAverage'
--
-- = Risk Management
--
-- Built-in risk management with position sizing and stop losses:
--
-- > conservativeRisk `withStopLoss` 1.0 `withTakeProfit` 2.0

module LynxDSL
  ( -- * Re-exports
    module LynxDSL.Types
  , module LynxDSL.Engine
  , module LynxDSL.Builder
  , module LynxDSL.ShortingStrategies
    -- * Indicator Calculations (non-conflicting)
  , bollingerBands
  , calculateIndicator
  , getPriceData
    -- * Common Patterns
  , quickStrategy
  , simpleCondition
  , basicRisk
    -- * Utilities
  , formatStrategy
  , validateStrategy
  , strategyStats
  ) where

import LynxDSL.Types
import LynxDSL.Engine
import LynxDSL.Builder
import LynxDSL.ShortingStrategies
import LynxDSL.Indicators (bollingerBands, calculateIndicator, getPriceData)

import Data.Text (Text)
import qualified Data.Text as T

-- | Quick strategy builder for simple cases
quickStrategy :: Text -> [Text] -> Condition -> Action -> Strategy
quickStrategy name symbols condition action = 
  rule "Main Rule" condition action (withSymbols (strategy name ("Quick strategy: " <> name)) symbols)

-- | Simple condition builder
simpleCondition :: PriceRef -> ComparisonOp -> Double -> Condition
simpleCondition priceRef op value = PriceCondition priceRef op value

-- | Basic risk parameters
basicRisk :: Double -> Double -> RiskParams
basicRisk stopLoss takeProfit = 
  withTakeProfit takeProfit (withStopLoss stopLoss defaultRisk)

-- | Format strategy for display
formatStrategy :: Strategy -> Text
formatStrategy strat = T.unlines
  [ "Strategy: " <> strategyName strat
  , "Description: " <> strategyDescription strat
  , "Symbols: " <> T.intercalate ", " (map (\(Symbol s) -> s) (strategySymbols strat))
  , "Time Frame: " <> T.pack (show (strategyTimeFrame strat))
  , "Rules: " <> T.pack (show (length (strategyRules strat)))
  , "Enabled: " <> T.pack (show (strategyEnabled strat))
  , "Risk - Stop Loss: " <> maybe "None" (T.pack . show) (stopLossPercent (strategyRiskParams strat))
  , "Risk - Take Profit: " <> maybe "None" (T.pack . show) (takeProfitPercent (strategyRiskParams strat))
  , "Risk - Max Risk: " <> T.pack (show (maxRiskPercent (strategyRiskParams strat))) <> "%"
  ]

-- | Validate strategy for common issues
validateStrategy :: Strategy -> [Text]
validateStrategy strat = 
  let issues = []
      issues' = if null (strategySymbols strat) 
                then "No symbols defined" : issues 
                else issues
      issues'' = if null (strategyRules strat)
                 then "No rules defined" : issues'
                 else issues'
      issues''' = if maxRiskPercent (strategyRiskParams strat) > 10
                  then "Max risk > 10% may be too aggressive" : issues''
                  else issues''
  in issues'''

-- | Get basic statistics about a strategy
strategyStats :: Strategy -> [(Text, Text)]
strategyStats strat = 
  [ ("Name", strategyName strat)
  , ("Symbols", T.pack . show . length $ strategySymbols strat)
  , ("Rules", T.pack . show . length $ strategyRules strat)
  , ("Enabled Rules", T.pack . show . length . filter ruleEnabled $ strategyRules strat)
  , ("Time Frame", T.pack . show $ strategyTimeFrame strat)
  , ("Max Risk", T.pack (show (maxRiskPercent (strategyRiskParams strat))) <> "%")
  , ("Status", if strategyEnabled strat then "Enabled" else "Disabled")
  ] 