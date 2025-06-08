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

module LynxTrader.LynxDSL
  ( -- * Core Strategy Types
    Strategy(..)
  , newStrategy
  , withSymbols
  , withTimeFrame  
  , withRisk
  , addIndicator
  , setCondition
  , withTimeFilter
    -- * Strategy Execution
  , runStrategy
  , evaluateStrategy
    -- * Re-exports
  , module LynxTrader.Types
  ) where

import LynxTrader.Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Function ((&))

-- | Create a new strategy with default settings
newStrategy :: String -> Strategy
newStrategy name = Strategy
  { strategyName = name
  , strategySymbols = []
  , strategyTimeFrame = FiveMin
  , strategyRiskParams = RiskParams 1.0 2.0 100
  , strategyIndicators = []
  , strategyCondition = Nothing
  , strategyTimeFilter = Nothing
  , strategyEnabled = True
  }

-- | Add symbols to strategy
withSymbols :: Strategy -> [String] -> Strategy
withSymbols strategy symbols = strategy { strategySymbols = symbols }

-- | Set time frame for strategy
withTimeFrame :: Strategy -> TimeFrame -> Strategy
withTimeFrame strategy timeFrame = strategy { strategyTimeFrame = timeFrame }

-- | Set risk parameters
withRisk :: Strategy -> RiskParams -> Strategy
withRisk strategy riskParams = strategy { strategyRiskParams = riskParams }

-- | Add indicator to strategy
addIndicator :: Strategy -> Indicator -> Strategy
addIndicator strategy indicator = 
  strategy { strategyIndicators = indicator : strategyIndicators strategy }

-- | Set main condition for strategy
setCondition :: Strategy -> StrategyCondition -> Strategy
setCondition strategy condition = strategy { strategyCondition = Just condition }

-- | Set time filter for strategy
withTimeFilter :: Strategy -> TimeFilter -> Strategy
withTimeFilter strategy timeFilter = strategy { strategyTimeFilter = Just timeFilter }

-- | Execute strategy with market data
runStrategy :: Strategy -> MarketData -> Maybe Signal
runStrategy strategy marketData = 
  case strategyCondition strategy of
    Just condition -> condition (price marketData) (LynxTrader.Types.volume marketData) (indicators marketData)
    Nothing -> Nothing

-- | Evaluate strategy (alias for runStrategy)
evaluateStrategy :: Strategy -> MarketData -> Maybe Signal
evaluateStrategy = runStrategy

-- Mock indicator functions for compilation
vwap :: Int -> Indicator
vwap period = Indicator ("VWAP_" ++ show period) (fromIntegral period)

volume :: Int -> Indicator  
volume period = Indicator ("Volume_MA_" ++ show period) (fromIntegral period)

rsi :: Int -> Indicator
rsi period = Indicator ("RSI_" ++ show period) (fromIntegral period)

bollingerBands :: Int -> Double -> Indicator
bollingerBands period stdDev = Indicator ("BB_" ++ show period ++ "_" ++ show stdDev) (fromIntegral period)

fibonacciRetracement :: Int -> Indicator
fibonacciRetracement period = Indicator ("Fib_" ++ show period) (fromIntegral period)

openingRange :: Int -> Indicator
openingRange minutes = Indicator ("OR_" ++ show minutes) (fromIntegral minutes)

trendlineDetector :: Int -> Indicator
trendlineDetector period = Indicator ("Trendline_" ++ show period) (fromIntegral period)

newsImpactScore :: Int -> Indicator
newsImpactScore period = Indicator ("News_" ++ show period) (fromIntegral period)

volatilityFilter :: Int -> Indicator
volatilityFilter period = Indicator ("Volatility_" ++ show period) (fromIntegral period)

flagPattern :: Int -> Indicator
flagPattern period = Indicator ("Flag_" ++ show period) (fromIntegral period)

liquidityLevels :: Int -> Indicator
liquidityLevels period = Indicator ("Liquidity_" ++ show period) (fromIntegral period)

volumeProfile :: Int -> Indicator
volumeProfile period = Indicator ("VolumeProfile_" ++ show period) (fromIntegral period)

orderFlow :: Int -> Indicator
orderFlow period = Indicator ("OrderFlow_" ++ show period) (fromIntegral period)

orderBlockDetector :: Int -> Indicator
orderBlockDetector period = Indicator ("OrderBlock_" ++ show period) (fromIntegral period)

structureBreak :: Int -> Indicator
structureBreak period = Indicator ("StructureBreak_" ++ show period) (fromIntegral period)

volumeCluster :: Int -> Indicator
volumeCluster period = Indicator ("VolumeCluster_" ++ show period) (fromIntegral period)

fairValueGap :: Int -> Indicator
fairValueGap period = Indicator ("FVG_" ++ show period) (fromIntegral period)

gapFillConfirmation :: Int -> Indicator
gapFillConfirmation period = Indicator ("GapFill_" ++ show period) (fromIntegral period)

trendContinuation :: Int -> Indicator
trendContinuation period = Indicator ("TrendCont_" ++ show period) (fromIntegral period)

pointOfControl :: Int -> Indicator
pointOfControl period = Indicator ("POC_" ++ show period) (fromIntegral period)

valueAreaHigh :: Int -> Indicator
valueAreaHigh period = Indicator ("VAH_" ++ show period) (fromIntegral period)

valueAreaLow :: Int -> Indicator
valueAreaLow period = Indicator ("VAL_" ++ show period) (fromIntegral period)

-- Helper function to get indicator value
getIndicator :: String -> Map.Map String Double -> Double
getIndicator name indicatorMap = Map.findWithDefault 0.0 name indicatorMap 