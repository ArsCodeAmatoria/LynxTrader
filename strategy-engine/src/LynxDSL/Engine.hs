{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : LynxDSL.Engine
-- Description : Strategy evaluation engine for LynxTrader
-- Copyright   : (c) LynxTrader Team, 2024
-- License     : MIT
-- Maintainer  : team@lynxtrader.com
-- Stability   : experimental

module LynxDSL.Engine
  ( -- * Strategy Evaluation
    evaluateStrategy
  , evaluateRule
  , evaluateCondition
    -- * Signal Generation
  , generateSignals
  , filterSignals
  , prioritizeSignals
    -- * Market Context
  , updateMarketContext
  , calculateAllIndicators
    -- * Utilities
  , isValidSignal
  , signalToOrder
  , riskAdjustSize
  ) where

import LynxDSL.Types
import LynxDSL.Indicators
import Data.Time (UTCTime, getCurrentTime)
import Data.Text (Text)
import Data.List (sortBy, filter)
import Data.Ord (comparing, Down(..))

-- | Evaluate a complete strategy against market context
evaluateStrategy :: Strategy -> MarketContext -> IO [Signal]
evaluateStrategy strategy context = do
  currentTime <- getCurrentTime
  let enabledRules = filter ruleEnabled (strategyRules strategy)
      signals = concatMap (evaluateRuleWithContext strategy context currentTime) enabledRules
      validSignals = filter isValidSignal signals
      filteredSignals = filterSignals strategy validSignals
      prioritizedSignals = prioritizeSignals filteredSignals
  return prioritizedSignals

-- | Evaluate a single rule with context
evaluateRuleWithContext :: Strategy -> MarketContext -> UTCTime -> Rule -> [Signal]
evaluateRuleWithContext strategy context currentTime rule = 
  let symbols = strategySymbols strategy
      ruleResult = evaluateRule rule context
  in if ruleResult
     then map (createSignal rule currentTime) symbols
     else []
  where
    createSignal :: Rule -> UTCTime -> Symbol -> Signal
    createSignal r time symbol = Signal
      { signalRule = ruleName r
      , signalAction = ruleAction r
      , signalSymbol = symbol
      , signalTime = time
      , signalStrength = calculateSignalStrength r context
      , signalMetadata = extractMetadata r context
      }

-- | Evaluate a single rule against market context
evaluateRule :: Rule -> MarketContext -> Bool
evaluateRule rule context = 
  if ruleEnabled rule
  then evaluateCondition (ruleCondition rule) context
  else False

-- | Evaluate a trading condition
evaluateCondition :: Condition -> MarketContext -> Bool
evaluateCondition condition context = 
  case condition of
    Always -> True
    Never -> False
    
    PriceCondition priceRef op value -> 
      let prices = getPriceDataFromContext priceRef context
          currentPrice = if null prices then 0 else head prices
      in compareValues currentPrice op value
    
    IndicatorCondition indicatorConfig op value ->
      let indicators = calculateIndicatorFromContext indicatorConfig context
          currentValue = if null indicators then 0 else head indicators
      in compareValues currentValue op value
    
    CrossOver priceRef1 priceRef2 ->
      let series1 = getPriceDataFromContext priceRef1 context
          series2 = getPriceDataFromContext priceRef2 context
          crosses = crossOver series1 series2
      in not (null crosses) && head crosses
    
    CrossUnder priceRef1 priceRef2 ->
      let series1 = getPriceDataFromContext priceRef1 context
          series2 = getPriceDataFromContext priceRef2 context
          crosses = crossUnder series1 series2
      in not (null crosses) && head crosses
    
    LogicalCondition logicalOp conditions ->
      case logicalOp of
        And -> all (`evaluateCondition` context) conditions
        Or -> any (`evaluateCondition` context) conditions
        Not -> case conditions of
                 [singleCondition] -> not (evaluateCondition singleCondition context)
                 _ -> False  -- Invalid NOT condition

-- | Compare two values using comparison operator
compareValues :: Double -> ComparisonOp -> Double -> Bool
compareValues val1 op val2 = 
  case op of
    GreaterThan -> val1 > val2
    LessThan -> val1 < val2
    GreaterEqual -> val1 >= val2
    LessEqual -> val1 <= val2
    Equal -> val1 == val2
    NotEqual -> val1 /= val2

-- | Extract price data from market context
getPriceDataFromContext :: PriceRef -> MarketContext -> [Double]
getPriceDataFromContext priceRef context = 
  getPriceData priceRef (contextCandles context)

-- | Calculate indicator from market context
calculateIndicatorFromContext :: IndicatorConfig -> MarketContext -> [Double]
calculateIndicatorFromContext config context = 
  calculateIndicator config (contextCandles context)

-- | Generate trading signals from strategy evaluation
generateSignals :: [Strategy] -> MarketContext -> IO [Signal]
generateSignals strategies context = do
  allSignals <- mapM (`evaluateStrategy` context) strategies
  return $ concat allSignals

-- | Filter signals based on risk management and portfolio constraints
filterSignals :: Strategy -> [Signal] -> [Signal]
filterSignals strategy signals = 
  let riskParams = strategyRiskParams strategy
  in filter (passesRiskFilter riskParams) signals

-- | Check if signal passes risk management filters
passesRiskFilter :: RiskParams -> Signal -> Bool
passesRiskFilter riskParams signal = 
  let action = signalAction signal
      risk = actionRisk action
  in maxRiskPercent risk <= maxRiskPercent riskParams

-- | Prioritize signals by strength and other factors
prioritizeSignals :: [Signal] -> [Signal]
prioritizeSignals = sortBy (comparing (Down . signalStrength))

-- | Update market context with new data
updateMarketContext :: [Candle] -> MarketContext -> MarketContext
updateMarketContext newCandles oldContext = 
  let updatedCandles = take 1000 (newCandles ++ contextCandles oldContext)  -- Keep last 1000 candles
  in oldContext 
     { contextCandles = updatedCandles
     , contextIndicators = calculateAllIndicators updatedCandles
     , contextTime = if null newCandles then contextTime oldContext else candleTime (head newCandles)
     }

-- | Calculate all common indicators for context
calculateAllIndicators :: [Candle] -> [(Text, Double)]
calculateAllIndicators candles = 
  let closes = map candleClose candles
      highs = map candleHigh candles
      lows = map candleLow candles
      volumes = map candleVolume candles
  in [ ("SMA_20", safeHead $ sma 20 closes)
     , ("SMA_50", safeHead $ sma 50 closes)
     , ("EMA_12", safeHead $ ema 12 closes)
     , ("EMA_26", safeHead $ ema 26 closes)
     , ("RSI_14", safeHead $ rsi 14 closes)
     , ("ATR_14", safeHead $ atr 14 highs lows closes)
     ]
  where
    safeHead :: [Double] -> Double
    safeHead [] = 0
    safeHead (x:_) = x

-- | Calculate signal strength based on multiple factors
calculateSignalStrength :: Rule -> MarketContext -> Double
calculateSignalStrength rule context = 
  let baseStrength = 0.5  -- Default strength
      -- Add factors based on confluence of indicators
      rsiStrength = calculateRSIStrength context
      volumeStrength = calculateVolumeStrength context
      trendStrength = calculateTrendStrength context
  in min 1.0 $ max 0.0 $ baseStrength + rsiStrength + volumeStrength + trendStrength

-- | Calculate RSI-based signal strength adjustment
calculateRSIStrength :: MarketContext -> Double
calculateRSIStrength context = 
  case lookup "RSI_14" (contextIndicators context) of
    Just rsiValue 
      | rsiValue > 70 -> 0.2   -- Overbought - strong sell signal
      | rsiValue < 30 -> 0.2   -- Oversold - strong buy signal
      | otherwise -> 0.0
    Nothing -> 0.0

-- | Calculate volume-based signal strength adjustment
calculateVolumeStrength :: MarketContext -> Double
calculateVolumeStrength context = 
  let candles = contextCandles context
      volumes = map candleVolume candles
      avgVolume = if length volumes >= 20 
                  then sum (take 20 volumes) / 20
                  else 0
      currentVolume = if null volumes then 0 else head volumes
  in if avgVolume > 0 && currentVolume > avgVolume * 1.5
     then 0.1  -- Higher volume adds strength
     else 0.0

-- | Calculate trend-based signal strength adjustment
calculateTrendStrength :: MarketContext -> Double
calculateTrendStrength context = 
  case (lookup "SMA_20" (contextIndicators context), 
        lookup "SMA_50" (contextIndicators context)) of
    (Just sma20, Just sma50) 
      | sma20 > sma50 -> 0.1   -- Uptrend
      | sma20 < sma50 -> 0.1   -- Downtrend (for short signals)
      | otherwise -> 0.0
    _ -> 0.0

-- | Extract metadata for signal
extractMetadata :: Rule -> MarketContext -> [(Text, Double)]
extractMetadata rule context = 
  [ ("rsi", fromMaybe 0 $ lookup "RSI_14" (contextIndicators context))
  , ("atr", fromMaybe 0 $ lookup "ATR_14" (contextIndicators context))
  , ("volume", if null (contextCandles context) then 0 else candleVolume (head (contextCandles context)))
  ]
  where
    fromMaybe :: Double -> Maybe Double -> Double
    fromMaybe def Nothing = def
    fromMaybe _ (Just x) = x

-- | Check if a signal is valid for execution
isValidSignal :: Signal -> Bool
isValidSignal signal = 
  signalStrength signal > 0.3  -- Minimum strength threshold

-- | Convert signal to order for execution
signalToOrder :: Signal -> MarketContext -> Maybe (Symbol, Direction, PositionSize, OrderType)
signalToOrder signal context = 
  let action = signalAction signal
      adjustedSize = riskAdjustSize (actionSize action) (actionRisk action) context
  in Just ( signalSymbol signal
          , actionDirection action
          , adjustedSize
          , actionOrderType action
          )

-- | Adjust position size based on risk parameters and available capital
riskAdjustSize :: PositionSize -> RiskParams -> MarketContext -> PositionSize
riskAdjustSize originalSize riskParams context = 
  case originalSize of
    Shares shares -> Shares shares  -- Keep original for now
    Dollars dollars -> 
      let maxRisk = contextCash context * maxRiskPercent riskParams / 100
          adjustedDollars = min dollars maxRisk
      in Dollars adjustedDollars
    Percent percent -> 
      let maxPercent = min percent (maxRiskPercent riskParams)
      in Percent maxPercent
    RiskBased riskAmount -> 
      let maxRisk = contextCash context * maxRiskPercent riskParams / 100
          adjustedRisk = min riskAmount maxRisk
      in RiskBased adjustedRisk 