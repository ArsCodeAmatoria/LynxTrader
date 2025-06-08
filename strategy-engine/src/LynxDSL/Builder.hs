{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : LynxDSL.Builder
-- Description : DSL builder functions for creating trading strategies
-- Copyright   : (c) LynxTrader Team, 2024
-- License     : MIT
-- Maintainer  : team@lynxtrader.com
-- Stability   : experimental

module LynxDSL.Builder
  ( -- * Strategy Construction
    strategy
  , withSymbols
  , withTimeFrame
  , withRisk
  , enable
  , disable
    -- * Rule Construction
  , rule
  , when_
  , then_
  , buyWhen
  , sellWhen
    -- * Condition Builders
  , price
  , indicator
  , rsi
  , sma
  , ema
  , macd
  , bollinger
  , (.>)
  , (.<)
  , (.>=)
  , (.<=)
  , (.==)
  , (./=)
  , (.&&)
  , (.||)
  , crossesAbove
  , crossesBelow
    -- * Action Builders
  , buy
  , sell
  , market
  , limit
  , stop
  , shares
  , dollars
  , percent
  , riskBased
    -- * Risk Management
  , withStopLoss
  , withTakeProfit
  , withMaxRisk
  , withMaxPositions
    -- * Example Strategies
  , simpleMomentumStrategy
  , meanReversionStrategy
  , breakoutStrategy
  ) where

import LynxDSL.Types
import Data.Text (Text)
import qualified Data.Text as T

-- | Strategy builder
strategy :: Text -> Text -> Strategy
strategy name desc = Strategy
  { strategyName = name
  , strategyDescription = desc
  , strategySymbols = []
  , strategyTimeFrame = Minute5
  , strategyRules = []
  , strategyRiskParams = defaultRisk
  , strategyEnabled = True
  }

-- | Add symbols to strategy
withSymbols :: Strategy -> [Text] -> Strategy
withSymbols strat symbols = strat { strategySymbols = map Symbol symbols }

-- | Set timeframe for strategy
withTimeFrame :: Strategy -> TimeFrame -> Strategy
withTimeFrame strat tf = strat { strategyTimeFrame = tf }

-- | Set risk parameters for strategy
withRisk :: Strategy -> RiskParams -> Strategy
withRisk strat risk = strat { strategyRiskParams = risk }

-- | Enable strategy
enable :: Strategy -> Strategy
enable strat = strat { strategyEnabled = True }

-- | Disable strategy
disable :: Strategy -> Strategy
disable strat = strat { strategyEnabled = False }

-- | Add rule to strategy
rule :: Text -> Condition -> Action -> Strategy -> Strategy
rule name condition action strat = 
  let newRule = Rule name condition action True
      currentRules = strategyRules strat
  in strat { strategyRules = currentRules ++ [newRule] }

-- | Conditional rule builder (alias for rule)
when_ :: Condition -> Action -> Text -> Strategy -> Strategy
when_ condition action name strat = rule name condition action strat

-- | Action rule builder
then_ :: Action -> Condition -> Text -> Strategy -> Strategy
then_ action condition name strat = rule name condition action strat

-- | Buy when condition is met
buyWhen :: Condition -> PositionSize -> Text -> Strategy -> Strategy
buyWhen condition size name strat = 
  let action = buyMarket size defaultRisk
  in rule name condition action strat

-- | Sell when condition is met
sellWhen :: Condition -> PositionSize -> Text -> Strategy -> Strategy
sellWhen condition size name strat = 
  let action = sellMarket size defaultRisk
  in rule name condition action strat

-- | Price reference builder
price :: PriceRef -> PriceRef
price = id

-- | Generic indicator builder
indicator :: IndicatorConfig -> PriceRef
indicator = Indicator

-- | RSI indicator builder
rsi :: Int -> PriceRef
rsi period = Indicator (RSIConfig period)

-- | Simple moving average builder
sma :: Int -> PriceRef
sma period = Indicator (SMAConfig period)

-- | Exponential moving average builder
ema :: Int -> PriceRef
ema period = Indicator (EMAConfig period)

-- | MACD indicator builder
macd :: Int -> Int -> Int -> PriceRef
macd fast slow signal = Indicator (MACDConfig fast slow signal)

-- | Bollinger bands builder
bollinger :: Int -> Double -> PriceRef
bollinger period stdDev = Indicator (BollingerConfig period stdDev)

-- | Comparison operators as infix functions
(.>) :: PriceRef -> Double -> Condition
priceRef .> value = PriceCondition priceRef GreaterThan value

(.<) :: PriceRef -> Double -> Condition
priceRef .< value = PriceCondition priceRef LessThan value

(.>=) :: PriceRef -> Double -> Condition
priceRef .>= value = PriceCondition priceRef GreaterEqual value

(.<=) :: PriceRef -> Double -> Condition
priceRef .<= value = PriceCondition priceRef LessEqual value

(.==) :: PriceRef -> Double -> Condition
priceRef .== value = PriceCondition priceRef Equal value

(./=) :: PriceRef -> Double -> Condition
priceRef ./= value = PriceCondition priceRef NotEqual value

-- | Logical operators
(.&&) :: Condition -> Condition -> Condition
cond1 .&& cond2 = LogicalCondition And [cond1, cond2]

(.||) :: Condition -> Condition -> Condition
cond1 .|| cond2 = LogicalCondition Or [cond1, cond2]

-- | Crossover conditions
crossesAbove :: PriceRef -> PriceRef -> Condition
crossesAbove ref1 ref2 = CrossOver ref1 ref2

crossesBelow :: PriceRef -> PriceRef -> Condition
crossesBelow ref1 ref2 = CrossUnder ref1 ref2

-- | Action builders
buy :: PositionSize -> OrderType -> RiskParams -> Action
buy size orderType risk = Action Buy size orderType risk

sell :: PositionSize -> OrderType -> RiskParams -> Action
sell size orderType risk = Action Sell size orderType risk

-- | Order type builders
market :: OrderType
market = Market

limit :: Double -> OrderType
limit price = Limit price

stop :: Double -> OrderType
stop price = Stop price

-- | Position size builders
shares :: Double -> PositionSize
shares = Shares

dollars :: Double -> PositionSize
dollars = Dollars

percent :: Double -> PositionSize
percent = Percent

riskBased :: Double -> PositionSize
riskBased = RiskBased

-- | Risk management builders
withStopLoss :: Double -> RiskParams -> RiskParams
withStopLoss stopLoss risk = risk { stopLossPercent = Just stopLoss }

withTakeProfit :: Double -> RiskParams -> RiskParams
withTakeProfit takeProfit risk = risk { takeProfitPercent = Just takeProfit }

withMaxRisk :: Double -> RiskParams -> RiskParams
withMaxRisk maxRisk risk = risk { maxRiskPercent = maxRisk }

withMaxPositions :: Int -> RiskParams -> RiskParams
withMaxPositions maxPos risk = risk { maxPositions = Just maxPos }

-- Example Strategies

-- | Simple momentum strategy using RSI
simpleMomentumStrategy :: Strategy
simpleMomentumStrategy = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy "Simple Momentum" "Buy on RSI oversold, sell on RSI overbought") 
                                               ["AAPL", "MSFT", "GOOGL"]) 
                               Minute15) 
                             (withTakeProfit 4.0 (withStopLoss 2.0 defaultRisk))
      withBuy = buyWhen (rsi 14 .< 30) (percent 10) "RSI Oversold Buy" baseStrategy
      withSell = sellWhen (rsi 14 .> 70) (percent 10) "RSI Overbought Sell" withBuy
  in withSell

-- | Mean reversion strategy using Bollinger Bands
meanReversionStrategy :: Strategy
meanReversionStrategy = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy "Mean Reversion" "Buy at lower band, sell at upper band") 
                                               ["SPY", "QQQ"]) 
                               Hour1) 
                             conservativeRisk
      withBuy = buyWhen (price Close .< 150.0) (dollars 500) "Bollinger Lower Buy" baseStrategy
      withSell = sellWhen (price Close .> 155.0) (dollars 500) "Bollinger Upper Sell" withBuy
  in withSell

-- | Breakout strategy using moving average crossover
breakoutStrategy :: Strategy
breakoutStrategy = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy "Moving Average Breakout" "Buy when fast MA crosses above slow MA") 
                                               ["BTC-USD", "ETH-USD"]) 
                               Hour4) 
                             (withMaxPositions 2 aggressiveRisk)
      withBuyRule = rule "MA Crossover Buy" 
                         ((ema 12 `crossesAbove` ema 26) .&& (rsi 14 .> 50))
                         (buy (percent 15) market aggressiveRisk) 
                         baseStrategy
      withSellRule = rule "MA Crossover Sell"
                          ((ema 12 `crossesBelow` ema 26) .|| (rsi 14 .< 30))
                          (sell (percent 15) market aggressiveRisk) 
                          withBuyRule
  in withSellRule

-- | Combine multiple conditions with logical operators
combinedConditionExample :: Condition
combinedConditionExample = 
  (rsi 14 .< 30) .&& 
  (price Close .> 150.0) .&& 
  (ema 12 `crossesAbove` ema 26)

-- | Complex multi-rule strategy example
complexStrategy :: Strategy
complexStrategy = 
  let baseStrategy = withRisk (withTimeFrame (withSymbols (strategy "Complex Multi-Signal" "Advanced strategy with multiple conditions") 
                                               ["TSLA", "NVDA", "AMD"]) 
                               Minute30) 
                             (withMaxPositions 3 (withMaxRisk 2.5 (withTakeProfit 3.0 (withStopLoss 1.5 defaultRisk))))
      strongBuy = rule "Strong Buy Signal"
                       ((rsi 14 .< 25) .&& 
                        (price Close .> 200.0) .&&
                        (ema 12 `crossesAbove` ema 26))
                       (buy (riskBased 100) market defaultRisk) 
                       baseStrategy 
      momentumBuy = rule "Momentum Buy"
                        ((rsi 14 .> 60) .&& 
                         (price Close .> 150.0) .&&
                         (price Volume .> 100000.0))
                        (buy (percent 5) market conservativeRisk) 
                        strongBuy
      takeProfit = rule "Take Profit"
                         (rsi 14 .> 75)
                         (sell (percent 50) market defaultRisk) 
                         momentumBuy
      stopLoss = rule "Stop Loss"
                      ((rsi 14 .< 20) .|| 
                       (price Close .< 140.0))
                      (sell (percent 100) market defaultRisk) 
                      takeProfit
  in stopLoss

-- | Utility functions for common patterns

-- | Create a simple RSI-based strategy
rsiStrategy :: Text -> [Text] -> TimeFrame -> Double -> Double -> Strategy
rsiStrategy name symbols timeframe oversold overbought = 
  let baseStrategy = withTimeFrame (withSymbols (strategy name ("RSI strategy with " <> T.pack (show oversold) <> "/" <> T.pack (show overbought))) 
                                     symbols) 
                     timeframe
      withBuy = buyWhen (rsi 14 .< oversold) (percent 10) "RSI Buy" baseStrategy
      withSell = sellWhen (rsi 14 .> overbought) (percent 10) "RSI Sell" withBuy
  in withSell

-- | Create a moving average crossover strategy
maStrategy :: Text -> [Text] -> TimeFrame -> Int -> Int -> Strategy
maStrategy name symbols timeframe fastPeriod slowPeriod = 
  let baseStrategy = withTimeFrame (withSymbols (strategy name ("MA crossover " <> T.pack (show fastPeriod) <> "/" <> T.pack (show slowPeriod))) 
                                     symbols) 
                     timeframe
      withBuyRule = rule "MA Buy" 
                         (ema fastPeriod `crossesAbove` ema slowPeriod)
                         (buy (percent 20) market defaultRisk) 
                         baseStrategy
      withSellRule = rule "MA Sell"
                          (ema fastPeriod `crossesBelow` ema slowPeriod)
                          (sell (percent 20) market defaultRisk) 
                          withBuyRule
  in withSellRule 