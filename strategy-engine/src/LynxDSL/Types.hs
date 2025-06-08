{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : LynxDSL.Types
-- Description : Core types for the LynxTrader strategy DSL
-- Copyright   : (c) LynxTrader Team, 2024
-- License     : MIT
-- Maintainer  : team@lynxtrader.com
-- Stability   : experimental

module LynxDSL.Types where

import Data.Time (UTCTime)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Market data price point
data Candle = Candle
  { candleTime   :: !UTCTime
  , candleOpen   :: !Double
  , candleHigh   :: !Double
  , candleLow    :: !Double
  , candleClose  :: !Double
  , candleVolume :: !Double
  } deriving (Show, Eq, Generic)

-- | Trading symbol
newtype Symbol = Symbol Text
  deriving (Show, Eq, Ord, Generic)

-- | Time frame for analysis
data TimeFrame 
  = Minute1
  | Minute5
  | Minute15
  | Minute30
  | Hour1
  | Hour4
  | Day1
  | Week1
  deriving (Show, Eq, Ord, Generic)

-- | Trade direction
data Direction 
  = Buy 
  | Sell
  deriving (Show, Eq, Generic)

-- | Order types
data OrderType
  = Market
  | Limit !Double
  | Stop !Double
  | StopLimit !Double !Double  -- stop price, limit price
  deriving (Show, Eq, Generic)

-- | Position size specification
data PositionSize
  = Shares !Double           -- Fixed number of shares
  | Dollars !Double          -- Dollar amount
  | Percent !Double          -- Percentage of portfolio
  | RiskBased !Double        -- Based on risk amount
  deriving (Show, Eq, Generic)

-- | Risk management parameters
data RiskParams = RiskParams
  { stopLossPercent   :: !(Maybe Double)
  , takeProfitPercent :: !(Maybe Double)
  , maxRiskPercent    :: !Double        -- Max risk per trade
  , maxPositions      :: !(Maybe Int)       -- Max concurrent positions
  } deriving (Show, Eq, Generic)

-- | Technical indicator configuration
data IndicatorConfig
  = EMAConfig !Int                    -- period
  | SMAConfig !Int                    -- period  
  | RSIConfig !Int                    -- period
  | MACDConfig !Int !Int !Int         -- fast, slow, signal
  | BollingerConfig !Int !Double      -- period, std dev
  | ATRConfig !Int                    -- period
  | VolumeConfig !Int                 -- period for volume average
  deriving (Show, Eq, Generic)

-- | Comparison operators for conditions
data ComparisonOp
  = GreaterThan
  | LessThan
  | GreaterEqual
  | LessEqual
  | Equal
  | NotEqual
  deriving (Show, Eq, Generic)

-- | Logical operators
data LogicalOp
  = And
  | Or
  | Not
  deriving (Show, Eq, Generic)

-- | Price reference points
data PriceRef
  = CurrentPrice
  | Open
  | High
  | Low
  | Close
  | Volume
  | Indicator !IndicatorConfig
  deriving (Show, Eq, Generic)

-- | Trading condition
data Condition
  = PriceCondition !PriceRef !ComparisonOp !Double
  | IndicatorCondition !IndicatorConfig !ComparisonOp !Double
  | CrossOver !PriceRef !PriceRef        -- First crosses above second
  | CrossUnder !PriceRef !PriceRef       -- First crosses under second
  | LogicalCondition !LogicalOp ![Condition]
  | Always                               -- Always true
  | Never                                -- Never true
  deriving (Show, Eq, Generic)

-- | Trade action
data Action = Action
  { actionDirection :: !Direction
  , actionSize      :: !PositionSize
  , actionOrderType :: !OrderType
  , actionRisk      :: !RiskParams
  } deriving (Show, Eq, Generic)

-- | Strategy rule - condition triggers action
data Rule = Rule
  { ruleName      :: !Text
  , ruleCondition :: !Condition
  , ruleAction    :: !Action
  , ruleEnabled   :: !Bool
  } deriving (Show, Eq, Generic)

-- | Complete trading strategy
data Strategy = Strategy
  { strategyName        :: !Text
  , strategyDescription :: !Text
  , strategySymbols     :: ![Symbol]
  , strategyTimeFrame   :: !TimeFrame
  , strategyRules       :: ![Rule]
  , strategyRiskParams  :: !RiskParams
  , strategyEnabled     :: !Bool
  } deriving (Show, Eq, Generic)

-- | Market context for strategy evaluation
data MarketContext = MarketContext
  { contextCandles    :: ![Candle]        -- Recent price history
  , contextIndicators :: ![(Text, Double)] -- Calculated indicators
  , contextPositions  :: ![(Symbol, Double)] -- Current positions
  , contextCash       :: !Double          -- Available cash
  , contextTime       :: !UTCTime         -- Current time
  } deriving (Show, Eq, Generic)

-- | Strategy evaluation result
data Signal = Signal
  { signalRule      :: !Text            -- Which rule triggered
  , signalAction    :: !Action          -- What action to take
  , signalSymbol    :: !Symbol          -- For which symbol
  , signalTime      :: !UTCTime         -- When the signal occurred
  , signalStrength  :: !Double          -- Signal strength 0-1
  , signalMetadata  :: ![(Text, Double)] -- Additional data
  } deriving (Show, Eq, Generic)

-- | Strategy execution state
data StrategyState = StrategyState
  { stateStrategy   :: !Strategy
  , stateContext    :: !MarketContext
  , stateSignals    :: ![Signal]
  , stateLastUpdate :: !UTCTime
  } deriving (Show, Eq, Generic)

-- | Backtest result
data BacktestResult = BacktestResult
  { backtestStrategy    :: !Text
  , backtestStartTime   :: !UTCTime
  , backtestEndTime     :: !UTCTime
  , backtestTotalReturn :: !Double
  , backtestMaxDrawdown :: !Double
  , backtestWinRate     :: !Double
  , backtestTotalTrades :: !Int
  , backtestSharpeRatio :: !Double
  , backtestTrades      :: ![TradeResult]
  } deriving (Show, Eq, Generic)

-- | Individual trade result
data TradeResult = TradeResult
  { tradeSymbol     :: !Symbol
  , tradeDirection  :: !Direction
  , tradeEntryTime  :: !UTCTime
  , tradeExitTime   :: !(Maybe UTCTime)
  , tradeEntryPrice :: !Double
  , tradeExitPrice  :: !(Maybe Double)
  , tradeQuantity   :: !Double
  , tradePnL        :: !(Maybe Double)
  , tradeRule       :: !Text
  } deriving (Show, Eq, Generic)

-- | Helper functions for common operations

-- | Create a simple buy market order
buyMarket :: PositionSize -> RiskParams -> Action
buyMarket size risk = Action Buy size Market risk

-- | Create a simple sell market order  
sellMarket :: PositionSize -> RiskParams -> Action
sellMarket size risk = Action Sell size Market risk

-- | Create a limit buy order
buyLimit :: Double -> PositionSize -> RiskParams -> Action
buyLimit price size risk = Action Buy size (Limit price) risk

-- | Create a limit sell order
sellLimit :: Double -> PositionSize -> RiskParams -> Action
sellLimit price size risk = Action Sell size (Limit price) risk

-- | Default risk parameters
defaultRisk :: RiskParams
defaultRisk = RiskParams
  { stopLossPercent = Just 2.0
  , takeProfitPercent = Just 4.0
  , maxRiskPercent = 2.0
  , maxPositions = Just 5
  }

-- | Conservative risk parameters
conservativeRisk :: RiskParams
conservativeRisk = RiskParams
  { stopLossPercent = Just 1.0
  , takeProfitPercent = Just 2.0
  , maxRiskPercent = 1.0
  , maxPositions = Just 3
  }

-- | Aggressive risk parameters
aggressiveRisk :: RiskParams
aggressiveRisk = RiskParams
  { stopLossPercent = Just 3.0
  , takeProfitPercent = Just 6.0
  , maxRiskPercent = 3.0
  , maxPositions = Just 10
  } 