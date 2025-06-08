{-# LANGUAGE OverloadedStrings #-}

module LynxTrader.Types where

import Data.Time (UTCTime, TimeOfDay)
import qualified Data.Map as Map

-- | Time frames for strategy execution
data TimeFrame 
  = OneMin | FiveMin | TenMin | FifteenMin | ThirtyMin 
  | Hour1 | Hour4 | Day1 | Week1
  deriving (Show, Eq, Ord)

-- | Risk management parameters
data RiskParams = RiskParams
  { stopLoss :: Double      -- Stop loss percentage
  , takeProfit :: Double    -- Take profit percentage  
  , positionSize :: Double  -- Position size
  } deriving (Show, Eq)

-- | Technical indicator
data Indicator = Indicator
  { indicatorName :: String
  , indicatorPeriod :: Double
  } deriving (Show, Eq)

-- | Time filter for strategy execution
data TimeFilter = TimeFilter
  { startTime :: TimeOfDay
  , endTime :: TimeOfDay
  } deriving (Show, Eq)

-- | Market data for strategy evaluation
data MarketData = MarketData
  { price :: Double
  , volume :: Double
  , timestamp :: String
  , indicators :: Map.Map String Double
  } deriving (Show)

-- | Trading signal types
data Signal = BuySignal 
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

-- | Strategy condition function type
type StrategyCondition = Double -> Double -> Map.Map String Double -> Maybe Signal

-- | Core strategy definition
data Strategy = Strategy
  { strategyName :: String
  , strategySymbols :: [String]
  , strategyTimeFrame :: TimeFrame
  , strategyRiskParams :: RiskParams
  , strategyIndicators :: [Indicator]
  , strategyCondition :: Maybe StrategyCondition
  , strategyTimeFilter :: Maybe TimeFilter
  , strategyEnabled :: Bool
  } 

instance Show Strategy where
  show s = "Strategy{name=" ++ strategyName s ++ 
           ", symbols=" ++ show (length (strategySymbols s)) ++
           ", timeFrame=" ++ show (strategyTimeFrame s) ++ "}"

-- | Default risk parameters
defaultRisk :: RiskParams
defaultRisk = RiskParams
  { stopLoss = 2.0
  , takeProfit = 4.0
  , positionSize = 100.0
  }

-- | Conservative risk parameters
conservativeRisk :: RiskParams
conservativeRisk = RiskParams
  { stopLoss = 1.0
  , takeProfit = 2.0
  , positionSize = 50.0
  }

-- | Aggressive risk parameters
aggressiveRisk :: RiskParams
aggressiveRisk = RiskParams
  { stopLoss = 3.0
  , takeProfit = 6.0
  , positionSize = 200.0
  } 