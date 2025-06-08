{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : LynxDSL.Indicators
-- Description : Technical indicators for trading strategies
-- Copyright   : (c) LynxTrader Team, 2024
-- License     : MIT
-- Maintainer  : team@lynxtrader.com
-- Stability   : experimental

module LynxDSL.Indicators 
  ( -- * Moving Averages
    sma
  , ema
  , wma
    -- * Momentum Indicators
  , rsi
  , stochastic
  , macd
  , macdHistogram
    -- * Volatility Indicators
  , bollingerBands
  , atr
  , standardDeviation
    -- * Volume Indicators
  , volumeAverage
  , onBalanceVolume
    -- * Support/Resistance
  , highestHigh
  , lowestLow
    -- * Utility Functions
  , calculateIndicator
  , getPriceData
  , crossOver
  , crossUnder
  ) where

import LynxDSL.Types
import Data.List (foldl', reverse, take, drop)

-- | Simple Moving Average
sma :: Int -> [Double] -> [Double]
sma period prices
  | period <= 0 = []
  | length prices < period = []
  | otherwise = go prices
  where
    go [] = []
    go ps
      | length ps < period = []
      | otherwise = 
          let avg = sum (take period ps) / fromIntegral period
          in avg : go (tail ps)

-- | Exponential Moving Average
ema :: Int -> [Double] -> [Double]
ema period prices
  | period <= 0 = []
  | null prices = []
  | otherwise = 
      let alpha = 2.0 / (fromIntegral period + 1.0)
          initialEma = head prices
      in reverse $ foldl' (\acc price -> 
           case acc of
             [] -> [price]
             (prevEma:_) -> (alpha * price + (1 - alpha) * prevEma) : acc
         ) [initialEma] (tail prices)

-- | Weighted Moving Average
wma :: Int -> [Double] -> [Double]
wma period prices
  | period <= 0 = []
  | length prices < period = []
  | otherwise = go prices
  where
    weights = map fromIntegral [1..period]
    weightSum = sum weights
    go [] = []
    go ps
      | length ps < period = []
      | otherwise = 
          let weightedSum = sum $ zipWith (*) weights (take period ps)
              avg = weightedSum / weightSum
          in avg : go (tail ps)

-- | Relative Strength Index
rsi :: Int -> [Double] -> [Double]
rsi period prices
  | period <= 0 = []
  | length prices < period + 1 = []
  | otherwise = 
      let changes = zipWith (-) (tail prices) prices
          gains = map (max 0) changes
          losses = map (abs . min 0) changes
          avgGains = sma period gains
          avgLosses = sma period losses
      in map (\(g, l) -> if l == 0 then 100 else 100 - (100 / (1 + g / l))) 
             (zip avgGains avgLosses)

-- | Stochastic Oscillator
stochastic :: Int -> Int -> [Double] -> [Double] -> [Double] -> [Double]
stochastic kPeriod dPeriod highs lows closes
  | kPeriod <= 0 || dPeriod <= 0 = []
  | length closes < kPeriod = []
  | otherwise = 
      let kValues = calculateK kPeriod highs lows closes
      in sma dPeriod kValues
  where
    calculateK :: Int -> [Double] -> [Double] -> [Double] -> [Double]
    calculateK k hs ls cs = go hs ls cs
      where
        go [] _ _ = []
        go _ [] _ = []
        go _ _ [] = []
        go (h:hs') (l:ls') (c:cs')
          | length (h:hs') < k || length (l:ls') < k || length (c:cs') < k = []
          | otherwise = 
              let highestH = maximum (take k (h:hs'))
                  lowestL = minimum (take k (l:ls'))
                  kValue = if highestH == lowestL then 50 
                          else 100 * (c - lowestL) / (highestH - lowestL)
              in kValue : go hs' ls' cs'

-- | MACD (Moving Average Convergence Divergence)
macd :: Int -> Int -> Int -> [Double] -> ([Double], [Double], [Double])
macd fastPeriod slowPeriod signalPeriod prices
  | fastPeriod <= 0 || slowPeriod <= 0 || signalPeriod <= 0 = ([], [], [])
  | fastPeriod >= slowPeriod = ([], [], [])
  | otherwise = 
      let fastEma = ema fastPeriod prices
          slowEma = ema slowPeriod prices
          macdLine = zipWith (-) fastEma slowEma
          signalLine = ema signalPeriod macdLine
          histogram = zipWith (-) macdLine signalLine
      in (macdLine, signalLine, histogram)

-- | MACD Histogram only
macdHistogram :: Int -> Int -> Int -> [Double] -> [Double]
macdHistogram fast slow signal prices = 
  let (_, _, hist) = macd fast slow signal prices
  in hist

-- | Bollinger Bands (Middle, Upper, Lower)
bollingerBands :: Int -> Double -> [Double] -> ([Double], [Double], [Double])
bollingerBands period stdDevMultiplier prices
  | period <= 0 = ([], [], [])
  | otherwise = 
      let middle = sma period prices
          stdDevs = standardDeviation period prices
          upper = zipWith (\m s -> m + stdDevMultiplier * s) middle stdDevs
          lower = zipWith (\m s -> m - stdDevMultiplier * s) middle stdDevs
      in (middle, upper, lower)

-- | Average True Range
atr :: Int -> [Double] -> [Double] -> [Double] -> [Double]
atr period highs lows closes
  | period <= 0 = []
  | length highs < 2 || length lows < 2 || length closes < 2 = []
  | otherwise = 
      let trueRanges = calculateTrueRanges highs lows closes
      in sma period trueRanges
  where
    calculateTrueRanges :: [Double] -> [Double] -> [Double] -> [Double]
    calculateTrueRanges (h1:h2:hs) (l1:l2:ls) (c1:c2:cs) = 
      let tr1 = h2 - l2
          tr2 = abs (h2 - c1)
          tr3 = abs (l2 - c1)
          tr = maximum [tr1, tr2, tr3]
      in tr : calculateTrueRanges (h2:hs) (l2:ls) (c2:cs)
    calculateTrueRanges _ _ _ = []

-- | Standard Deviation
standardDeviation :: Int -> [Double] -> [Double]
standardDeviation period prices
  | period <= 0 = []
  | length prices < period = []
  | otherwise = go prices
  where
    go [] = []
    go ps
      | length ps < period = []
      | otherwise = 
          let sample = take period ps
              mean = sum sample / fromIntegral period
              variance = sum (map (\x -> (x - mean) ** 2) sample) / fromIntegral period
              stdDev = sqrt variance
          in stdDev : go (tail ps)

-- | Volume Average
volumeAverage :: Int -> [Double] -> [Double]
volumeAverage = sma

-- | On Balance Volume
onBalanceVolume :: [Double] -> [Double] -> [Double]
onBalanceVolume prices volumes = 
  reverse $ foldl' (\acc (p1, p2, v) -> 
    let obv = case acc of
                [] -> v
                (prev:_) -> if p2 > p1 then prev + v
                           else if p2 < p1 then prev - v
                           else prev
    in obv : acc
  ) [] (zip3 prices (tail prices) (tail volumes))

-- | Highest High over period
highestHigh :: Int -> [Double] -> [Double]
highestHigh period prices
  | period <= 0 = []
  | length prices < period = []
  | otherwise = go prices
  where
    go [] = []
    go ps
      | length ps < period = []
      | otherwise = maximum (take period ps) : go (tail ps)

-- | Lowest Low over period
lowestLow :: Int -> [Double] -> [Double]
lowestLow period prices
  | period <= 0 = []
  | length prices < period = []
  | otherwise = go prices
  where
    go [] = []
    go ps
      | length ps < period = []
      | otherwise = minimum (take period ps) : go (tail ps)

-- | Calculate indicator based on configuration
calculateIndicator :: IndicatorConfig -> [Candle] -> [Double]
calculateIndicator config candles = 
  let closes = map candleClose candles
      highs = map candleHigh candles
      lows = map candleLow candles
      volumes = map candleVolume candles
  in case config of
       EMAConfig period -> ema period closes
       SMAConfig period -> sma period closes
       RSIConfig period -> rsi period closes
       MACDConfig fast slow signal -> 
         let (macdLine, _, _) = macd fast slow signal closes
         in macdLine
       BollingerConfig period stdDev -> 
         let (middle, _, _) = bollingerBands period stdDev closes
         in middle
       ATRConfig period -> atr period highs lows closes
       VolumeConfig period -> volumeAverage period volumes

-- | Extract price data from candles
getPriceData :: PriceRef -> [Candle] -> [Double]
getPriceData priceRef candles = 
  case priceRef of
    Open -> map candleOpen candles
    High -> map candleHigh candles
    Low -> map candleLow candles
    Close -> map candleClose candles
    Volume -> map candleVolume candles
    CurrentPrice -> map candleClose candles  -- Use close as current price
    Indicator config -> calculateIndicator config candles

-- | Check if first series crosses over second series
crossOver :: [Double] -> [Double] -> [Bool]
crossOver series1 series2 = 
  zipWith3 (\curr1 curr2 (prev1, prev2) -> 
    curr1 > curr2 && prev1 <= prev2
  ) series1 series2 (zip (drop 1 series1) (drop 1 series2))

-- | Check if first series crosses under second series  
crossUnder :: [Double] -> [Double] -> [Bool]
crossUnder series1 series2 = 
  zipWith3 (\curr1 curr2 (prev1, prev2) -> 
    curr1 < curr2 && prev1 >= prev2
  ) series1 series2 (zip (drop 1 series1) (drop 1 series2)) 