'use client'

import React, { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { 
  PlayCircle, 
  Settings, 
  Calendar, 
  DollarSign, 
  Target,
  Database,
  RefreshCw,
  AlertCircle,
  CheckCircle,
  Clock
} from 'lucide-react'
import { motion } from 'framer-motion'

import { 
  BacktestingAPI, 
  BacktestRequest, 
  BacktestResult, 
  BacktestPreset,
  formatCurrency
} from '@/lib/api'

interface BacktestRunnerProps {
  onBacktestStart?: (result: BacktestResult) => void
  onBacktestComplete?: (result: BacktestResult) => void
}

const GlowingCard = ({ children, className = "", glowColor = "neon-blue" }: { 
  children: React.ReactNode
  className?: string
  glowColor?: string 
}) => (
  <motion.div
    initial={{ opacity: 0, scale: 0.95 }}
    animate={{ opacity: 1, scale: 1 }}
    transition={{ duration: 0.3 }}
    className={`relative ${className}`}
  >
    <div className={`absolute inset-0 bg-gradient-to-r from-${glowColor}/20 to-transparent rounded-lg blur-sm animate-glow`} />
    <div className="relative bg-cyber-100/80 backdrop-blur-sm border border-neon-blue/30 rounded-lg">
      {children}
    </div>
  </motion.div>
)

export default function BacktestRunner({ onBacktestStart, onBacktestComplete }: BacktestRunnerProps) {
  const [presets, setPresets] = useState<BacktestPreset[]>([])
  const [selectedPreset, setSelectedPreset] = useState<string | null>(null)
  const [customConfig, setCustomConfig] = useState<BacktestRequest>({
    symbols: ['AAPL', 'GOOGL'],
    initial_capital: 100000,
    data_source: 'yfinance',
    timeframes: ['1h', '1d']
  })
  const [isRunning, setIsRunning] = useState(false)
  const [currentBacktest, setCurrentBacktest] = useState<string | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [useCustom, setUseCustom] = useState(false)

  useEffect(() => {
    loadPresets()
  }, [])

  useEffect(() => {
    let pollInterval: NodeJS.Timeout | null = null
    
    if (currentBacktest) {
      pollInterval = setInterval(async () => {
        try {
          const result = await BacktestingAPI.getBacktestStatus(currentBacktest)
          
          if (result.status === 'completed' || result.status === 'error') {
            setIsRunning(false)
            setCurrentBacktest(null)
            onBacktestComplete?.(result)
            
            if (pollInterval) {
              clearInterval(pollInterval)
            }
          }
        } catch (err) {
          console.error('Error polling backtest status:', err)
        }
      }, 2000)
    }

    return () => {
      if (pollInterval) {
        clearInterval(pollInterval)
      }
    }
  }, [currentBacktest, onBacktestComplete])

  const loadPresets = async () => {
    try {
      const data = await BacktestingAPI.getPresets()
      setPresets(data.presets)
      if (data.presets.length > 0) {
        setSelectedPreset(data.presets[0].name)
      }
    } catch (err: any) {
      setError(err.message || 'Failed to load presets')
    }
  }

  const handleRunBacktest = async () => {
    try {
      setError(null)
      setIsRunning(true)

      const request: BacktestRequest = useCustom ? customConfig : { preset: selectedPreset || 'quick' }
      
      const response = await BacktestingAPI.startBacktest(request)
      setCurrentBacktest(response.backtest_id)
      
      // Create initial result for tracking
      const initialResult: BacktestResult = {
        id: response.backtest_id,
        status: 'starting',
        start_time: new Date().toISOString(),
        config: request
      }
      
      onBacktestStart?.(initialResult)
      
    } catch (err: any) {
      setError(err.message || 'Failed to start backtest')
      setIsRunning(false)
    }
  }

  const selectedPresetData = presets.find(p => p.name === selectedPreset)

  return (
    <div className="space-y-6">
      {/* Configuration Mode Toggle */}
      <div className="flex space-x-4">
        <Button
          onClick={() => setUseCustom(false)}
          variant={!useCustom ? "default" : "outline"}
          className={!useCustom ? "bg-neon-blue/20 text-neon-blue border-neon-blue" : "border-cyber-200/30 text-cyber-200"}
        >
          <Target className="h-4 w-4 mr-2" />
          Presets
        </Button>
        <Button
          onClick={() => setUseCustom(true)}
          variant={useCustom ? "default" : "outline"}
          className={useCustom ? "bg-neon-purple/20 text-neon-purple border-neon-purple" : "border-cyber-200/30 text-cyber-200"}
        >
          <Settings className="h-4 w-4 mr-2" />
          Custom
        </Button>
      </div>

      {error && (
        <motion.div
          initial={{ opacity: 0, y: -10 }}
          animate={{ opacity: 1, y: 0 }}
          className="bg-red-500/10 border border-red-500/30 rounded-lg p-4"
        >
          <div className="flex items-center">
            <AlertCircle className="h-5 w-5 text-red-400 mr-2" />
            <span className="text-red-400">{error}</span>
          </div>
        </motion.div>
      )}

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Configuration Panel */}
        <GlowingCard>
          <CardHeader>
            <CardTitle className="text-neon-blue">
              {useCustom ? 'Custom Configuration' : 'Preset Configuration'}
            </CardTitle>
            <CardDescription className="text-cyber-200">
              {useCustom ? 'Configure your custom backtest parameters' : 'Choose from pre-configured backtest scenarios'}
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            {!useCustom ? (
              /* Preset Selection */
              <div className="space-y-4">
                {presets.map((preset) => (
                  <motion.div
                    key={preset.name}
                    whileHover={{ scale: 1.02 }}
                    whileTap={{ scale: 0.98 }}
                    onClick={() => setSelectedPreset(preset.name)}
                    className={`p-4 rounded-lg border cursor-pointer transition-all ${
                      selectedPreset === preset.name
                        ? 'border-neon-blue bg-neon-blue/10'
                        : 'border-cyber-200/30 hover:border-neon-blue/50'
                    }`}
                  >
                    <div className="flex items-center justify-between mb-2">
                      <h3 className="font-semibold text-cyber-100 capitalize">{preset.name}</h3>
                      <Badge className="bg-neon-blue/20 text-neon-blue border-neon-blue/30">
                        {formatCurrency(preset.initial_capital)}
                      </Badge>
                    </div>
                    <p className="text-cyber-200 text-sm mb-3">{preset.description}</p>
                    <div className="flex flex-wrap gap-2">
                      <div className="flex items-center text-xs text-cyber-200">
                        <Calendar className="h-3 w-3 mr-1" />
                        {preset.duration_days} days
                      </div>
                      <div className="flex items-center text-xs text-cyber-200">
                        <Target className="h-3 w-3 mr-1" />
                        {preset.symbols.length} symbols
                      </div>
                    </div>
                  </motion.div>
                ))}
              </div>
            ) : (
              /* Custom Configuration */
              <div className="space-y-4">
                <div>
                  <label className="block text-cyber-200 text-sm mb-2">Symbols</label>
                  <input
                    type="text"
                    value={customConfig.symbols?.join(', ') || ''}
                    onChange={(e) => setCustomConfig({
                      ...customConfig,
                      symbols: e.target.value.split(',').map(s => s.trim()).filter(Boolean)
                    })}
                    className="w-full p-3 bg-cyber-100/20 border border-cyber-200/30 rounded-lg text-cyber-100 placeholder-cyber-200/50"
                    placeholder="AAPL, GOOGL, MSFT"
                  />
                </div>

                <div>
                  <label className="block text-cyber-200 text-sm mb-2">Initial Capital</label>
                  <input
                    type="number"
                    value={customConfig.initial_capital || 100000}
                    onChange={(e) => setCustomConfig({
                      ...customConfig,
                      initial_capital: Number(e.target.value)
                    })}
                    className="w-full p-3 bg-cyber-100/20 border border-cyber-200/30 rounded-lg text-cyber-100"
                  />
                </div>

                <div>
                  <label className="block text-cyber-200 text-sm mb-2">Timeframes</label>
                  <input
                    type="text"
                    value={customConfig.timeframes?.join(', ') || ''}
                    onChange={(e) => setCustomConfig({
                      ...customConfig,
                      timeframes: e.target.value.split(',').map(s => s.trim()).filter(Boolean)
                    })}
                    className="w-full p-3 bg-cyber-100/20 border border-cyber-200/30 rounded-lg text-cyber-100 placeholder-cyber-200/50"
                    placeholder="1h, 1d"
                  />
                </div>

                <div>
                  <label className="block text-cyber-200 text-sm mb-2">Data Source</label>
                  <select
                    value={customConfig.data_source || 'yfinance'}
                    onChange={(e) => setCustomConfig({
                      ...customConfig,
                      data_source: e.target.value
                    })}
                    className="w-full p-3 bg-cyber-100/20 border border-cyber-200/30 rounded-lg text-cyber-100"
                  >
                    <option value="yfinance">Yahoo Finance</option>
                    <option value="alpaca">Alpaca Markets</option>
                    <option value="alpha_vantage">Alpha Vantage</option>
                  </select>
                </div>
              </div>
            )}
          </CardContent>
        </GlowingCard>

        {/* Preview & Controls */}
        <GlowingCard glowColor="matrix-100">
          <CardHeader>
            <CardTitle className="text-matrix-100">Backtest Preview</CardTitle>
            <CardDescription className="text-cyber-200">
              Review configuration before running
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            {selectedPresetData && !useCustom ? (
              <div className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div className="p-3 bg-neon-blue/10 border border-neon-blue/30 rounded-lg">
                    <div className="flex items-center text-neon-blue text-sm mb-1">
                      <DollarSign className="h-4 w-4 mr-1" />
                      Capital
                    </div>
                    <div className="text-cyber-100 font-semibold">
                      {formatCurrency(selectedPresetData.initial_capital)}
                    </div>
                  </div>

                  <div className="p-3 bg-matrix-100/10 border border-matrix-100/30 rounded-lg">
                    <div className="flex items-center text-matrix-100 text-sm mb-1">
                      <Calendar className="h-4 w-4 mr-1" />
                      Duration
                    </div>
                    <div className="text-cyber-100 font-semibold">
                      {selectedPresetData.duration_days} days
                    </div>
                  </div>
                </div>

                <div className="p-3 bg-cyber-100/10 border border-cyber-200/30 rounded-lg">
                  <div className="flex items-center text-cyber-200 text-sm mb-2">
                    <Target className="h-4 w-4 mr-1" />
                    Symbols ({selectedPresetData.symbols.length})
                  </div>
                  <div className="flex flex-wrap gap-1">
                    {selectedPresetData.symbols.map((symbol) => (
                      <Badge 
                        key={symbol} 
                        className="bg-cyber-accent/20 text-cyber-accent border-cyber-accent/30 text-xs"
                      >
                        {symbol}
                      </Badge>
                    ))}
                  </div>
                </div>
              </div>
            ) : useCustom ? (
              <div className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div className="p-3 bg-neon-purple/10 border border-neon-purple/30 rounded-lg">
                    <div className="flex items-center text-neon-purple text-sm mb-1">
                      <DollarSign className="h-4 w-4 mr-1" />
                      Capital
                    </div>
                    <div className="text-cyber-100 font-semibold">
                      {formatCurrency(customConfig.initial_capital || 100000)}
                    </div>
                  </div>

                  <div className="p-3 bg-cyber-accent/10 border border-cyber-accent/30 rounded-lg">
                    <div className="flex items-center text-cyber-accent text-sm mb-1">
                      <Database className="h-4 w-4 mr-1" />
                      Data Source
                    </div>
                    <div className="text-cyber-100 font-semibold capitalize">
                      {customConfig.data_source || 'yfinance'}
                    </div>
                  </div>
                </div>

                <div className="space-y-2">
                  <div className="flex items-center text-cyber-200 text-sm">
                    <Target className="h-4 w-4 mr-1" />
                    Symbols ({customConfig.symbols?.length || 0})
                  </div>
                  <div className="flex flex-wrap gap-1">
                    {customConfig.symbols?.map((symbol) => (
                      <Badge 
                        key={symbol} 
                        className="bg-neon-purple/20 text-neon-purple border-neon-purple/30 text-xs"
                      >
                        {symbol}
                      </Badge>
                    ))}
                  </div>
                </div>
              </div>
            ) : null}

            <div className="pt-4 border-t border-cyber-200/20">
              <Button
                onClick={handleRunBacktest}
                disabled={isRunning || (!selectedPreset && !useCustom)}
                className={`w-full ${
                  isRunning 
                    ? 'bg-yellow-500/20 text-yellow-400 border-yellow-500/30' 
                    : 'bg-neon-blue/20 text-neon-blue border-neon-blue/30 hover:bg-neon-blue/30'
                }`}
              >
                {isRunning ? (
                  <>
                    <RefreshCw className="h-4 w-4 mr-2 animate-spin" />
                    Running Backtest...
                  </>
                ) : (
                  <>
                    <PlayCircle className="h-4 w-4 mr-2" />
                    Start Backtest
                  </>
                )}
              </Button>
            </div>
          </CardContent>
        </GlowingCard>
      </div>
    </div>
  )
} 