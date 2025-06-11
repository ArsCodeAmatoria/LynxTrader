'use client'

import React, { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { 
  BarChart3, 
  TrendingUp, 
  TrendingDown, 
  Target, 
  Clock, 
  AlertCircle, 
  CheckCircle, 
  RefreshCw,
  Trash2,
  Eye,
  Calendar,
  DollarSign,
  Activity,
  Zap
} from 'lucide-react'
import { motion, AnimatePresence } from 'framer-motion'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, BarChart, Bar } from 'recharts'

import { 
  BacktestResult,
  formatCurrency, 
  formatPercentage, 
  formatNumber,
  getStatusColor
} from '@/lib/api'

interface BacktestResultsProps {
  backtests: BacktestResult[]
  onDelete?: (backtestId: string) => void
  onRefresh?: () => void
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

const StatusIcon = ({ status }: { status: string }) => {
  switch (status) {
    case 'running':
    case 'starting':
      return <RefreshCw className="h-4 w-4 animate-spin" />
    case 'completed':
      return <CheckCircle className="h-4 w-4" />
    case 'error':
      return <AlertCircle className="h-4 w-4" />
    default:
      return <Clock className="h-4 w-4" />
  }
}

export default function BacktestResults({ backtests, onDelete, onRefresh }: BacktestResultsProps) {
  const [selectedBacktest, setSelectedBacktest] = useState<BacktestResult | null>(null)
  const [viewMode, setViewMode] = useState<'list' | 'details'>('list')

  const completedBacktests = backtests.filter(bt => bt.status === 'completed' && bt.results)
  const runningBacktests = backtests.filter(bt => bt.status === 'running' || bt.status === 'starting')
  const errorBacktests = backtests.filter(bt => bt.status === 'error')

  // Calculate summary metrics
  const totalBacktests = backtests.length
  const avgReturn = completedBacktests.length > 0 
    ? completedBacktests.reduce((sum, bt) => sum + (bt.results?.total_return || 0), 0) / completedBacktests.length
    : 0
  const avgWinRate = completedBacktests.length > 0
    ? completedBacktests.reduce((sum, bt) => sum + (bt.results?.win_rate || 0), 0) / completedBacktests.length
    : 0
  const bestReturn = Math.max(...completedBacktests.map(bt => bt.results?.total_return || 0), 0)

  // Chart data for performance overview
  const performanceData = completedBacktests.map((bt, index) => ({
    name: `Test ${index + 1}`,
    return: bt.results?.total_return || 0,
    winRate: bt.results?.win_rate || 0,
    trades: bt.results?.total_trades || 0
  }))

  const handleDeleteBacktest = (backtestId: string) => {
    if (selectedBacktest?.id === backtestId) {
      setSelectedBacktest(null)
    }
    onDelete?.(backtestId)
  }

  const formatDate = (dateString: string) => {
    return new Date(dateString).toLocaleDateString('en-US', {
      month: 'short',
      day: 'numeric',
      hour: '2-digit',
      minute: '2-digit'
    })
  }

  const formatDuration = (startTime: string, endTime?: string) => {
    if (!endTime) return 'Running...'
    
    const start = new Date(startTime)
    const end = new Date(endTime)
    const diffMs = end.getTime() - start.getTime()
    const diffSecs = Math.floor(diffMs / 1000)
    
    if (diffSecs < 60) return `${diffSecs}s`
    if (diffSecs < 3600) return `${Math.floor(diffSecs / 60)}m ${diffSecs % 60}s`
    return `${Math.floor(diffSecs / 3600)}h ${Math.floor((diffSecs % 3600) / 60)}m`
  }

  if (backtests.length === 0) {
    return (
      <GlowingCard>
        <CardContent className="p-12 text-center">
          <BarChart3 className="h-16 w-16 text-cyber-200/50 mx-auto mb-4" />
          <h3 className="text-xl font-semibold text-cyber-100 mb-2">No Backtests Yet</h3>
          <p className="text-cyber-200 mb-6">Start by running your first backtest to see results here.</p>
          <Button
            onClick={onRefresh}
            className="bg-neon-blue/20 text-neon-blue border-neon-blue/30"
          >
            <RefreshCw className="h-4 w-4 mr-2" />
            Refresh
          </Button>
        </CardContent>
      </GlowingCard>
    )
  }

  return (
    <div className="space-y-6">
      {/* Summary Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <GlowingCard glowColor="neon-blue">
          <CardContent className="p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-cyber-200 text-sm">Total Tests</p>
                <p className="text-2xl font-bold text-neon-blue">{totalBacktests}</p>
              </div>
              <BarChart3 className="h-8 w-8 text-neon-blue/60" />
            </div>
          </CardContent>
        </GlowingCard>

        <GlowingCard glowColor="matrix-100">
          <CardContent className="p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-cyber-200 text-sm">Avg Return</p>
                <p className="text-2xl font-bold text-matrix-100">{formatPercentage(avgReturn)}</p>
              </div>
              <TrendingUp className="h-8 w-8 text-matrix-100/60" />
            </div>
          </CardContent>
        </GlowingCard>

        <GlowingCard glowColor="neon-purple">
          <CardContent className="p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-cyber-200 text-sm">Avg Win Rate</p>
                <p className="text-2xl font-bold text-neon-purple">{formatPercentage(avgWinRate)}</p>
              </div>
              <Target className="h-8 w-8 text-neon-purple/60" />
            </div>
          </CardContent>
        </GlowingCard>

        <GlowingCard glowColor="cyber-accent">
          <CardContent className="p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-cyber-200 text-sm">Best Return</p>
                <p className="text-2xl font-bold text-cyber-accent">{formatPercentage(bestReturn)}</p>
              </div>
              <Zap className="h-8 w-8 text-cyber-accent/60" />
            </div>
          </CardContent>
        </GlowingCard>
      </div>

      {/* Performance Chart */}
      {completedBacktests.length > 0 && (
        <GlowingCard>
          <CardHeader>
            <CardTitle className="text-neon-blue">Performance Overview</CardTitle>
            <CardDescription className="text-cyber-200">
              Returns and win rates across all completed backtests
            </CardDescription>
          </CardHeader>
          <CardContent>
            <div className="h-64">
              <ResponsiveContainer width="100%" height="100%">
                <BarChart data={performanceData}>
                  <CartesianGrid strokeDasharray="3 3" stroke="#1a1a2e" />
                  <XAxis dataKey="name" stroke="#64748b" />
                  <YAxis stroke="#64748b" />
                  <Tooltip 
                    contentStyle={{ 
                      backgroundColor: '#0f0f23', 
                      border: '1px solid #2563eb', 
                      borderRadius: '8px' 
                    }}
                  />
                  <Bar dataKey="return" fill="#3b82f6" />
                </BarChart>
              </ResponsiveContainer>
            </div>
          </CardContent>
        </GlowingCard>
      )}

      {/* Results List */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Backtest List */}
        <GlowingCard>
          <CardHeader>
            <div className="flex items-center justify-between">
              <CardTitle className="text-matrix-100">Backtest History</CardTitle>
              <Button
                onClick={onRefresh}
                variant="outline"
                className="border-cyber-200/30 text-cyber-200"
              >
                <RefreshCw className="h-4 w-4" />
              </Button>
            </div>
          </CardHeader>
          <CardContent className="space-y-3 max-h-96 overflow-y-auto">
            <AnimatePresence>
              {backtests.map((backtest, index) => (
                <motion.div
                  key={backtest.id}
                  initial={{ opacity: 0, y: 20 }}
                  animate={{ opacity: 1, y: 0 }}
                  exit={{ opacity: 0, y: -20 }}
                  transition={{ delay: index * 0.1 }}
                  onClick={() => setSelectedBacktest(backtest)}
                  className={`p-4 rounded-lg border cursor-pointer transition-all ${
                    selectedBacktest?.id === backtest.id
                      ? 'border-neon-blue bg-neon-blue/10'
                      : 'border-cyber-200/30 hover:border-neon-blue/50'
                  }`}
                >
                  <div className="flex items-center justify-between mb-2">
                    <div className="flex items-center space-x-2">
                      <div className={getStatusColor(backtest.status)}>
                        <StatusIcon status={backtest.status} />
                      </div>
                      <span className="font-medium text-cyber-100">
                        {backtest.config.preset ? 
                          `${backtest.config.preset.charAt(0).toUpperCase()}${backtest.config.preset.slice(1)} Test` :
                          'Custom Test'
                        }
                      </span>
                    </div>
                    
                    <div className="flex items-center space-x-2">
                      <Badge className={`${getStatusColor(backtest.status)} border-current/30`}>
                        {backtest.status}
                      </Badge>
                      <Button
                        onClick={(e) => {
                          e.stopPropagation()
                          handleDeleteBacktest(backtest.id)
                        }}
                        variant="outline"
                        className="h-6 w-6 p-0 border-red-400/30 text-red-400 hover:bg-red-400/10"
                      >
                        <Trash2 className="h-3 w-3" />
                      </Button>
                    </div>
                  </div>
                  
                  <div className="flex items-center justify-between text-sm">
                    <div className="flex items-center text-cyber-200">
                      <Calendar className="h-3 w-3 mr-1" />
                      {formatDate(backtest.start_time)}
                    </div>
                    <div className="text-cyber-200">
                      {formatDuration(backtest.start_time, backtest.end_time)}
                    </div>
                  </div>
                  
                  {backtest.results && (
                    <div className="mt-2 grid grid-cols-3 gap-2 text-xs">
                      <div className="text-center p-1 bg-neon-blue/10 rounded">
                        <div className="text-neon-blue font-semibold">
                          {formatPercentage(backtest.results.total_return)}
                        </div>
                        <div className="text-cyber-200">Return</div>
                      </div>
                      <div className="text-center p-1 bg-matrix-100/10 rounded">
                        <div className="text-matrix-100 font-semibold">
                          {formatPercentage(backtest.results.win_rate)}
                        </div>
                        <div className="text-cyber-200">Win Rate</div>
                      </div>
                      <div className="text-center p-1 bg-cyber-accent/10 rounded">
                        <div className="text-cyber-accent font-semibold">
                          {backtest.results.total_trades}
                        </div>
                        <div className="text-cyber-200">Trades</div>
                      </div>
                    </div>
                  )}

                  {backtest.error && (
                    <div className="mt-2 text-xs text-red-400 bg-red-400/10 p-2 rounded">
                      {backtest.error}
                    </div>
                  )}
                </motion.div>
              ))}
            </AnimatePresence>
          </CardContent>
        </GlowingCard>

        {/* Detailed View */}
        <GlowingCard glowColor="neon-purple">
          <CardHeader>
            <CardTitle className="text-neon-purple">
              {selectedBacktest ? 'Backtest Details' : 'Select a Backtest'}
            </CardTitle>
          </CardHeader>
          <CardContent>
            {selectedBacktest ? (
              <div className="space-y-4">
                {/* Basic Info */}
                <div className="grid grid-cols-2 gap-4">
                  <div className="p-3 bg-neon-purple/10 border border-neon-purple/30 rounded-lg">
                    <div className="flex items-center text-neon-purple text-sm mb-1">
                      <Clock className="h-4 w-4 mr-1" />
                      Status
                    </div>
                    <div className="text-cyber-100 font-semibold capitalize">
                      {selectedBacktest.status}
                    </div>
                  </div>

                  <div className="p-3 bg-matrix-100/10 border border-matrix-100/30 rounded-lg">
                    <div className="flex items-center text-matrix-100 text-sm mb-1">
                      <Calendar className="h-4 w-4 mr-1" />
                      Duration
                    </div>
                    <div className="text-cyber-100 font-semibold">
                      {formatDuration(selectedBacktest.start_time, selectedBacktest.end_time)}
                    </div>
                  </div>
                </div>

                {/* Configuration */}
                <div className="p-3 bg-cyber-100/10 border border-cyber-200/30 rounded-lg">
                  <h4 className="text-cyber-200 text-sm mb-2">Configuration</h4>
                  <div className="space-y-2 text-sm">
                    {selectedBacktest.config.preset && (
                      <div>
                        <span className="text-cyber-200">Preset: </span>
                        <span className="text-cyber-100 capitalize">{selectedBacktest.config.preset}</span>
                      </div>
                    )}
                    {selectedBacktest.config.symbols && (
                      <div>
                        <span className="text-cyber-200">Symbols: </span>
                        <span className="text-cyber-100">{selectedBacktest.config.symbols.join(', ')}</span>
                      </div>
                    )}
                    {selectedBacktest.config.initial_capital && (
                      <div>
                        <span className="text-cyber-200">Capital: </span>
                        <span className="text-cyber-100">{formatCurrency(selectedBacktest.config.initial_capital)}</span>
                      </div>
                    )}
                    {selectedBacktest.config.timeframes && (
                      <div>
                        <span className="text-cyber-200">Timeframes: </span>
                        <span className="text-cyber-100">{selectedBacktest.config.timeframes.join(', ')}</span>
                      </div>
                    )}
                  </div>
                </div>

                {/* Results */}
                {selectedBacktest.results && (
                  <div className="space-y-3">
                    <h4 className="text-cyber-200 text-sm">Performance Metrics</h4>
                    
                    <div className="grid grid-cols-2 gap-3">
                      <div className="p-3 bg-neon-blue/10 border border-neon-blue/30 rounded-lg">
                        <div className="text-neon-blue text-sm">Total Return</div>
                        <div className="text-cyber-100 font-bold text-lg">
                          {formatPercentage(selectedBacktest.results.total_return)}
                        </div>
                      </div>

                      <div className="p-3 bg-matrix-100/10 border border-matrix-100/30 rounded-lg">
                        <div className="text-matrix-100 text-sm">Annual Return</div>
                        <div className="text-cyber-100 font-bold text-lg">
                          {formatPercentage(selectedBacktest.results.annual_return)}
                        </div>
                      </div>

                      <div className="p-3 bg-neon-purple/10 border border-neon-purple/30 rounded-lg">
                        <div className="text-neon-purple text-sm">Win Rate</div>
                        <div className="text-cyber-100 font-bold text-lg">
                          {formatPercentage(selectedBacktest.results.win_rate)}
                        </div>
                      </div>

                      <div className="p-3 bg-cyber-accent/10 border border-cyber-accent/30 rounded-lg">
                        <div className="text-cyber-accent text-sm">Max Drawdown</div>
                        <div className="text-cyber-100 font-bold text-lg">
                          {formatPercentage(selectedBacktest.results.max_drawdown)}
                        </div>
                      </div>
                    </div>

                    <div className="grid grid-cols-2 gap-3">
                      <div className="p-3 bg-cyan-500/10 border border-cyan-500/30 rounded-lg">
                        <div className="text-cyan-400 text-sm">Total Trades</div>
                        <div className="text-cyber-100 font-bold text-lg">
                          {selectedBacktest.results.total_trades}
                        </div>
                      </div>

                      <div className="p-3 bg-yellow-500/10 border border-yellow-500/30 rounded-lg">
                        <div className="text-yellow-400 text-sm">Execution Time</div>
                        <div className="text-cyber-100 font-bold text-lg">
                          {selectedBacktest.results.execution_time.toFixed(2)}s
                        </div>
                      </div>
                    </div>
                  </div>
                )}

                {selectedBacktest.error && (
                  <div className="p-3 bg-red-500/10 border border-red-500/30 rounded-lg">
                    <div className="text-red-400 text-sm mb-1">Error</div>
                    <div className="text-red-300 text-sm">
                      {selectedBacktest.error}
                    </div>
                  </div>
                )}
              </div>
            ) : (
              <div className="text-center py-12">
                <Eye className="h-16 w-16 text-cyber-200/50 mx-auto mb-4" />
                <p className="text-cyber-200">Select a backtest from the list to view details</p>
              </div>
            )}
          </CardContent>
        </GlowingCard>
      </div>
    </div>
  )
} 