'use client'

import React, { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { 
  PlayCircle, 
  PauseCircle, 
  RefreshCw, 
  TrendingUp, 
  TrendingDown, 
  BarChart3, 
  TestTube,
  Target,
  Zap,
  Database,
  AlertCircle,
  CheckCircle,
  Clock,
  Trash2,
  Settings,
  Brain,
  Activity
} from 'lucide-react'
import { motion } from 'framer-motion'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, BarChart, Bar } from 'recharts'

import { 
  BacktestingAPI, 
  BacktestResult, 
  BacktestPreset, 
  Strategy,
  formatCurrency, 
  formatPercentage, 
  formatNumber,
  getStatusColor,
  getRiskProfileColor
} from '@/lib/api'

// Sub-components
import BacktestRunner from './BacktestRunner'
import BacktestResults from './BacktestResults'
import StrategyTester from './StrategyTester'

interface BacktestingProps {
  className?: string
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

export default function Backtesting({ className = "" }: BacktestingProps) {
  const [activeTab, setActiveTab] = useState("runner")
  const [backtests, setBacktests] = useState<BacktestResult[]>([])
  const [strategies, setStrategies] = useState<Strategy[]>([])
  const [loading, setLoading] = useState(true)
  const [error, setError] = useState<string | null>(null)
  const [apiHealth, setApiHealth] = useState<{ status: string; running_backtests: number; completed_backtests: number } | null>(null)

  // Load initial data
  useEffect(() => {
    loadData()
    
    // Set up polling for active backtests
    const pollInterval = setInterval(() => {
      if (backtests.some(bt => bt.status === 'running' || bt.status === 'starting')) {
        loadBacktests()
      }
    }, 2000)

    return () => clearInterval(pollInterval)
  }, [])

  const loadData = async () => {
    try {
      setLoading(true)
      setError(null)
      
      const [backtestsData, strategiesData, healthData] = await Promise.all([
        BacktestingAPI.listBacktests(),
        BacktestingAPI.getStrategies(),
        BacktestingAPI.getHealth()
      ])
      
      setBacktests(backtestsData.backtests)
      setStrategies(strategiesData.strategies)
      setApiHealth(healthData)
    } catch (err: any) {
      setError(err.message || 'Failed to load data')
      console.error('Error loading data:', err)
    } finally {
      setLoading(false)
    }
  }

  const loadBacktests = async () => {
    try {
      const data = await BacktestingAPI.listBacktests()
      setBacktests(data.backtests)
    } catch (err) {
      console.error('Error loading backtests:', err)
    }
  }

  const handleBacktestComplete = (result: BacktestResult) => {
    setBacktests(prev => {
      const index = prev.findIndex(bt => bt.id === result.id)
      if (index >= 0) {
        const updated = [...prev]
        updated[index] = result
        return updated
      }
      return [result, ...prev]
    })
  }

  const handleDeleteBacktest = async (backtestId: string) => {
    try {
      await BacktestingAPI.deleteBacktest(backtestId)
      setBacktests(prev => prev.filter(bt => bt.id !== backtestId))
    } catch (err: any) {
      setError(err.message || 'Failed to delete backtest')
    }
  }

  // Get summary stats
  const runningBacktests = backtests.filter(bt => bt.status === 'running' || bt.status === 'starting').length
  const completedBacktests = backtests.filter(bt => bt.status === 'completed').length
  const errorBacktests = backtests.filter(bt => bt.status === 'error').length
  
  const avgReturn = backtests
    .filter(bt => bt.results?.total_return)
    .reduce((sum, bt) => sum + (bt.results?.total_return || 0), 0) / 
    Math.max(completedBacktests, 1)

  const avgWinRate = backtests
    .filter(bt => bt.results?.win_rate)
    .reduce((sum, bt) => sum + (bt.results?.win_rate || 0), 0) / 
    Math.max(completedBacktests, 1)

  if (loading) {
    return (
      <div className={`flex items-center justify-center h-96 ${className}`}>
        <motion.div
          animate={{ rotate: 360 }}
          transition={{ duration: 2, repeat: Infinity, ease: "linear" }}
        >
          <RefreshCw className="h-8 w-8 text-neon-blue" />
        </motion.div>
        <span className="ml-3 text-cyber-200">Loading backtesting system...</span>
      </div>
    )
  }

  return (
    <div className={`space-y-6 ${className}`}>
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold bg-gradient-to-r from-neon-blue to-matrix-100 bg-clip-text text-transparent">
            Advanced Backtesting
          </h1>
          <p className="text-cyber-200 mt-1">
            Test and optimize trading strategies with real market data
          </p>
        </div>
        
        <div className="flex items-center space-x-4">
          {apiHealth && (
            <Badge className="border-green-400/30 text-green-400">
              <CheckCircle className="h-3 w-3 mr-1" />
              API Healthy
            </Badge>
          )}
          
          <Button
            onClick={loadData}
            variant="outline"
            className="border-neon-blue/30 text-neon-blue hover:bg-neon-blue/10"
          >
            <RefreshCw className="h-4 w-4 mr-2" />
            Refresh
          </Button>
        </div>
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

      {/* Quick Stats */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <GlowingCard glowColor="neon-blue">
          <CardContent className="p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-cyber-200 text-sm">Running</p>
                <p className="text-2xl font-bold text-neon-blue">{runningBacktests}</p>
              </div>
              <Clock className="h-8 w-8 text-neon-blue/60" />
            </div>
          </CardContent>
        </GlowingCard>

        <GlowingCard glowColor="matrix-100">
          <CardContent className="p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-cyber-200 text-sm">Completed</p>
                <p className="text-2xl font-bold text-matrix-100">{completedBacktests}</p>
              </div>
              <CheckCircle className="h-8 w-8 text-matrix-100/60" />
            </div>
          </CardContent>
        </GlowingCard>

        <GlowingCard glowColor="neon-purple">
          <CardContent className="p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-cyber-200 text-sm">Avg Return</p>
                <p className="text-2xl font-bold text-neon-purple">{formatPercentage(avgReturn)}</p>
              </div>
              <TrendingUp className="h-8 w-8 text-neon-purple/60" />
            </div>
          </CardContent>
        </GlowingCard>

        <GlowingCard glowColor="cyber-accent">
          <CardContent className="p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-cyber-200 text-sm">Avg Win Rate</p>
                <p className="text-2xl font-bold text-cyber-accent">{formatPercentage(avgWinRate)}</p>
              </div>
              <Target className="h-8 w-8 text-cyber-accent/60" />
            </div>
          </CardContent>
        </GlowingCard>
      </div>

      {/* Main Interface */}
      <Tabs value={activeTab} onValueChange={setActiveTab} className="space-y-6">
        <TabsList className="grid w-full grid-cols-3 bg-cyber-100/20 border border-neon-blue/30">
          <TabsTrigger 
            value="runner" 
            className="data-[state=active]:bg-neon-blue/20 data-[state=active]:text-neon-blue"
          >
            <PlayCircle className="h-4 w-4 mr-2" />
            Run Backtest
          </TabsTrigger>
          <TabsTrigger 
            value="results"
            className="data-[state=active]:bg-matrix-100/20 data-[state=active]:text-matrix-100"
          >
            <BarChart3 className="h-4 w-4 mr-2" />
            Results
          </TabsTrigger>
          <TabsTrigger 
            value="strategies"
            className="data-[state=active]:bg-neon-purple/20 data-[state=active]:text-neon-purple"
          >
            <Brain className="h-4 w-4 mr-2" />
            Strategy Testing
          </TabsTrigger>
        </TabsList>

        <TabsContent value="runner" className="space-y-6">
          <BacktestRunner 
            onBacktestStart={handleBacktestComplete}
            onBacktestComplete={handleBacktestComplete}
          />
        </TabsContent>

        <TabsContent value="results" className="space-y-6">
          <BacktestResults 
            backtests={backtests}
            onDelete={handleDeleteBacktest}
            onRefresh={loadBacktests}
          />
        </TabsContent>

        <TabsContent value="strategies" className="space-y-6">
          <StrategyTester 
            strategies={strategies}
            onRefresh={() => loadData()}
          />
        </TabsContent>
      </Tabs>
    </div>
  )
} 