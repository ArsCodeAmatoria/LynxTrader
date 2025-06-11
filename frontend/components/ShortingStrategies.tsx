'use client'

import React, { useState } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { TrendingDown, Brain, Zap, Sun, Target, PlayCircle, Eye, Crosshair } from 'lucide-react'
import { motion } from 'framer-motion'
import CountUp from 'react-countup'

interface ShortingStrategy {
  id: string
  name: string
  category: 'scalping' | 'day-trading' | 'swing-trading' | 'smart-money'
  timeframe: string
  status: 'active' | 'paused' | 'setup' | 'triggered'
  description: string
  pnl: number
  trades: number
  winRate: number
}

const GlowingCard = ({ children, className = "" }: { children: React.ReactNode, className?: string }) => (
  <motion.div
    initial={{ opacity: 0, scale: 0.95 }}
    animate={{ opacity: 1, scale: 1 }}
    transition={{ duration: 0.3 }}
    className={`relative ${className}`}
  >
    <div className="absolute inset-0 bg-gradient-to-r from-red-500/20 to-transparent rounded-lg blur-sm" />
    <div className="relative bg-cyber-100/80 backdrop-blur-sm border border-red-300/30 rounded-lg">
      {children}
    </div>
  </motion.div>
)

export default function ShortingStrategies() {
  const [shortingStrategies] = useState<ShortingStrategy[]>([
    {
      id: '1',
      name: 'Fake Breakout Reversal',
      category: 'scalping',
      timeframe: '1-5m',
      status: 'active',
      description: 'Fake breakouts above highs on low volume with reversal confirmation',
      pnl: 432.67,
      trades: 28,
      winRate: 82.1
    },
    {
      id: '2',
      name: 'VWAP Slap',
      category: 'scalping',
      timeframe: '1-5m',
      status: 'triggered',
      description: 'VWAP rejection shorts with volume divergence confirmation',
      pnl: 267.89,
      trades: 35,
      winRate: 74.3
    },
    {
      id: '3',
      name: 'Bear Flag Breakdown',
      category: 'day-trading',
      timeframe: '5-30m',
      status: 'active',
      description: 'Bear flag pattern breakdown with trendline break confirmation',
      pnl: 634.12,
      trades: 19,
      winRate: 78.9
    },
    {
      id: '4',
      name: 'Liquidity Sweep Trap',
      category: 'smart-money',
      timeframe: '15m-1H',
      status: 'active',
      description: 'Smart money liquidity grabs above previous highs',
      pnl: 823.45,
      trades: 14,
      winRate: 85.7
    }
  ])

  const getCategoryIcon = (category: string) => {
    switch (category) {
      case 'scalping': return <Zap className="h-4 w-4 text-yellow-400" />
      case 'day-trading': return <Sun className="h-4 w-4 text-orange-400" />
      case 'swing-trading': return <Target className="h-4 w-4 text-blue-400" />
      case 'smart-money': return <Brain className="h-4 w-4 text-pink-400" />
      default: return <Target className="h-4 w-4" />
    }
  }

  const activeStrategies = shortingStrategies.filter(s => s.status === 'active').length
  const totalShortPnL = shortingStrategies.reduce((sum, strategy) => sum + strategy.pnl, 0)

  return (
    <div className="space-y-6 p-6">
      <motion.div 
        initial={{ opacity: 0, y: -20 }}
        animate={{ opacity: 1, y: 0 }}
        className="flex items-center justify-between"
      >
        <div className="space-y-2">
          <h2 className="text-3xl font-bold bg-gradient-to-r from-red-500 via-pink-500 to-red-700 bg-clip-text text-transparent">
            🔻 SHORT STRATEGIES
          </h2>
          <p className="text-pink-400 font-mono text-sm">
            ACTIVE SHORTS: {activeStrategies}
          </p>
        </div>
        <Badge className="px-4 py-2 text-sm bg-red-500/20 text-red-300 border-red-300">
          <TrendingDown className="w-4 h-4 mr-2" />
          SHORT BIAS ACTIVE
        </Badge>
      </motion.div>

      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
        <GlowingCard>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-mono text-red-300">SHORT P&L</CardTitle>
            <TrendingDown className="h-4 w-4 text-red-300" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-red-300">
              $<CountUp end={totalShortPnL} decimals={2} duration={2} />
            </div>
          </CardContent>
        </GlowingCard>
      </div>

      <div className="space-y-4">
        <h3 className="text-xl font-bold text-white flex items-center gap-2">
          <Crosshair className="h-5 w-5 text-red-400" />
          ACTIVE SHORT PROTOCOLS
        </h3>
        <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
          {shortingStrategies.map((strategy, index) => (
            <motion.div
              key={strategy.id}
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ delay: index * 0.1 }}
            >
              <GlowingCard>
                <CardHeader>
                  <div className="flex items-center gap-2">
                    {getCategoryIcon(strategy.category)}
                    <CardTitle className="text-sm font-mono text-white">
                      {strategy.name.toUpperCase()}
                    </CardTitle>
                  </div>
                  <CardDescription className="text-xs font-mono text-gray-400">
                    {strategy.timeframe} • {strategy.description}
                  </CardDescription>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <div className="text-lg font-bold text-white">
                        ${strategy.pnl.toFixed(2)}
                      </div>
                      <div className="text-xs text-gray-400">P&L</div>
                    </div>
                    <div>
                      <div className="text-lg font-bold text-green-400">
                        {strategy.winRate.toFixed(1)}%
                      </div>
                      <div className="text-xs text-gray-400">Win Rate</div>
                    </div>
                  </div>
                  <div className="flex gap-2">
                    <Button variant="outline" className="flex-1 border-blue-400/30 text-blue-400 h-8 px-2 text-xs">
                      <Eye className="h-3 w-3 mr-1" />
                      Monitor
                    </Button>
                    <Button variant="outline" className="flex-1 border-green-400/30 text-green-400 h-8 px-2 text-xs">
                      <PlayCircle className="h-3 w-3 mr-1" />
                      Execute
                    </Button>
                  </div>
                </CardContent>
              </GlowingCard>
            </motion.div>
          ))}
        </div>
      </div>
    </div>
  )
} 