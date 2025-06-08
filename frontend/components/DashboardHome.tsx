'use client'

import React, { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Progress } from '@/components/ui/progress'
import { Button } from '@/components/ui/button'
import { 
  TrendingUp, 
  TrendingDown, 
  DollarSign, 
  Activity, 
  Target,
  AlertCircle,
  PlayCircle,
  PauseCircle,
  Brain,
  Zap,
  Sun,
  Moon
} from 'lucide-react'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, BarChart, Bar } from 'recharts'

interface TradeSummary {
  totalTrades: number
  winRate: number
  totalPnL: number
  dailyPnL: number
  weeklyPnL: number
  bestTrade: number
  worstTrade: number
  avgTrade: number
  sharpeRatio: number
  maxDrawdown: number
}

interface StrategyStatus {
  id: string
  name: string
  type: 'scalping' | 'day-trading' | 'smart-money'
  status: 'active' | 'paused' | 'error'
  strategy: string
  pnl: number
  trades: number
  winRate: number
  riskScore: number
  allocation: number
}

interface PnLDataPoint {
  time: string
  pnl: number
  cumulative: number
}

interface RiskMetrics {
  dailyVaR: number
  portfolioHeat: number
  correlationRisk: number
  positionSizing: 'Conservative' | 'Balanced' | 'Aggressive'
}

export default function DashboardHome() {
  const [tradeSummary, setTradeSummary] = useState<TradeSummary>({
    totalTrades: 156,
    winRate: 73.2,
    totalPnL: 2847.65,
    dailyPnL: 156.78,
    weeklyPnL: 945.23,
    bestTrade: 287.45,
    worstTrade: -98.32,
    avgTrade: 18.25,
    sharpeRatio: 2.34,
    maxDrawdown: 4.2
  })

  const [strategyStatuses, setStrategyStatuses] = useState<StrategyStatus[]>([
    { id: '1', name: 'VWAP Bounce Scalper', type: 'scalping', status: 'active', strategy: 'VWAP + Volume Spike', pnl: 234.67, trades: 45, winRate: 68.9, riskScore: 3.2, allocation: 15 },
    { id: '2', name: 'Opening Range Breakout', type: 'day-trading', status: 'active', strategy: 'ORB 30min + Volume', pnl: 456.78, trades: 23, winRate: 78.3, riskScore: 4.1, allocation: 25 },
    { id: '3', name: 'Liquidity Grab Reversal', type: 'smart-money', status: 'active', strategy: 'Order Flow + Liquidity', pnl: 678.90, trades: 18, winRate: 83.3, riskScore: 2.8, allocation: 30 },
    { id: '4', name: 'Micro Breakout Trap', type: 'scalping', status: 'paused', strategy: 'Range Break + Volume', pnl: 123.45, trades: 32, winRate: 65.6, riskScore: 3.8, allocation: 10 },
    { id: '5', name: 'Fair Value Gap Fill', type: 'smart-money', status: 'active', strategy: 'Imbalance + Continuation', pnl: 345.67, trades: 14, winRate: 85.7, riskScore: 2.5, allocation: 20 }
  ])

  const [pnlData, setPnlData] = useState<PnLDataPoint[]>([
    { time: '09:30', pnl: 0, cumulative: 0 },
    { time: '10:00', pnl: 45.2, cumulative: 45.2 },
    { time: '10:30', pnl: 23.8, cumulative: 69.0 },
    { time: '11:00', pnl: -12.5, cumulative: 56.5 },
    { time: '11:30', pnl: 67.3, cumulative: 123.8 },
    { time: '12:00', pnl: 8.9, cumulative: 132.7 },
    { time: '12:30', pnl: 34.1, cumulative: 166.8 },
    { time: '13:00', pnl: -5.2, cumulative: 161.6 },
    { time: '13:30', pnl: 28.7, cumulative: 190.3 }
  ])

  const [riskMetrics, setRiskMetrics] = useState<RiskMetrics>({
    dailyVaR: 2.1,
    portfolioHeat: 67.5,
    correlationRisk: 23.4,
    positionSizing: 'Balanced'
  })

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'active': return 'bg-green-500'
      case 'paused': return 'bg-yellow-500'
      case 'error': return 'bg-red-500'
      default: return 'bg-gray-500'
    }
  }

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'active': return <PlayCircle className="h-4 w-4" />
      case 'paused': return <PauseCircle className="h-4 w-4" />
      case 'error': return <AlertCircle className="h-4 w-4" />
      default: return <Activity className="h-4 w-4" />
    }
  }

  const getTypeIcon = (type: string) => {
    switch (type) {
      case 'scalping': return <Zap className="h-4 w-4 text-yellow-500" />
      case 'day-trading': return <Sun className="h-4 w-4 text-orange-500" />
      case 'smart-money': return <Moon className="h-4 w-4 text-purple-500" />
      default: return <Activity className="h-4 w-4" />
    }
  }

  const strategyPerformanceData = strategyStatuses.map(strategy => ({
    name: strategy.name.split(' ')[0],
    pnl: strategy.pnl,
    winRate: strategy.winRate,
    trades: strategy.trades
  }))

  return (
    <div className="p-6 space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold tracking-tight">LynxTrader - Advanced Strategy Engine</h1>
          <p className="text-muted-foreground">AI-Enhanced Algorithmic Trading Platform</p>
        </div>
        <div className="flex gap-2">
          <Badge className="px-3 py-1 text-sm bg-green-100 text-green-800">
            Live Trading
          </Badge>
          <Badge className="px-3 py-1 text-sm bg-blue-100 text-blue-800">
            AI Risk Management
          </Badge>
        </div>
      </div>

      {/* Key Performance Metrics */}
      <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total P&L</CardTitle>
            <DollarSign className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-green-600">
              ${tradeSummary.totalPnL.toFixed(2)}
            </div>
            <p className="text-xs text-muted-foreground">
              Sharpe Ratio: {tradeSummary.sharpeRatio}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Daily P&L</CardTitle>
            <TrendingUp className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-green-600">
              ${tradeSummary.dailyPnL.toFixed(2)}
            </div>
            <p className="text-xs text-muted-foreground">
              Weekly: ${tradeSummary.weeklyPnL.toFixed(2)}
            </p>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Win Rate</CardTitle>
            <Target className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{tradeSummary.winRate}%</div>
            <Progress value={tradeSummary.winRate} className="mt-2" />
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Max Drawdown</CardTitle>
            <TrendingDown className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-red-600">{tradeSummary.maxDrawdown}%</div>
            <p className="text-xs text-muted-foreground">
              {tradeSummary.totalTrades} total trades
            </p>
          </CardContent>
        </Card>
      </div>

      {/* P&L Chart and Risk Metrics */}
      <div className="grid gap-6 md:grid-cols-2">
        <Card>
          <CardHeader>
            <CardTitle>Daily P&L Performance</CardTitle>
            <CardDescription>Real-time profit and loss tracking</CardDescription>
          </CardHeader>
          <CardContent>
            <ResponsiveContainer width="100%" height={300}>
              <LineChart data={pnlData}>
                <CartesianGrid strokeDasharray="3 3" />
                <XAxis dataKey="time" />
                <YAxis />
                <Tooltip 
                  formatter={(value: number) => [`$${value.toFixed(2)}`, 'P&L']}
                  labelFormatter={(label) => `Time: ${label}`}
                />
                <Line 
                  type="monotone" 
                  dataKey="cumulative" 
                  stroke="#22c55e" 
                  strokeWidth={2}
                  dot={{ fill: '#22c55e' }}
                />
              </LineChart>
            </ResponsiveContainer>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Brain className="h-5 w-5" />
              AI Risk Management
            </CardTitle>
            <CardDescription>Real-time risk monitoring and control</CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            <div className="flex justify-between items-center">
              <span className="text-sm font-medium">Daily VaR (95%)</span>
              <span className="text-sm text-red-600">{riskMetrics.dailyVaR}%</span>
            </div>
            <Progress value={riskMetrics.dailyVaR * 10} className="h-2" />
            
            <div className="flex justify-between items-center">
              <span className="text-sm font-medium">Portfolio Heat</span>
              <span className="text-sm">{riskMetrics.portfolioHeat}%</span>
            </div>
            <Progress value={riskMetrics.portfolioHeat} className="h-2" />
            
            <div className="flex justify-between items-center">
              <span className="text-sm font-medium">Correlation Risk</span>
              <span className="text-sm text-orange-600">{riskMetrics.correlationRisk}%</span>
            </div>
            <Progress value={riskMetrics.correlationRisk} className="h-2" />
            
            <div className="pt-2 border-t">
              <div className="flex justify-between items-center">
                <span className="text-sm font-medium">Position Sizing</span>
                <Badge className="border">{riskMetrics.positionSizing}</Badge>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Strategy Performance Chart */}
      <Card>
        <CardHeader>
          <CardTitle>Strategy Performance Comparison</CardTitle>
          <CardDescription>P&L and win rate by strategy type</CardDescription>
        </CardHeader>
        <CardContent>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={strategyPerformanceData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="name" />
              <YAxis />
              <Tooltip 
                formatter={(value: number, name: string) => {
                  if (name === 'pnl') return [`$${value.toFixed(2)}`, 'P&L']
                  if (name === 'winRate') return [`${value.toFixed(1)}%`, 'Win Rate']
                  return [value, name]
                }}
              />
              <Bar dataKey="pnl" fill="#22c55e" />
              <Bar dataKey="winRate" fill="#3b82f6" />
            </BarChart>
          </ResponsiveContainer>
        </CardContent>
      </Card>

      {/* Active Trading Strategies */}
      <Card>
        <CardHeader>
          <CardTitle>Active Trading Strategies</CardTitle>
          <CardDescription>Monitor your automated trading strategies with AI optimization</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
            {strategyStatuses.map((strategy) => (
              <Card key={strategy.id} className="relative">
                <CardHeader className="pb-3">
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-2">
                      {getTypeIcon(strategy.type)}
                      <span className="font-semibold text-sm">{strategy.name}</span>
                    </div>
                    <div className="flex items-center gap-2">
                      <div className={`w-2 h-2 rounded-full ${getStatusColor(strategy.status)}`} />
                      {getStatusIcon(strategy.status)}
                    </div>
                  </div>
                  <p className="text-xs text-muted-foreground">{strategy.strategy}</p>
                </CardHeader>
                <CardContent className="space-y-2">
                  <div className="flex justify-between text-sm">
                    <span>P&L:</span>
                    <span className={strategy.pnl >= 0 ? 'text-green-600' : 'text-red-600'}>
                      ${strategy.pnl.toFixed(2)}
                    </span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span>Trades:</span>
                    <span>{strategy.trades}</span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span>Win Rate:</span>
                    <span>{strategy.winRate.toFixed(1)}%</span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span>Risk Score:</span>
                    <span className={strategy.riskScore < 3 ? 'text-green-600' : strategy.riskScore < 4 ? 'text-yellow-600' : 'text-red-600'}>
                      {strategy.riskScore}/5
                    </span>
                  </div>
                  <div className="flex justify-between text-sm">
                    <span>Allocation:</span>
                    <span>{strategy.allocation}%</span>
                  </div>
                  <Progress value={strategy.allocation} className="h-1 mt-2" />
                  
                  <div className="flex gap-2 pt-2">
                    <Button variant="outline" className="flex-1 text-xs py-1">
                      {strategy.status === 'active' ? 'Pause' : 'Start'}
                    </Button>
                    <Button variant="outline" className="flex-1 text-xs py-1">
                      Settings
                    </Button>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </CardContent>
      </Card>

      {/* Trading Summary Stats */}
      <div className="grid gap-4 md:grid-cols-3">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2 text-yellow-600">
              <Zap className="h-5 w-5" />
              Scalping (1-5min)
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-sm">Active Strategies:</span>
                <span className="text-sm font-medium">2</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">Combined P&L:</span>
                <span className="text-sm font-medium text-green-600">$358.12</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">Avg Trade Duration:</span>
                <span className="text-sm font-medium">2.3 min</span>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2 text-orange-600">
              <Sun className="h-5 w-5" />
              Day Trading (5-15min)
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-sm">Active Strategies:</span>
                <span className="text-sm font-medium">1</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">Combined P&L:</span>
                <span className="text-sm font-medium text-green-600">$456.78</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">Avg Trade Duration:</span>
                <span className="text-sm font-medium">8.7 min</span>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2 text-purple-600">
              <Moon className="h-5 w-5" />
              Smart Money (15min+)
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-sm">Active Strategies:</span>
                <span className="text-sm font-medium">2</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">Combined P&L:</span>
                <span className="text-sm font-medium text-green-600">$1,024.57</span>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">Avg Trade Duration:</span>
                <span className="text-sm font-medium">24.5 min</span>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>
    </div>
  )
} 