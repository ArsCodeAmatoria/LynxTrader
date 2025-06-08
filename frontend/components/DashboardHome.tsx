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
  PauseCircle
} from 'lucide-react'
import { LineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from 'recharts'

interface TradeSummary {
  totalTrades: number
  winRate: number
  totalPnL: number
  dailyPnL: number
  weeklyPnL: number
  bestTrade: number
  worstTrade: number
  avgTrade: number
}

interface BotStatus {
  id: string
  name: string
  status: 'active' | 'paused' | 'error'
  strategy: string
  pnl: number
  trades: number
}

interface PnLDataPoint {
  time: string
  pnl: number
  cumulative: number
}

export default function DashboardHome() {
  const [tradeSummary, setTradeSummary] = useState<TradeSummary>({
    totalTrades: 24,
    winRate: 67.5,
    totalPnL: 156.78,
    dailyPnL: 23.45,
    weeklyPnL: 89.12,
    bestTrade: 45.67,
    worstTrade: -12.34,
    avgTrade: 6.53
  })

  const [botStatuses, setBotStatuses] = useState<BotStatus[]>([
    { id: '1', name: 'Scalp-Master', status: 'active', strategy: 'EMA Crossover', pnl: 67.23, trades: 15 },
    { id: '2', name: 'Swing-Hunter', status: 'paused', strategy: 'RSI + Support', pnl: 34.12, trades: 8 },
    { id: '3', name: 'Crypto-Flash', status: 'error', strategy: 'Volume Spike', pnl: -8.45, trades: 12 }
  ])

  const [pnlData, setPnlData] = useState<PnLDataPoint[]>([
    { time: '09:30', pnl: 0, cumulative: 0 },
    { time: '10:00', pnl: 12.5, cumulative: 12.5 },
    { time: '10:30', pnl: -5.2, cumulative: 7.3 },
    { time: '11:00', pnl: 18.7, cumulative: 26.0 },
    { time: '11:30', pnl: 8.4, cumulative: 34.4 },
    { time: '12:00', pnl: -3.1, cumulative: 31.3 },
  ])

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

  return (
    <div className="p-6 space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold tracking-tight">LynxTrader Dashboard</h1>
          <p className="text-muted-foreground">Monitor your algorithmic trading performance</p>
        </div>
        <Badge className="px-3 py-1 text-sm bg-green-100 text-green-800">
          Live Trading
        </Badge>
      </div>

      {/* Key Metrics */}
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
              +12.3% from last week
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
              Target: $30.00 (78%)
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
            <CardTitle className="text-sm font-medium">Total Trades</CardTitle>
            <Activity className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{tradeSummary.totalTrades}</div>
            <p className="text-xs text-muted-foreground">
              Avg: ${tradeSummary.avgTrade.toFixed(2)}/trade
            </p>
          </CardContent>
        </Card>
      </div>

      {/* P&L Chart */}
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

      {/* Bot Status Cards */}
      <Card>
        <CardHeader>
          <CardTitle>Active Trading Bots</CardTitle>
          <CardDescription>Monitor your automated trading strategies</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="grid gap-4 md:grid-cols-3">
            {botStatuses.map((bot) => (
              <Card key={bot.id} className="relative">
                <CardHeader className="pb-3">
                  <div className="flex items-center justify-between">
                    <CardTitle className="text-sm">{bot.name}</CardTitle>
                    <div className="flex items-center space-x-2">
                      {getStatusIcon(bot.status)}
                      <div className={`w-2 h-2 rounded-full ${getStatusColor(bot.status)}`} />
                    </div>
                  </div>
                  <CardDescription>{bot.strategy}</CardDescription>
                </CardHeader>
                <CardContent className="pt-0">
                  <div className="flex justify-between items-center text-sm">
                    <span>P&L:</span>
                    <span className={bot.pnl >= 0 ? 'text-green-600' : 'text-red-600'}>
                      ${bot.pnl.toFixed(2)}
                    </span>
                  </div>
                  <div className="flex justify-between items-center text-sm mt-1">
                    <span>Trades:</span>
                    <span>{bot.trades}</span>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </CardContent>
      </Card>

      {/* Quick Actions */}
      <Card>
        <CardHeader>
          <CardTitle>Quick Actions</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex space-x-4">
            <Button>Start New Bot</Button>
            <Button variant="outline">View Strategy Builder</Button>
            <Button variant="outline">Open Trade Lab</Button>
            <Button variant="destructive">Emergency Stop All</Button>
          </div>
        </CardContent>
      </Card>
    </div>
  )
} 