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
  Moon,
  Database,
  Cpu,
  BarChart3,
  PieChart,
  LineChart,
  Calculator,
  Binary,
  Signal,
  Wifi
} from 'lucide-react'
import { LineChart as RechartsLineChart, Line, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer, BarChart, Bar, Area, AreaChart } from 'recharts'
import { motion, AnimatePresence } from 'framer-motion'
import { InlineMath, BlockMath } from 'react-katex'
import 'katex/dist/katex.min.css'
import CountUp from 'react-countup'
import Backtesting from './Backtesting'

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
  status: 'active' | 'paused' | 'error' | 'triggered'
  strategy: string
  pnl: number
  trades: number
  winRate: number
  riskScore: number
  allocation: number
  formula: string
}

interface PnLDataPoint {
  time: string
  pnl: number
  cumulative: number
  volume: number
}

interface RiskMetrics {
  dailyVaR: number
  portfolioHeat: number
  correlationRisk: number
  positionSizing: 'Conservative' | 'Balanced' | 'Aggressive'
  kellyFraction: number
  expectedReturn: number
  volatility: number
}

// Matrix rain component
const MatrixRain = () => {
  const [drops, setDrops] = useState<number[]>([])

  useEffect(() => {
    const newDrops = Array.from({ length: 20 }, () => Math.random() * 100)
    setDrops(newDrops)
  }, [])

  return (
    <div className="fixed inset-0 pointer-events-none overflow-hidden opacity-10">
      {drops.map((drop, index) => (
        <div
          key={index}
          className="absolute text-matrix-100 font-matrix text-xs animate-matrix-rain"
          style={{
            left: `${drop}%`,
            animationDelay: `${Math.random() * 2}s`,
            animationDuration: `${3 + Math.random() * 2}s`
          }}
        >
          {Array.from({ length: 15 }, () => String.fromCharCode(0x30A0 + Math.random() * 96)).join('')}
        </div>
      ))}
    </div>
  )
}

// Cyber scan line effect
const ScanLine = () => (
  <div className="fixed top-0 left-0 w-full h-0.5 bg-gradient-to-r from-transparent via-neon-blue to-transparent animate-scan-line opacity-30 pointer-events-none" />
)

// Glowing border component
const GlowingCard = ({ children, className = "", glowColor = "neon-blue" }: { children: React.ReactNode, className?: string, glowColor?: string }) => (
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

export default function DashboardHome() {
  const [activeView, setActiveView] = useState<'dashboard' | 'backtesting'>('dashboard')
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
    // Long Strategies
    { 
      id: '1', 
      name: 'SCALP: VWAP Bounce', 
      type: 'scalping', 
      status: 'active', 
      strategy: 'Scalping • 1-5min • VWAP + Volume Spike', 
      pnl: 234.67, 
      trades: 45, 
      winRate: 68.9, 
      riskScore: 3.2, 
      allocation: 15,
      formula: 'P_{entry} = VWAP \\pm \\sigma \\cdot \\sqrt{\\frac{V}{V_{avg}}}'
    },
    { 
      id: '2', 
      name: 'DAY: Opening Range Breakout', 
      type: 'day-trading', 
      status: 'active', 
      strategy: 'Day Trading • 5-30min • ORB + Volume Confirmation', 
      pnl: 456.78, 
      trades: 23, 
      winRate: 78.3, 
      riskScore: 4.1, 
      allocation: 25,
      formula: 'R = \\max(P_{30}) - \\min(P_{30}), \\quad Entry = P_{30} \\pm 0.1R'
    },
    { 
      id: '3', 
      name: 'SWING: Liquidity Grab Reversal', 
      type: 'smart-money', 
      status: 'active', 
      strategy: 'Swing Trading • 1-4hr • Smart Money Concepts', 
      pnl: 678.90, 
      trades: 18, 
      winRate: 83.3, 
      riskScore: 2.8, 
      allocation: 30,
      formula: 'L_{score} = \\frac{\\sum V_{bid}}{\\sum V_{ask}} \\cdot \\frac{OI_{change}}{OI_{avg}}'
    },
    { 
      id: '4', 
      name: 'SWING: Fair Value Gap Fill', 
      type: 'smart-money', 
      status: 'active', 
      strategy: 'Swing Trading • 30min-2hr • Imbalance Correction', 
      pnl: 345.67, 
      trades: 14, 
      winRate: 85.7, 
      riskScore: 2.5, 
      allocation: 20,
      formula: 'FVG = |P_{high}^{i-1} - P_{low}^{i+1}|, \\quad Fill = 0.5 \\cdot FVG'
    },
    // Shorting Strategies 
    { 
      id: '6', 
      name: 'SHORT: Fake Breakout Reversal', 
      type: 'scalping', 
      status: 'active', 
      strategy: 'Scalping • 1-5min • Fake Highs on Low Volume', 
      pnl: 432.67, 
      trades: 28, 
      winRate: 82.1, 
      riskScore: 3.5, 
      allocation: 12,
      formula: 'Short = P > HOD \\land V < V_{avg} \\land \\Delta P < 0.1\\%'
    },
    { 
      id: '7', 
      name: 'SHORT: VWAP Slap', 
      type: 'scalping', 
      status: 'active', 
      strategy: 'Scalping • 1-5min • VWAP Rejection + Volume Divergence', 
      pnl: 267.89, 
      trades: 35, 
      winRate: 74.3, 
      riskScore: 3.8, 
      allocation: 8,
      formula: 'Short = P_{reject} > VWAP \\land \\frac{V}{V_{avg}} < 0.7'
    },
    { 
      id: '8', 
      name: 'SHORT: Bear Flag Breakdown', 
      type: 'day-trading', 
      status: 'active', 
      strategy: 'Day Trading • 5-30min • Flag Pattern + Trendline Break', 
      pnl: 634.12, 
      trades: 19, 
      winRate: 78.9, 
      riskScore: 2.9, 
      allocation: 18,
      formula: 'Short = P < TL_{break} \\land Flag_{confirmed} \\land V > 1.5V_{avg}'
    },
    { 
      id: '9', 
      name: 'SHORT: Supply Zone Rejection', 
      type: 'smart-money', 
      status: 'active', 
      strategy: 'Swing Trading • 1H-1D • Historical Resistance Zones', 
      pnl: 823.45, 
      trades: 14, 
      winRate: 85.7, 
      riskScore: 2.6, 
      allocation: 22,
      formula: 'Short = P_{test} \\in Supply_{zone} \\land RSI > 70 \\land DI < 0'
    },
    { 
      id: '10', 
      name: 'SHORT: Liquidity Sweep Trap', 
      type: 'smart-money', 
      status: 'triggered', 
      strategy: 'Smart Money • 15m-1H • Equal Highs Liquidity Grab', 
      pnl: 589.34, 
      trades: 11, 
      winRate: 90.9, 
      riskScore: 2.3, 
      allocation: 16,
      formula: 'Short = P > EH_{sweep} \\land OI_{decrease} \\land V_{spike}'
    },
    { 
      id: '5', 
      name: 'SCALP: Micro Breakout Trap', 
      type: 'scalping', 
      status: 'paused', 
      strategy: 'Scalping • 1-3min • Range Break + Volume Trap', 
      pnl: 123.45, 
      trades: 32, 
      winRate: 65.6, 
      riskScore: 3.8, 
      allocation: 10,
      formula: 'Range = ATR_{14}, \\quad Breakout = P \\pm 1.5 \\cdot Range'
    }
  ])

  const [pnlData, setPnlData] = useState<PnLDataPoint[]>([
    { time: '09:30', pnl: 0, cumulative: 0, volume: 100 },
    { time: '10:00', pnl: 45.2, cumulative: 45.2, volume: 150 },
    { time: '10:30', pnl: 23.8, cumulative: 69.0, volume: 200 },
    { time: '11:00', pnl: -12.5, cumulative: 56.5, volume: 80 },
    { time: '11:30', pnl: 67.3, cumulative: 123.8, volume: 300 },
    { time: '12:00', pnl: 8.9, cumulative: 132.7, volume: 120 },
    { time: '12:30', pnl: 34.1, cumulative: 166.8, volume: 180 },
    { time: '13:00', pnl: -5.2, cumulative: 161.6, volume: 90 },
    { time: '13:30', pnl: 28.7, cumulative: 190.3, volume: 250 }
  ])

  const [riskMetrics, setRiskMetrics] = useState<RiskMetrics>({
    dailyVaR: 2.1,
    portfolioHeat: 67.5,
    correlationRisk: 23.4,
    positionSizing: 'Balanced',
    kellyFraction: 0.15,
    expectedReturn: 0.067,
    volatility: 0.28
  })

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'active': return 'text-matrix-100 bg-matrix-500/20 border-matrix-100'
      case 'paused': return 'text-neon-orange bg-neon-orange/20 border-neon-orange'
      case 'error': return 'text-neon-pink bg-neon-pink/20 border-neon-pink'
      case 'triggered': return 'text-neon-yellow bg-neon-yellow/20 border-neon-yellow'
      default: return 'text-gray-400 bg-gray-500/20 border-gray-400'
    }
  }

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'active': return <PlayCircle className="h-4 w-4" />
      case 'paused': return <PauseCircle className="h-4 w-4" />
      case 'error': return <AlertCircle className="h-4 w-4" />
      case 'triggered': return <Zap className="h-4 w-4" />
      default: return <Activity className="h-4 w-4" />
    }
  }

  const getTypeIcon = (type: string) => {
    switch (type) {
      case 'scalping': return <Zap className="h-4 w-4 text-neon-yellow" />
      case 'day-trading': return <Sun className="h-4 w-4 text-neon-orange" />
      case 'smart-money': return <Moon className="h-4 w-4 text-crypto-400" />
      default: return <Activity className="h-4 w-4" />
    }
  }

  const getTypeLabel = (type: string) => {
    switch (type) {
      case 'scalping': return 'SCALP'
      case 'day-trading': return 'DAY'
      case 'smart-money': return 'SWING'
      default: return 'UNKNOWN'
    }
  }

  const strategyPerformanceData = strategyStatuses.map(strategy => ({
    name: strategy.name.split(':')[0], // Show the trading type prefix (SCALP, DAY, SWING)
    pnl: strategy.pnl,
    winRate: strategy.winRate,
    trades: strategy.trades
  }))

  return (
    <div className="min-h-screen bg-cyber-gradient text-white relative overflow-hidden">
      <MatrixRain />
      <ScanLine />
      
      <div className="relative z-10 p-6 space-y-6">
        {/* Cyber Header */}
        <motion.div 
          initial={{ opacity: 0, y: -20 }}
          animate={{ opacity: 1, y: 0 }}
          className="flex items-center justify-between"
        >
          <div className="space-y-2">
            <h1 className="text-4xl font-cyber font-bold bg-gradient-to-r from-neon-blue via-neon-green to-neon-pink bg-clip-text text-transparent animate-glow">
              LYNX_TRADER.EXE
            </h1>
            <p className="text-neon-blue font-mono text-sm animate-cyber-pulse">
              &gt; QUANTUM_TRADING_MATRIX :: STATUS_ONLINE
            </p>
            <div className="flex gap-4 font-mono text-xs">
              <span className="text-matrix-100">UPTIME: 99.7%</span>
              <span className="text-neon-blue">LATENCY: 0.3ms</span>
              <span className="text-neon-pink">SYNC: REALTIME</span>
            </div>
          </div>
          <div className="flex gap-3">
            <Badge className="px-4 py-2 text-sm bg-matrix-500/20 text-matrix-100 border-matrix-100 animate-glow">
              <Signal className="w-4 h-4 mr-2" />
              NEURAL_LINK_ACTIVE
            </Badge>
            <Badge className="px-4 py-2 text-sm bg-crypto-500/20 text-crypto-300 border-crypto-300 animate-glow">
              <Brain className="w-4 h-4 mr-2" />
              AI_RISK_ENGINE
            </Badge>
          </div>
        </motion.div>

        {/* Navigation */}
        <motion.div 
          initial={{ opacity: 0, y: -10 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: 0.3 }}
          className="flex space-x-4 mb-6"
        >
          <Button
            onClick={() => setActiveView('dashboard')}
            variant={activeView === 'dashboard' ? "default" : "outline"}
            className={activeView === 'dashboard' ? 
              "bg-neon-blue/20 text-neon-blue border-neon-blue font-cyber" : 
              "border-cyber-200/30 text-cyber-200 font-cyber hover:bg-neon-blue/10"
            }
          >
            <Activity className="h-4 w-4 mr-2" />
            LIVE_DASHBOARD
          </Button>
          <Button
            onClick={() => setActiveView('backtesting')}
            variant={activeView === 'backtesting' ? "default" : "outline"}
            className={activeView === 'backtesting' ? 
              "bg-matrix-100/20 text-matrix-100 border-matrix-100 font-cyber" : 
              "border-cyber-200/30 text-cyber-200 font-cyber hover:bg-matrix-100/10"
            }
          >
            <BarChart3 className="h-4 w-4 mr-2" />
            BACKTESTING_ENGINE
          </Button>
        </motion.div>

        {/* Main Content - Conditional Rendering */}
        {activeView === 'dashboard' ? (
          <div className="space-y-6">

        {/* Kelly Criterion Formula Display */}
        <GlowingCard className="mb-6" glowColor="crypto-400">
          <CardHeader>
            <CardTitle className="text-crypto-300 font-cyber flex items-center gap-2">
              <Calculator className="h-5 w-5" />
              KELLY CRITERION OPTIMIZATION
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="grid md:grid-cols-2 gap-6">
              <div className="space-y-3">
                <BlockMath math="f^* = \frac{bp - q}{b}" />
                <div className="text-xs font-mono text-neon-blue space-y-1">
                  <div>f* = Optimal fraction to wager</div>
                  <div>b = Odds received (reward/risk ratio)</div>
                  <div>p = Probability of winning = {(tradeSummary.winRate/100).toFixed(3)}</div>
                  <div>q = Probability of losing = {(1 - tradeSummary.winRate/100).toFixed(3)}</div>
                </div>
              </div>
              <div className="space-y-3">
                <div className="text-neon-green font-mono">
                  Current Kelly Fraction: <InlineMath math={`f^* = ${riskMetrics.kellyFraction.toFixed(3)}`} />
                </div>
                <div className="text-neon-orange font-mono">
                  Expected Return: <InlineMath math={`E[R] = ${(riskMetrics.expectedReturn * 100).toFixed(1)}\\%`} />
                </div>
                <div className="text-neon-pink font-mono">
                  Portfolio Volatility: <InlineMath math={`\\sigma = ${(riskMetrics.volatility * 100).toFixed(1)}\\%`} />
                </div>
              </div>
            </div>
          </CardContent>
        </GlowingCard>

        {/* Key Performance Metrics - Cyber Style */}
        <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-4">
          <GlowingCard glowColor="matrix-100">
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-mono text-matrix-100">TOTAL_P&L</CardTitle>
              <DollarSign className="h-4 w-4 text-matrix-100 animate-glow" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-cyber font-bold text-matrix-100">
                $<CountUp end={tradeSummary.totalPnL} decimals={2} duration={2} />
              </div>
              <p className="text-xs text-neon-blue font-mono">
                SHARPE: <InlineMath math={`S = ${tradeSummary.sharpeRatio}`} />
              </p>
            </CardContent>
          </GlowingCard>

          <GlowingCard glowColor="neon-blue">
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-mono text-neon-blue">DAILY_DELTA</CardTitle>
              <TrendingUp className="h-4 w-4 text-neon-blue animate-glow" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-cyber font-bold text-neon-blue">
                $<CountUp end={tradeSummary.dailyPnL} decimals={2} duration={2} />
              </div>
              <p className="text-xs text-neon-green font-mono">
                WEEKLY: $<CountUp end={tradeSummary.weeklyPnL} decimals={2} duration={2} />
              </p>
            </CardContent>
          </GlowingCard>

          <GlowingCard glowColor="neon-green">
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-mono text-neon-green">WIN_RATIO</CardTitle>
              <Target className="h-4 w-4 text-neon-green animate-glow" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-cyber font-bold text-neon-green">
                <CountUp end={tradeSummary.winRate} decimals={1} duration={2} />%
              </div>
              <Progress value={tradeSummary.winRate} className="mt-2 bg-cyber-200" />
            </CardContent>
          </GlowingCard>

          <GlowingCard glowColor="neon-pink">
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-mono text-neon-pink">MAX_DRAWDOWN</CardTitle>
              <TrendingDown className="h-4 w-4 text-neon-pink animate-glow" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-cyber font-bold text-neon-pink">
                <CountUp end={tradeSummary.maxDrawdown} decimals={1} duration={2} />%
              </div>
              <p className="text-xs text-neon-blue font-mono">
                TRADES: <CountUp end={tradeSummary.totalTrades} duration={2} />
              </p>
            </CardContent>
          </GlowingCard>
        </div>

        {/* Advanced Charts Section */}
        <div className="grid gap-6 md:grid-cols-2">
          <GlowingCard glowColor="neon-blue">
            <CardHeader>
              <CardTitle className="text-neon-blue font-cyber flex items-center gap-2">
                <LineChart className="h-5 w-5" />
                QUANTUM_P&L_MATRIX
              </CardTitle>
              <CardDescription className="text-cyber-300 font-mono">Real-time profit oscillations</CardDescription>
            </CardHeader>
            <CardContent>
              <ResponsiveContainer width="100%" height={300}>
                <AreaChart data={pnlData}>
                  <defs>
                    <linearGradient id="pnlGradient" x1="0" y1="0" x2="0" y2="1">
                      <stop offset="5%" stopColor="#00ffff" stopOpacity={0.8}/>
                      <stop offset="95%" stopColor="#00ffff" stopOpacity={0.1}/>
                    </linearGradient>
                  </defs>
                  <CartesianGrid strokeDasharray="3 3" stroke="#1a1a2e" />
                  <XAxis dataKey="time" stroke="#00ffff" fontSize={10} />
                  <YAxis stroke="#00ffff" fontSize={10} />
                                  <Tooltip 
                  contentStyle={{ 
                    backgroundColor: '#0f0f1a !important', 
                    border: '1px solid #00ffff !important',
                    borderRadius: '8px',
                    color: '#00ffff !important',
                    fontFamily: 'Fira Code, monospace',
                    boxShadow: '0 0 20px rgba(0, 255, 255, 0.3)',
                    backdropFilter: 'blur(10px)'
                  }}
                  wrapperStyle={{
                    backgroundColor: 'transparent !important',
                    border: 'none !important'
                  }}
                  labelStyle={{
                    color: '#ff007f !important',
                    fontWeight: 'bold',
                    backgroundColor: 'transparent !important'
                  }}
                  itemStyle={{
                    color: '#00ffff !important',
                    backgroundColor: 'transparent !important'
                  }}
                  formatter={(value: number) => [`$${value.toFixed(2)}`, 'P&L']}
                  labelFormatter={(label) => `TIME: ${label}`}
                />
                  <Area 
                    type="monotone" 
                    dataKey="cumulative" 
                    stroke="#00ffff" 
                    fill="url(#pnlGradient)"
                    strokeWidth={2}
                    dot={{ fill: '#00ffff', stroke: '#00ffff', strokeWidth: 2, r: 4 }}
                  />
                </AreaChart>
              </ResponsiveContainer>
            </CardContent>
          </GlowingCard>

          <GlowingCard glowColor="crypto-400">
            <CardHeader>
              <CardTitle className="text-crypto-300 font-cyber flex items-center gap-2">
                <Brain className="h-5 w-5" />
                AI_RISK_NEURAL_NET
              </CardTitle>
              <CardDescription className="text-cyber-300 font-mono">Quantum risk assessment protocols</CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="space-y-3">
                <div className="flex justify-between items-center">
                  <span className="text-sm font-mono text-neon-pink">VaR_95%:</span>
                  <span className="text-sm text-neon-pink font-cyber">
                    <InlineMath math={`VaR = ${riskMetrics.dailyVaR}\\%`} />
                  </span>
                </div>
                <Progress value={riskMetrics.dailyVaR * 10} className="h-2 bg-cyber-200" />
                
                <div className="flex justify-between items-center">
                  <span className="text-sm font-mono text-neon-orange">PORTFOLIO_HEAT:</span>
                  <span className="text-sm text-neon-orange font-cyber">{riskMetrics.portfolioHeat}%</span>
                </div>
                <Progress value={riskMetrics.portfolioHeat} className="h-2 bg-cyber-200" />
                
                <div className="flex justify-between items-center">
                  <span className="text-sm font-mono text-neon-blue">CORRELATION_MATRIX:</span>
                  <span className="text-sm text-neon-blue font-cyber">{riskMetrics.correlationRisk}%</span>
                </div>
                <Progress value={riskMetrics.correlationRisk} className="h-2 bg-cyber-200" />
              </div>
              
              <div className="pt-3 border-t border-neon-blue/30">
                <div className="flex justify-between items-center">
                  <span className="text-sm font-mono text-crypto-300">POSITION_ALGORITHM:</span>
                  <Badge className="border border-crypto-300 text-crypto-300 bg-crypto-500/20">
                    {riskMetrics.positionSizing}
                  </Badge>
                </div>
              </div>
            </CardContent>
          </GlowingCard>
        </div>

        {/* Strategy Performance with Formulas */}
        <GlowingCard glowColor="neon-green">
          <CardHeader>
            <CardTitle className="text-neon-green font-cyber flex items-center gap-2">
              <BarChart3 className="h-5 w-5" />
              STRATEGY_PERFORMANCE_MATRIX
            </CardTitle>
            <CardDescription className="text-cyber-300 font-mono">Neural network strategy comparison</CardDescription>
          </CardHeader>
          <CardContent>
            <ResponsiveContainer width="100%" height={300}>
              <BarChart data={strategyPerformanceData}>
                <defs>
                  <linearGradient id="barGradient" x1="0" y1="0" x2="0" y2="1">
                    <stop offset="5%" stopColor="#39ff14" stopOpacity={0.8}/>
                    <stop offset="95%" stopColor="#39ff14" stopOpacity={0.3}/>
                  </linearGradient>
                </defs>
                <CartesianGrid strokeDasharray="3 3" stroke="#1a1a2e" />
                <XAxis dataKey="name" stroke="#39ff14" fontSize={10} />
                <YAxis stroke="#39ff14" fontSize={10} />
                <Tooltip 
                  contentStyle={{ 
                    backgroundColor: '#0f0f1a !important', 
                    border: '1px solid #39ff14 !important',
                    borderRadius: '8px',
                    color: '#39ff14 !important',
                    fontFamily: 'Fira Code, monospace',
                    boxShadow: '0 0 20px rgba(57, 255, 20, 0.3)',
                    backdropFilter: 'blur(10px)'
                  }}
                  wrapperStyle={{
                    backgroundColor: 'transparent !important',
                    border: 'none !important'
                  }}
                  labelStyle={{
                    color: '#ff007f !important',
                    fontWeight: 'bold',
                    backgroundColor: 'transparent !important'
                  }}
                  itemStyle={{
                    color: '#39ff14 !important',
                    backgroundColor: 'transparent !important'
                  }}
                  formatter={(value: number, name: string) => {
                    if (name === 'pnl') return [`$${value.toFixed(2)}`, 'P&L']
                    if (name === 'winRate') return [`${value.toFixed(1)}%`, 'Win Rate']
                    return [value, name]
                  }}
                />
                <Bar dataKey="pnl" fill="url(#barGradient)" />
              </BarChart>
            </ResponsiveContainer>
          </CardContent>
        </GlowingCard>

        {/* Active Strategies with Mathematical Formulas */}
        <GlowingCard glowColor="crypto-400">
          <CardHeader>
            <CardTitle className="text-crypto-300 font-cyber flex items-center gap-2">
              <Cpu className="h-5 w-5" />
              ACTIVE_NEURAL_STRATEGIES
            </CardTitle>
            <CardDescription className="text-cyber-300 font-mono">Quantum trading algorithms with mathematical models</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="grid gap-4 md:grid-cols-2 lg:grid-cols-3">
              {strategyStatuses.map((strategy, index) => (
                <motion.div
                  key={strategy.id}
                  initial={{ opacity: 0, y: 20 }}
                  animate={{ opacity: 1, y: 0 }}
                  transition={{ delay: index * 0.1 }}
                >
                  <GlowingCard className="h-full" glowColor={strategy.status === 'active' ? 'matrix-100' : strategy.status === 'triggered' ? 'neon-yellow' : 'neon-orange'}>
                    <CardHeader className="pb-3">
                      <div className="flex items-center justify-between">
                        <div className="flex items-center gap-2">
                          {getTypeIcon(strategy.type)}
                          <span className="font-cyber text-sm text-neon-blue">{strategy.name}</span>
                        </div>
                        <div className="flex items-center gap-2">
                          <div className={`w-2 h-2 rounded-full animate-glow ${strategy.status === 'active' ? 'bg-matrix-100' : strategy.status === 'triggered' ? 'bg-neon-yellow' : 'bg-neon-orange'}`} />
                          {getStatusIcon(strategy.status)}
                        </div>
                      </div>
                      <p className="text-xs text-cyber-300 font-mono">{strategy.strategy}</p>
                    </CardHeader>
                    <CardContent className="space-y-3">
                      {/* Mathematical Formula */}
                      <div className="bg-cyber-200/50 p-3 rounded border border-neon-blue/30">
                        <div className="text-xs text-neon-blue font-mono mb-2">ALGORITHM:</div>
                        <div className="text-center">
                          <InlineMath math={strategy.formula} />
                        </div>
                      </div>
                      
                      <div className="grid grid-cols-2 gap-2 text-sm font-mono">
                        <div className="flex justify-between">
                          <span className="text-cyber-300">P&L:</span>
                          <span className={strategy.pnl >= 0 ? 'text-matrix-100' : 'text-neon-pink'}>
                            ${strategy.pnl.toFixed(2)}
                          </span>
                        </div>
                        <div className="flex justify-between">
                          <span className="text-cyber-300">TRADES:</span>
                          <span className="text-neon-blue">{strategy.trades}</span>
                        </div>
                        <div className="flex justify-between">
                          <span className="text-cyber-300">WIN_RATE:</span>
                          <span className="text-neon-green">{strategy.winRate.toFixed(1)}%</span>
                        </div>
                        <div className="flex justify-between">
                          <span className="text-cyber-300">RISK:</span>
                          <span className={strategy.riskScore < 3 ? 'text-matrix-100' : strategy.riskScore < 4 ? 'text-neon-orange' : 'text-neon-pink'}>
                            {strategy.riskScore}/5
                          </span>
                        </div>
                      </div>
                      
                      <div className="flex justify-between text-sm font-mono">
                        <span className="text-crypto-300">ALLOCATION:</span>
                        <span className="text-crypto-300">{strategy.allocation}%</span>
                      </div>
                      <Progress value={strategy.allocation} className="h-1 mt-2 bg-cyber-200" />
                      
                      <div className="flex gap-2 pt-2">
                        <Button 
                          variant="outline" 
                          className={`flex-1 text-xs py-1 font-mono ${getStatusColor(strategy.status)} hover:bg-neon-blue/20`}
                        >
                          {strategy.status === 'active' ? 'PAUSE' : 'ACTIVATE'}
                        </Button>
                        <Button 
                          variant="outline" 
                          className="flex-1 text-xs py-1 font-mono border-crypto-300 text-crypto-300 hover:bg-crypto-400/20"
                        >
                          CONFIG
                        </Button>
                      </div>
                    </CardContent>
                  </GlowingCard>
                </motion.div>
              ))}
            </div>
          </CardContent>
        </GlowingCard>

        {/* Sharpe Ratio & Risk Formulas */}
        <GlowingCard glowColor="neon-pink">
          <CardHeader>
            <CardTitle className="text-neon-pink font-cyber flex items-center gap-2">
              <PieChart className="h-5 w-5" />
              RISK_MATHEMATICS_ENGINE
            </CardTitle>
            <CardDescription className="text-cyber-300 font-mono">Advanced portfolio optimization formulas</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="grid md:grid-cols-3 gap-6">
              <div className="space-y-3">
                <h4 className="text-neon-blue font-cyber">SHARPE RATIO</h4>
                <BlockMath math="S = \frac{R_p - R_f}{\sigma_p}" />
                <div className="text-xs font-mono text-cyber-300">
                  Current: <InlineMath math={`S = ${tradeSummary.sharpeRatio}`} />
                </div>
              </div>
              <div className="space-y-3">
                <h4 className="text-neon-green font-cyber">VALUE AT RISK</h4>
                <BlockMath math="VaR = \mu + \sigma \cdot Z_{\alpha}" />
                <div className="text-xs font-mono text-cyber-300">
                  95% VaR: <InlineMath math={`${riskMetrics.dailyVaR}\\%`} />
                </div>
              </div>
              <div className="space-y-3">
                <h4 className="text-neon-orange font-cyber">PORTFOLIO HEAT</h4>
                <BlockMath math="Heat = \frac{\sum |w_i \cdot \sigma_i|}{Capital}" />
                <div className="text-xs font-mono text-cyber-300">
                  Current: <InlineMath math={`${riskMetrics.portfolioHeat}\\%`} />
                </div>
              </div>
            </div>
          </CardContent>
        </GlowingCard>

        {/* System Status Footer */}
        <motion.div 
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 0.5 }}
          className="mt-8 p-4 bg-cyber-200/50 border border-neon-blue/30 rounded-lg backdrop-blur-sm"
        >
          <div className="flex justify-between items-center text-xs font-mono">
            <div className="flex gap-6">
              <span className="text-matrix-100">SYSTEM_STATUS: OPTIMAL</span>
              <span className="text-neon-blue">NEURAL_LINK: SYNCHRONIZED</span>
              <span className="text-neon-green">QUANTUM_STATE: ENTANGLED</span>
            </div>
            <div className="flex gap-4 text-cyber-300">
              <span>CPU: 23%</span>
              <span>RAM: 45%</span>
              <span>NET: 1.2GB/s</span>
            </div>
          </div>
        </motion.div>
          </div>
        ) : (
          /* Backtesting View */
          <Backtesting />
        )}
      </div>
    </div>
  )
} 