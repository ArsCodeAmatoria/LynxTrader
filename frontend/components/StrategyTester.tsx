'use client'

import React, { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { 
  Brain, 
  TestTube, 
  Zap, 
  Target, 
  Database, 
  Activity,
  PlayCircle,
  RefreshCw,
  Settings,
  BarChart3,
  TrendingUp,
  Shield,
  AlertTriangle,
  Clock
} from 'lucide-react'
import { motion, AnimatePresence } from 'framer-motion'

import { 
  BacktestingAPI,
  Strategy,
  StrategyImplementation,
  RiskProfile,
  getRiskProfileColor
} from '@/lib/api'

interface StrategyTesterProps {
  strategies: Strategy[]
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

const getRiskIcon = (profile: string) => {
  switch (profile.toLowerCase()) {
    case 'conservative':
      return <Shield className="h-4 w-4" />
    case 'moderate':
      return <Target className="h-4 w-4" />
    case 'aggressive':
      return <AlertTriangle className="h-4 w-4" />
    default:
      return <Activity className="h-4 w-4" />
  }
}

const getImplementationIcon = (implementation: string) => {
  switch (implementation.toLowerCase()) {
    case 'python_ai':
      return <Brain className="h-4 w-4" />
    case 'haskell_dsl':
      return <Database className="h-4 w-4" />
    case 'backtest_engine':
      return <Zap className="h-4 w-4" />
    default:
      return <Activity className="h-4 w-4" />
  }
}

export default function StrategyTester({ strategies, onRefresh }: StrategyTesterProps) {
  const [implementations, setImplementations] = useState<StrategyImplementation[]>([])
  const [riskProfiles, setRiskProfiles] = useState<RiskProfile[]>([])
  const [selectedStrategy, setSelectedStrategy] = useState<Strategy | null>(null)
  const [testMode, setTestMode] = useState<'single' | 'implementation' | 'risk_profile' | 'all'>('single')
  const [isRunning, setIsRunning] = useState(false)
  const [testResults, setTestResults] = useState<any[]>([])
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    loadMetadata()
  }, [])

  const loadMetadata = async () => {
    try {
      const [implData, riskData] = await Promise.all([
        BacktestingAPI.getStrategyImplementations(),
        BacktestingAPI.getRiskProfiles()
      ])
      
      setImplementations(implData.implementations)
      setRiskProfiles(riskData.risk_profiles)
    } catch (err: any) {
      setError(err.message || 'Failed to load metadata')
    }
  }

  const handleRunTest = async () => {
    try {
      setError(null)
      setIsRunning(true)

             let request: any
       switch (testMode) {
         case 'single':
           if (!selectedStrategy) {
             throw new Error('Please select a strategy')
           }
           request = {
             strategy_name: selectedStrategy.name,
             test_type: 'single' as const
           }
           break
         case 'implementation':
           request = {
             implementation: selectedImplementation,
             test_type: 'implementation' as const
           }
           break
         case 'risk_profile':
           request = {
             risk_profile: selectedRiskProfile,
             test_type: 'risk_profile' as const
           }
           break
         case 'all':
           request = { test_type: 'all' as const }
           break
       }

      const response = await BacktestingAPI.testStrategy(request)
      
      // In a real implementation, you'd poll for results
      // For now, just show success
      setTestResults([{
        id: response.test_id,
        status: 'completed',
        message: response.message
      }])
      
    } catch (err: any) {
      setError(err.message || 'Failed to run strategy test')
    } finally {
      setIsRunning(false)
    }
  }

  // Group strategies by implementation
  const strategyGroups = strategies.reduce((groups, strategy) => {
    if (!groups[strategy.implementation]) {
      groups[strategy.implementation] = []
    }
    groups[strategy.implementation].push(strategy)
    return groups
  }, {} as Record<string, Strategy[]>)

  // Group strategies by risk profile
  const riskGroups = strategies.reduce((groups, strategy) => {
    if (!groups[strategy.risk_profile]) {
      groups[strategy.risk_profile] = []
    }
    groups[strategy.risk_profile].push(strategy)
    return groups
  }, {} as Record<string, Strategy[]>)

  const [selectedImplementation, setSelectedImplementation] = useState<string>('')
  const [selectedRiskProfile, setSelectedRiskProfile] = useState<string>('')

  return (
    <div className="space-y-6">
      {/* Test Mode Selection */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <Button
          onClick={() => setTestMode('single')}
          variant={testMode === 'single' ? "default" : "outline"}
          className={testMode === 'single' ? "bg-neon-blue/20 text-neon-blue border-neon-blue" : "border-cyber-200/30 text-cyber-200"}
        >
          <TestTube className="h-4 w-4 mr-2" />
          Single Strategy
        </Button>
        <Button
          onClick={() => setTestMode('implementation')}
          variant={testMode === 'implementation' ? "default" : "outline"}
          className={testMode === 'implementation' ? "bg-matrix-100/20 text-matrix-100 border-matrix-100" : "border-cyber-200/30 text-cyber-200"}
        >
          <Brain className="h-4 w-4 mr-2" />
          By Implementation
        </Button>
        <Button
          onClick={() => setTestMode('risk_profile')}
          variant={testMode === 'risk_profile' ? "default" : "outline"}
          className={testMode === 'risk_profile' ? "bg-neon-purple/20 text-neon-purple border-neon-purple" : "border-cyber-200/30 text-cyber-200"}
        >
          <Target className="h-4 w-4 mr-2" />
          By Risk Profile
        </Button>
        <Button
          onClick={() => setTestMode('all')}
          variant={testMode === 'all' ? "default" : "outline"}
          className={testMode === 'all' ? "bg-cyber-accent/20 text-cyber-accent border-cyber-accent" : "border-cyber-200/30 text-cyber-200"}
        >
          <Zap className="h-4 w-4 mr-2" />
          Test All
        </Button>
      </div>

      {error && (
        <motion.div
          initial={{ opacity: 0, y: -10 }}
          animate={{ opacity: 1, y: 0 }}
          className="bg-red-500/10 border border-red-500/30 rounded-lg p-4"
        >
          <div className="flex items-center">
            <AlertTriangle className="h-5 w-5 text-red-400 mr-2" />
            <span className="text-red-400">{error}</span>
          </div>
        </motion.div>
      )}

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Configuration Panel */}
        <GlowingCard>
          <CardHeader>
            <CardTitle className="text-neon-blue">
              {testMode === 'single' && 'Single Strategy Test'}
              {testMode === 'implementation' && 'Implementation Test'}
              {testMode === 'risk_profile' && 'Risk Profile Test'}
              {testMode === 'all' && 'Comprehensive Test'}
            </CardTitle>
            <CardDescription className="text-cyber-200">
              {testMode === 'single' && 'Test an individual strategy'}
              {testMode === 'implementation' && 'Test all strategies of a specific implementation'}
              {testMode === 'risk_profile' && 'Test all strategies with a specific risk profile'}
              {testMode === 'all' && 'Test all available strategies'}
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            {testMode === 'single' && (
              <div className="space-y-3">
                <h4 className="text-cyber-200 text-sm">Available Strategies</h4>
                <div className="max-h-60 overflow-y-auto space-y-2">
                  {strategies.map((strategy) => (
                    <motion.div
                      key={strategy.name}
                      whileHover={{ scale: 1.02 }}
                      whileTap={{ scale: 0.98 }}
                      onClick={() => setSelectedStrategy(strategy)}
                      className={`p-3 rounded-lg border cursor-pointer transition-all ${
                        selectedStrategy?.name === strategy.name
                          ? 'border-neon-blue bg-neon-blue/10'
                          : 'border-cyber-200/30 hover:border-neon-blue/50'
                      }`}
                    >
                      <div className="flex items-center justify-between mb-1">
                        <div className="flex items-center space-x-2">
                          {getImplementationIcon(strategy.implementation)}
                          <span className="font-medium text-cyber-100 text-sm">{strategy.name}</span>
                        </div>
                        <div className="flex items-center space-x-1">
                          <Badge className={`${getRiskProfileColor(strategy.risk_profile)} border-current/30 text-xs`}>
                            {strategy.risk_profile}
                          </Badge>
                        </div>
                      </div>
                      <div className="flex items-center justify-between text-xs text-cyber-200">
                        <span className="capitalize">{strategy.implementation.replace('_', ' ')}</span>
                        <span>{strategy.symbols.length} symbols</span>
                      </div>
                    </motion.div>
                  ))}
                </div>
              </div>
            )}

            {testMode === 'implementation' && (
              <div className="space-y-3">
                <h4 className="text-cyber-200 text-sm">Select Implementation</h4>
                <div className="space-y-2">
                  {implementations.map((impl) => (
                    <motion.div
                      key={impl.name}
                      whileHover={{ scale: 1.02 }}
                      whileTap={{ scale: 0.98 }}
                      onClick={() => setSelectedImplementation(impl.name)}
                      className={`p-3 rounded-lg border cursor-pointer transition-all ${
                        selectedImplementation === impl.name
                          ? 'border-matrix-100 bg-matrix-100/10'
                          : 'border-cyber-200/30 hover:border-matrix-100/50'
                      }`}
                    >
                      <div className="flex items-center justify-between mb-1">
                        <div className="flex items-center space-x-2">
                          {getImplementationIcon(impl.name)}
                          <span className="font-medium text-cyber-100 capitalize">{impl.name.replace('_', ' ')}</span>
                        </div>
                        <Badge className="bg-matrix-100/20 text-matrix-100 border-matrix-100/30 text-xs">
                          {impl.count} strategies
                        </Badge>
                      </div>
                      <p className="text-xs text-cyber-200">{impl.description}</p>
                    </motion.div>
                  ))}
                </div>
              </div>
            )}

            {testMode === 'risk_profile' && (
              <div className="space-y-3">
                <h4 className="text-cyber-200 text-sm">Select Risk Profile</h4>
                <div className="space-y-2">
                  {riskProfiles.map((profile) => (
                    <motion.div
                      key={profile.name}
                      whileHover={{ scale: 1.02 }}
                      whileTap={{ scale: 0.98 }}
                      onClick={() => setSelectedRiskProfile(profile.name)}
                      className={`p-3 rounded-lg border cursor-pointer transition-all ${
                        selectedRiskProfile === profile.name
                          ? 'border-neon-purple bg-neon-purple/10'
                          : 'border-cyber-200/30 hover:border-neon-purple/50'
                      }`}
                    >
                      <div className="flex items-center justify-between mb-1">
                        <div className="flex items-center space-x-2">
                          {getRiskIcon(profile.name)}
                          <span className="font-medium text-cyber-100 capitalize">{profile.name}</span>
                        </div>
                        <Badge className={`${getRiskProfileColor(profile.name)} border-current/30 text-xs`}>
                          {profile.strategies} strategies
                        </Badge>
                      </div>
                      <p className="text-xs text-cyber-200">{profile.description}</p>
                    </motion.div>
                  ))}
                </div>
              </div>
            )}

            {testMode === 'all' && (
              <div className="space-y-4">
                <div className="text-center p-6 bg-cyber-accent/10 border border-cyber-accent/30 rounded-lg">
                  <Zap className="h-12 w-12 text-cyber-accent mx-auto mb-3" />
                  <h4 className="text-cyber-accent font-semibold mb-2">Comprehensive Testing</h4>
                  <p className="text-cyber-200 text-sm">
                    This will test all {strategies.length} available strategies across all implementations and risk profiles.
                  </p>
                </div>
                
                <div className="grid grid-cols-3 gap-3 text-center">
                  <div className="p-3 bg-neon-blue/10 border border-neon-blue/30 rounded-lg">
                    <div className="text-neon-blue font-bold text-lg">{implementations.length}</div>
                    <div className="text-cyber-200 text-xs">Implementations</div>
                  </div>
                  <div className="p-3 bg-matrix-100/10 border border-matrix-100/30 rounded-lg">
                    <div className="text-matrix-100 font-bold text-lg">{riskProfiles.length}</div>
                    <div className="text-cyber-200 text-xs">Risk Profiles</div>
                  </div>
                  <div className="p-3 bg-cyber-accent/10 border border-cyber-accent/30 rounded-lg">
                    <div className="text-cyber-accent font-bold text-lg">{strategies.length}</div>
                    <div className="text-cyber-200 text-xs">Total Strategies</div>
                  </div>
                </div>
              </div>
            )}

            <div className="pt-4 border-t border-cyber-200/20">
              <Button
                onClick={handleRunTest}
                disabled={isRunning || (testMode === 'single' && !selectedStrategy) || 
                         (testMode === 'implementation' && !selectedImplementation) ||
                         (testMode === 'risk_profile' && !selectedRiskProfile)}
                className={`w-full ${
                  isRunning 
                    ? 'bg-yellow-500/20 text-yellow-400 border-yellow-500/30' 
                    : 'bg-neon-blue/20 text-neon-blue border-neon-blue/30 hover:bg-neon-blue/30'
                }`}
              >
                {isRunning ? (
                  <>
                    <RefreshCw className="h-4 w-4 mr-2 animate-spin" />
                    Running Test...
                  </>
                ) : (
                  <>
                    <PlayCircle className="h-4 w-4 mr-2" />
                    Start Test
                  </>
                )}
              </Button>
            </div>
          </CardContent>
        </GlowingCard>

        {/* Strategy Overview */}
        <GlowingCard glowColor="matrix-100">
          <CardHeader>
            <CardTitle className="text-matrix-100">Strategy Overview</CardTitle>
            <CardDescription className="text-cyber-200">
              Available strategies grouped by implementation and risk profile
            </CardDescription>
          </CardHeader>
          <CardContent className="space-y-4">
            {/* By Implementation */}
            <div>
              <h4 className="text-cyber-200 text-sm mb-3">By Implementation</h4>
              <div className="space-y-2">
                {Object.entries(strategyGroups).map(([implementation, strategies]) => (
                  <div
                    key={implementation}
                    className="flex items-center justify-between p-2 bg-cyber-100/10 border border-cyber-200/30 rounded-lg"
                  >
                    <div className="flex items-center space-x-2">
                      {getImplementationIcon(implementation)}
                      <span className="text-cyber-100 text-sm capitalize">{implementation.replace('_', ' ')}</span>
                    </div>
                    <Badge className="bg-matrix-100/20 text-matrix-100 border-matrix-100/30 text-xs">
                      {strategies.length}
                    </Badge>
                  </div>
                ))}
              </div>
            </div>

            {/* By Risk Profile */}
            <div>
              <h4 className="text-cyber-200 text-sm mb-3">By Risk Profile</h4>
              <div className="space-y-2">
                {Object.entries(riskGroups).map(([riskProfile, strategies]) => (
                  <div
                    key={riskProfile}
                    className="flex items-center justify-between p-2 bg-cyber-100/10 border border-cyber-200/30 rounded-lg"
                  >
                    <div className="flex items-center space-x-2">
                      {getRiskIcon(riskProfile)}
                      <span className="text-cyber-100 text-sm capitalize">{riskProfile}</span>
                    </div>
                    <Badge className={`${getRiskProfileColor(riskProfile)} border-current/30 text-xs`}>
                      {strategies.length}
                    </Badge>
                  </div>
                ))}
              </div>
            </div>

            {/* Selected Strategy Details */}
            {selectedStrategy && testMode === 'single' && (
              <div className="pt-4 border-t border-cyber-200/20">
                <h4 className="text-cyber-200 text-sm mb-3">Selected Strategy</h4>
                <div className="p-3 bg-neon-blue/10 border border-neon-blue/30 rounded-lg space-y-2">
                  <div className="flex items-center justify-between">
                    <span className="font-medium text-neon-blue">{selectedStrategy.name}</span>
                    <Badge className={`${getRiskProfileColor(selectedStrategy.risk_profile)} border-current/30 text-xs`}>
                      {selectedStrategy.risk_profile}
                    </Badge>
                  </div>
                  <div className="text-xs text-cyber-200">
                    <div>Implementation: {selectedStrategy.implementation.replace('_', ' ')}</div>
                    <div>Symbols: {selectedStrategy.symbols.join(', ')}</div>
                    <div>Timeframes: {selectedStrategy.timeframes.join(', ')}</div>
                  </div>
                  {selectedStrategy.description && (
                    <p className="text-xs text-cyber-200 italic">{selectedStrategy.description}</p>
                  )}
                </div>
              </div>
            )}

            {/* Test Results */}
            {testResults.length > 0 && (
              <div className="pt-4 border-t border-cyber-200/20">
                <h4 className="text-cyber-200 text-sm mb-3">Test Results</h4>
                <div className="space-y-2">
                  {testResults.map((result, index) => (
                    <div
                      key={index}
                      className="p-3 bg-green-500/10 border border-green-500/30 rounded-lg"
                    >
                      <div className="flex items-center justify-between">
                        <span className="text-green-400 text-sm font-medium">Test Completed</span>
                        <Badge className="bg-green-500/20 text-green-400 border-green-500/30 text-xs">
                          Success
                        </Badge>
                      </div>
                      <p className="text-green-300 text-xs mt-1">{result.message}</p>
                    </div>
                  ))}
                </div>
              </div>
            )}
          </CardContent>
        </GlowingCard>
      </div>
    </div>
  )
} 