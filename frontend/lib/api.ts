import axios from 'axios'

// API Configuration
const BACKTESTING_API_BASE = process.env.NEXT_PUBLIC_BACKTESTING_API || 'http://localhost:8000'
const RUST_API_BASE = process.env.NEXT_PUBLIC_RUST_API || 'http://localhost:3001'

// Create axios instances
const backtestingApi = axios.create({
  baseURL: BACKTESTING_API_BASE,
  timeout: 30000,
  headers: {
    'Content-Type': 'application/json',
  }
})

const rustApi = axios.create({
  baseURL: RUST_API_BASE,
  timeout: 10000,
  headers: {
    'Content-Type': 'application/json',
  }
})

// Types for backtesting
export interface BacktestRequest {
  preset?: string
  symbols?: string[]
  start_date?: string
  end_date?: string
  initial_capital?: number
  data_source?: string
  timeframes?: string[]
  strategies?: string[]
}

export interface BacktestResult {
  id: string
  status: 'starting' | 'running' | 'completed' | 'error'
  start_time: string
  end_time?: string
  config: Record<string, any>
  results?: {
    total_return: number
    annual_return: number
    max_drawdown: number
    win_rate: number
    total_trades: number
    execution_time: number
    symbols_processed: number
    timeframes_used: string[]
  }
  error?: string
}

export interface BacktestPreset {
  name: string
  description: string
  symbols: string[]
  duration_days: number
  initial_capital: number
}

export interface Strategy {
  name: string
  implementation: string
  risk_profile: string
  symbols: string[]
  timeframes: string[]
  description: string
}

export interface StrategyTestRequest {
  strategy_name?: string
  implementation?: string
  risk_profile?: string
  test_type: 'single' | 'implementation' | 'risk_profile' | 'all'
}

export interface StrategyImplementation {
  name: string
  description: string
  count: number
}

export interface RiskProfile {
  name: string
  description: string
  strategies: number
}

// Backtesting API functions
export class BacktestingAPI {
  // Health check
  static async getHealth() {
    try {
      const response = await backtestingApi.get('/health')
      return response.data
    } catch (error) {
      console.error('Health check failed:', error)
      throw error
    }
  }

  // Get available presets
  static async getPresets(): Promise<{ presets: BacktestPreset[] }> {
    const response = await backtestingApi.get('/backtests/presets')
    return response.data
  }

  // Start a backtest
  static async startBacktest(request: BacktestRequest): Promise<{ backtest_id: string; status: string; message: string }> {
    const response = await backtestingApi.post('/backtests/run', request)
    return response.data
  }

  // Get backtest status
  static async getBacktestStatus(backtestId: string): Promise<BacktestResult> {
    const response = await backtestingApi.get(`/backtests/${backtestId}`)
    return response.data
  }

  // List all backtests
  static async listBacktests(): Promise<{
    backtests: BacktestResult[]
    total: number
    running: number
    completed: number
  }> {
    const response = await backtestingApi.get('/backtests')
    return response.data
  }

  // Delete a backtest
  static async deleteBacktest(backtestId: string): Promise<{ message: string }> {
    const response = await backtestingApi.delete(`/backtests/${backtestId}`)
    return response.data
  }

  // Get strategies
  static async getStrategies(): Promise<{ strategies: Strategy[]; total: number }> {
    const response = await backtestingApi.get('/strategies')
    return response.data
  }

  // Test strategy
  static async testStrategy(request: StrategyTestRequest): Promise<{ test_id: string; status: string; message: string }> {
    const response = await backtestingApi.post('/strategies/test', request)
    return response.data
  }

  // Get strategy implementations
  static async getStrategyImplementations(): Promise<{ implementations: StrategyImplementation[] }> {
    const response = await backtestingApi.get('/strategies/implementations')
    return response.data
  }

  // Get risk profiles
  static async getRiskProfiles(): Promise<{ risk_profiles: RiskProfile[] }> {
    const response = await backtestingApi.get('/strategies/risk-profiles')
    return response.data
  }
}

// Types for Rust API integration
export interface RustStrategy {
  id: string
  name: string
  strategy_type: string
  enabled: boolean
  symbols: string[]
  parameters: Record<string, number>
}

export interface StrategyPerformance {
  strategy_id: string
  total_return: number
  win_rate: number
  sharpe_ratio: number
  max_drawdown: number
  total_trades: number
}

export interface TradingSignal {
  id: string
  strategy_id: string
  symbol: string
  signal_type: 'buy' | 'sell' | 'hold'
  confidence: number
  timestamp: string
  price: number
  quantity: number
}

export interface PortfolioMetrics {
  total_value: number
  cash_balance: number
  daily_pnl: number
  total_pnl: number
  positions: number
}

// Rust API functions (for existing strategy management)
export class RustAPI {
  // Get strategies
  static async getStrategies(): Promise<{ strategies: RustStrategy[]; total: number }> {
    const response = await rustApi.get('/strategies')
    return response.data
  }

  // Get strategy performance
  static async getPerformance(): Promise<{
    performances: StrategyPerformance[]
    portfolio: PortfolioMetrics
  }> {
    const response = await rustApi.get('/performance')
    return response.data
  }

  // Get trading signals
  static async getSignals(): Promise<{ signals: TradingSignal[]; active_count: number }> {
    const response = await rustApi.get('/signals')
    return response.data
  }

  // Enable/disable strategy
  static async toggleStrategy(strategyId: string, enabled: boolean): Promise<RustStrategy> {
    const endpoint = enabled ? `/strategies/${strategyId}/enable` : `/strategies/${strategyId}/disable`
    const response = await rustApi.post(endpoint)
    return response.data
  }

  // Get portfolio metrics
  static async getPortfolioMetrics(): Promise<PortfolioMetrics> {
    const response = await rustApi.get('/portfolio')
    return response.data
  }

  // Health check
  static async getHealth() {
    const response = await rustApi.get('/health')
    return response.data
  }
}

// Error handling utilities
export class APIError extends Error {
  constructor(message: string, public status?: number, public response?: any) {
    super(message)
    this.name = 'APIError'
  }
}

// Add response interceptors for error handling
backtestingApi.interceptors.response.use(
  (response) => response,
  (error) => {
    const message = error.response?.data?.detail || error.message || 'An error occurred'
    throw new APIError(message, error.response?.status, error.response?.data)
  }
)

rustApi.interceptors.response.use(
  (response) => response,
  (error) => {
    const message = error.response?.data?.message || error.message || 'An error occurred'
    throw new APIError(message, error.response?.status, error.response?.data)
  }
)

// Utility functions
export const formatCurrency = (value: number): string => {
  return new Intl.NumberFormat('en-US', {
    style: 'currency',
    currency: 'USD',
    minimumFractionDigits: 2,
    maximumFractionDigits: 2,
  }).format(value)
}

export const formatPercentage = (value: number): string => {
  return new Intl.NumberFormat('en-US', {
    style: 'percent',
    minimumFractionDigits: 2,
    maximumFractionDigits: 2,
  }).format(value / 100)
}

export const formatNumber = (value: number): string => {
  return new Intl.NumberFormat('en-US', {
    minimumFractionDigits: 0,
    maximumFractionDigits: 2,
  }).format(value)
}

export const getStatusColor = (status: string): string => {
  switch (status) {
    case 'running':
    case 'starting':
      return 'text-yellow-400'
    case 'completed':
      return 'text-green-400'
    case 'error':
      return 'text-red-400'
    default:
      return 'text-gray-400'
  }
}

export const getRiskProfileColor = (profile: string): string => {
  switch (profile.toLowerCase()) {
    case 'conservative':
      return 'text-green-400'
    case 'moderate':
      return 'text-yellow-400'
    case 'aggressive':
      return 'text-red-400'
    default:
      return 'text-gray-400'
  }
} 