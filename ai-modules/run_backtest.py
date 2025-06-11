#!/usr/bin/env python3
"""
LynxTrader Backtest Runner
Easy-to-use script for running comprehensive backtests
"""

import asyncio
import argparse
import sys
import os
from datetime import datetime, timedelta
import pandas as pd

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from backtest_engine import BacktestEngine, BacktestConfig, run_shorting_backtest, run_crypto_backtest

def create_custom_config(args):
    """Create a custom backtest configuration from command line arguments"""
    end_date = datetime.now()
    start_date = end_date - timedelta(days=args.days)
    
    return BacktestConfig(
        start_date=start_date.strftime('%Y-%m-%d'),
        end_date=end_date.strftime('%Y-%m-%d'),
        initial_capital=args.capital,
        commission=args.commission,
        slippage=args.slippage,
        data_sources=args.sources,
        symbols=args.symbols,
        timeframes=args.timeframes,
        enable_shorting=args.enable_shorting,
        max_leverage=args.leverage,
        benchmark=args.benchmark
    )

async def run_custom_backtest(args):
    """Run a custom backtest with user-specified parameters"""
    config = create_custom_config(args)
    
    print("🔧 Custom Backtest Configuration:")
    print(f"  Period: {config.start_date} to {config.end_date}")
    print(f"  Capital: ${config.initial_capital:,.2f}")
    print(f"  Symbols: {', '.join(config.symbols)}")
    print(f"  Timeframes: {', '.join(config.timeframes)}")
    print(f"  Data Sources: {', '.join(config.data_sources)}")
    print(f"  Shorting: {'Enabled' if config.enable_shorting else 'Disabled'}")
    print(f"  Max Leverage: {config.max_leverage}x")
    
    engine = BacktestEngine(config)
    results = await engine.run_backtest()
    
    return results

def print_results(results, title="Backtest Results"):
    """Print backtest results in a formatted way"""
    print(f"\n📈 {title}")
    print("=" * 60)
    print(f"  📊 Performance Metrics:")
    print(f"    Total Return:     {results.total_return:>8.2f}%")
    print(f"    Annual Return:    {results.annual_return:>8.2f}%")
    print(f"    Max Drawdown:     {results.max_drawdown:>8.2f}%")
    print(f"    Sharpe Ratio:     {results.sharpe_ratio:>8.2f}")
    print(f"    Sortino Ratio:    {results.sortino_ratio:>8.2f}")
    print(f"    Calmar Ratio:     {results.calmar_ratio:>8.2f}")
    
    print(f"\n  📈 Trading Statistics:")
    print(f"    Total Trades:     {results.total_trades:>8}")
    print(f"    Win Rate:         {results.win_rate:>8.2f}%")
    print(f"    Profit Factor:    {results.profit_factor:>8.2f}")
    print(f"    Best Trade:       {results.best_trade:>8.2f}")
    print(f"    Worst Trade:      {results.worst_trade:>8.2f}")
    
    print(f"\n  💰 Capital Analysis:")
    print(f"    Initial Capital:  ${results.initial_capital:>10,.2f}")
    print(f"    Final Capital:    ${results.final_capital:>10,.2f}")
    print(f"    Net P&L:          ${results.final_capital - results.initial_capital:>10,.2f}")
    
    if results.strategy_performance:
        print(f"\n  📊 Strategy Breakdown:")
        for strategy, stats in results.strategy_performance.items():
            print(f"    {strategy[:25]:<25} {stats['total_trades']:>3} trades, "
                  f"{stats['avg_confidence']:.2f} confidence")

async def run_quick_test():
    """Run a quick test with minimal data for development"""
    config = BacktestConfig(
        start_date='2024-10-01',
        end_date='2024-10-31',
        initial_capital=10000.0,
        commission=0.001,
        slippage=0.0005,
        data_sources=['yfinance'],
        symbols=['AAPL', 'MSFT'],
        timeframes=['5min'],
        enable_shorting=True,
        max_leverage=2.0,
        benchmark='SPY'
    )
    
    print("⚡ Running Quick Test...")
    engine = BacktestEngine(config)
    results = await engine.run_backtest()
    
    return results

async def run_comprehensive_test():
    """Run a comprehensive test across multiple timeframes and symbols"""
    config = BacktestConfig(
        start_date='2024-01-01',
        end_date='2024-11-01',
        initial_capital=100000.0,
        commission=0.001,
        slippage=0.0005,
        data_sources=['yfinance'],
        symbols=['AAPL', 'MSFT', 'TSLA', 'NVDA', 'QQQ', 'SPY', 'AMZN', 'GOOGL'],
        timeframes=['5min', '15min'],
        enable_shorting=True,
        max_leverage=2.0,
        benchmark='SPY'
    )
    
    print("🌟 Running Comprehensive Test...")
    engine = BacktestEngine(config)
    results = await engine.run_backtest()
    
    return results

def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description='LynxTrader Advanced Backtesting Engine',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Run predefined shorting strategies backtest
  python run_backtest.py --preset shorting
  
  # Run crypto backtest
  python run_backtest.py --preset crypto
  
  # Quick test for development
  python run_backtest.py --preset quick
  
  # Custom backtest
  python run_backtest.py --symbols AAPL MSFT TSLA --days 90 --capital 50000
  
  # Full comprehensive test
  python run_backtest.py --preset comprehensive
        """
    )
    
    parser.add_argument(
        '--preset',
        choices=['shorting', 'crypto', 'quick', 'comprehensive'],
        help='Run a predefined backtest configuration'
    )
    
    parser.add_argument(
        '--symbols',
        nargs='+',
        default=['AAPL', 'MSFT', 'TSLA', 'NVDA'],
        help='List of symbols to backtest (default: AAPL MSFT TSLA NVDA)'
    )
    
    parser.add_argument(
        '--days',
        type=int,
        default=90,
        help='Number of days to backtest (default: 90)'
    )
    
    parser.add_argument(
        '--capital',
        type=float,
        default=100000.0,
        help='Initial capital (default: 100000)'
    )
    
    parser.add_argument(
        '--commission',
        type=float,
        default=0.001,
        help='Commission rate (default: 0.001 = 0.1%%)'
    )
    
    parser.add_argument(
        '--slippage',
        type=float,
        default=0.0005,
        help='Slippage rate (default: 0.0005 = 0.05%%)'
    )
    
    parser.add_argument(
        '--sources',
        nargs='+',
        default=['yfinance'],
        choices=['yfinance', 'alpaca', 'alpha_vantage', 'crypto'],
        help='Data sources to use (default: yfinance)'
    )
    
    parser.add_argument(
        '--timeframes',
        nargs='+',
        default=['5min', '15min'],
        choices=['1min', '5min', '15min', '30min', '1hour', '1day'],
        help='Timeframes to test (default: 5min 15min)'
    )
    
    parser.add_argument(
        '--leverage',
        type=float,
        default=2.0,
        help='Maximum leverage (default: 2.0)'
    )
    
    parser.add_argument(
        '--benchmark',
        default='SPY',
        help='Benchmark symbol (default: SPY)'
    )
    
    parser.add_argument(
        '--enable-shorting',
        action='store_true',
        default=True,
        help='Enable shorting strategies (default: True)'
    )
    
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Enable verbose logging'
    )
    
    args = parser.parse_args()
    
    async def run_selected_backtest():
        """Run the selected backtest based on arguments"""
        start_time = datetime.now()
        
        print("🚀 LynxTrader Advanced Backtesting Engine")
        print("=" * 60)
        print(f"Started at: {start_time.strftime('%Y-%m-%d %H:%M:%S')}")
        
        try:
            if args.preset == 'shorting':
                print("\n📉 Running Shorting Strategies Backtest...")
                results = await run_shorting_backtest()
                print_results(results, "Shorting Strategies Results")
                
            elif args.preset == 'crypto':
                print("\n₿ Running Cryptocurrency Backtest...")
                results = await run_crypto_backtest()
                print_results(results, "Cryptocurrency Results")
                
            elif args.preset == 'quick':
                print("\n⚡ Running Quick Development Test...")
                results = await run_quick_test()
                print_results(results, "Quick Test Results")
                
            elif args.preset == 'comprehensive':
                print("\n🌟 Running Comprehensive Backtest...")
                results = await run_comprehensive_test()
                print_results(results, "Comprehensive Test Results")
                
            else:
                print("\n🔧 Running Custom Backtest...")
                results = await run_custom_backtest(args)
                print_results(results, "Custom Backtest Results")
            
            end_time = datetime.now()
            execution_time = end_time - start_time
            
            print(f"\n⏱️  Execution completed in: {execution_time}")
            print(f"🎯 TensorBoard available at: http://localhost:6006")
            print(f"💾 Results stored in: backtest_results.db")
            
            # Suggest next steps
            print(f"\n📋 Next Steps:")
            print(f"  1. Start TensorBoard: cd ai-modules && python start_tensorboard.py")
            print(f"  2. View backtest visualizations in your browser")
            print(f"  3. Analyze strategy performance in the database")
            print(f"  4. Modify strategies based on results")
            
        except Exception as e:
            print(f"\n❌ Backtest failed: {e}")
            if args.verbose:
                import traceback
                traceback.print_exc()
            sys.exit(1)
    
    # Run the backtest
    asyncio.run(run_selected_backtest())

if __name__ == "__main__":
    main() 