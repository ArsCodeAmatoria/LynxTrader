#!/usr/bin/env python3
"""
LynxTrader Strategy Testing CLI
Command-line interface for testing individual or groups of strategies
"""

import asyncio
import argparse
import sys
import os
from datetime import datetime
from typing import List, Optional

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from strategy_tester import ComprehensiveStrategyTester, StrategyRegistry
from run_backtest import main as run_backtest_main

def print_available_strategies(registry: StrategyRegistry):
    """Print all available strategies organized by implementation"""
    print("\n=== Available Strategies ===")
    
    implementations = set(s.implementation for s in registry.strategies)
    
    for impl in sorted(implementations):
        print(f"\n{impl.upper()} Strategies:")
        strategies = registry.get_strategies_by_implementation(impl)
        
        for i, strategy in enumerate(strategies, 1):
            timeframes = ", ".join(strategy.timeframes)
            symbols = ", ".join(strategy.symbols[:3])  # Show first 3 symbols
            if len(strategy.symbols) > 3:
                symbols += f" (+{len(strategy.symbols)-3} more)"
            
            print(f"  {i}. {strategy.name}")
            print(f"     Risk: {strategy.risk_profile} | Timeframes: {timeframes}")
            print(f"     Symbols: {symbols}")

async def test_specific_strategy(strategy_name: str, duration_days: int = 30):
    """Test a specific strategy by name"""
    tester = ComprehensiveStrategyTester()
    
    # Find strategy
    strategy = None
    for s in tester.registry.strategies:
        if s.name.lower() == strategy_name.lower():
            strategy = s
            break
    
    if not strategy:
        print(f"Strategy '{strategy_name}' not found.")
        print_available_strategies(tester.registry)
        return
    
    print(f"Testing strategy: {strategy.name} ({strategy.implementation})")
    print(f"Timeframes: {', '.join(strategy.timeframes)}")
    print(f"Symbols: {', '.join(strategy.symbols)}")
    print(f"Test duration: {duration_days} days")
    print("=" * 50)
    
    # Test the strategy
    result = await tester._test_strategy(strategy, duration_days)
    
    if result:
        print(f"\n=== Results for {result.strategy_name} ===")
        print(f"Total signals: {result.total_signals}")
        print(f"Signal accuracy: {result.signal_accuracy:.2%}")
        print(f"Average confidence: {result.avg_confidence:.2%}")
        print(f"Errors: {result.error_count}")
        
        if result.performance_metrics:
            print(f"\nPerformance Metrics:")
            for metric, value in result.performance_metrics.items():
                if isinstance(value, float):
                    if 'ratio' in metric or 'return' in metric:
                        print(f"  {metric}: {value:.3f}")
                    elif 'rate' in metric or 'accuracy' in metric:
                        print(f"  {metric}: {value:.2%}")
                    else:
                        print(f"  {metric}: {value:.2f}")
                else:
                    print(f"  {metric}: {value}")
    else:
        print("Failed to test strategy.")

async def test_by_implementation(implementation: str, duration_days: int = 30):
    """Test all strategies of a specific implementation"""
    tester = ComprehensiveStrategyTester()
    
    print(f"Testing all {implementation} strategies...")
    results = await tester.test_all_strategies(
        filter_implementation=implementation,
        test_duration_days=duration_days
    )
    
    if results:
        print(f"\n=== Results for {implementation} Strategies ===")
        for result in sorted(results, key=lambda r: r.signal_accuracy, reverse=True):
            print(f"\n{result.strategy_name}:")
            print(f"  Signals: {result.total_signals}")
            print(f"  Accuracy: {result.signal_accuracy:.2%}")
            print(f"  Confidence: {result.avg_confidence:.2%}")
            print(f"  Errors: {result.error_count}")
        
        # Generate summary report
        tester.test_results = results
        report = tester.generate_test_report(f"{implementation}_test_report.json")
        print(f"\nDetailed report saved to: {implementation}_test_report.json")
    else:
        print(f"No results for {implementation} strategies.")

async def test_by_risk_profile(risk_profile: str, duration_days: int = 30):
    """Test all strategies of a specific risk profile"""
    tester = ComprehensiveStrategyTester()
    
    print(f"Testing all {risk_profile} risk strategies...")
    results = await tester.test_all_strategies(
        filter_risk=risk_profile,
        test_duration_days=duration_days
    )
    
    if results:
        print(f"\n=== Results for {risk_profile} Risk Strategies ===")
        for result in sorted(results, key=lambda r: r.signal_accuracy, reverse=True):
            print(f"\n{result.strategy_name} ({result.implementation}):")
            print(f"  Signals: {result.total_signals}")
            print(f"  Accuracy: {result.signal_accuracy:.2%}")
            print(f"  Confidence: {result.avg_confidence:.2%}")
        
        # Generate summary report
        tester.test_results = results
        report = tester.generate_test_report(f"{risk_profile}_risk_test_report.json")
        print(f"\nDetailed report saved to: {risk_profile}_risk_test_report.json")
    else:
        print(f"No results for {risk_profile} risk strategies.")

async def run_quick_test():
    """Run a quick test of key strategies"""
    print("Running quick test of key strategies...")
    
    tester = ComprehensiveStrategyTester()
    
    # Test one strategy from each implementation
    key_strategies = [
        "Fake Breakout Reversal",  # python_ai
        "VWAP Bounce Scalper",     # haskell_dsl
        "Multi-Source Backtest"    # backtest_engine
    ]
    
    results = []
    for strategy_name in key_strategies:
        for strategy in tester.registry.strategies:
            if strategy.name == strategy_name:
                print(f"\nTesting {strategy_name}...")
                result = await tester._test_strategy(strategy, 7)  # 7 day test
                if result:
                    results.append(result)
                break
    
    # Print results
    print(f"\n=== Quick Test Results ===")
    for result in results:
        print(f"\n{result.strategy_name} ({result.implementation}):")
        print(f"  Signals: {result.total_signals}")
        print(f"  Accuracy: {result.signal_accuracy:.2%}")
        print(f"  Confidence: {result.avg_confidence:.2%}")

def run_backtest_preset(preset: str):
    """Run a specific backtest preset"""
    print(f"Running backtest preset: {preset}")
    
    # Map preset names to run_backtest arguments
    preset_map = {
        'quick': ['--preset', 'quick'],
        'shorting': ['--preset', 'shorting'],
        'crypto': ['--preset', 'crypto'],
        'comprehensive': ['--preset', 'comprehensive']
    }
    
    if preset in preset_map:
        # Import and run the backtest
        import subprocess
        import sys
        
        cmd = [sys.executable, 'run_backtest.py'] + preset_map[preset]
        try:
            result = subprocess.run(cmd, cwd=os.path.dirname(__file__), capture_output=True, text=True)
            print(result.stdout)
            if result.stderr:
                print("Errors:", result.stderr)
        except Exception as e:
            print(f"Error running backtest: {e}")
    else:
        print(f"Unknown preset: {preset}")
        print("Available presets: quick, shorting, crypto, comprehensive")

async def main():
    """Main CLI function"""
    parser = argparse.ArgumentParser(description="LynxTrader Strategy Testing CLI")
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # List strategies command
    list_parser = subparsers.add_parser('list', help='List all available strategies')
    
    # Test specific strategy command
    test_parser = subparsers.add_parser('test', help='Test a specific strategy')
    test_parser.add_argument('strategy_name', help='Name of the strategy to test')
    test_parser.add_argument('--days', type=int, default=30, help='Test duration in days (default: 30)')
    
    # Test by implementation command
    impl_parser = subparsers.add_parser('impl', help='Test all strategies of an implementation')
    impl_parser.add_argument('implementation', choices=['python_ai', 'haskell_dsl', 'backtest_engine'],
                            help='Implementation type to test')
    impl_parser.add_argument('--days', type=int, default=30, help='Test duration in days (default: 30)')
    
    # Test by risk profile command
    risk_parser = subparsers.add_parser('risk', help='Test all strategies of a risk profile')
    risk_parser.add_argument('risk_profile', choices=['conservative', 'moderate', 'aggressive'],
                            help='Risk profile to test')
    risk_parser.add_argument('--days', type=int, default=30, help='Test duration in days (default: 30)')
    
    # Quick test command
    quick_parser = subparsers.add_parser('quick', help='Run quick test of key strategies')
    
    # Backtest preset command
    backtest_parser = subparsers.add_parser('backtest', help='Run backtest with preset')
    backtest_parser.add_argument('preset', choices=['quick', 'shorting', 'crypto', 'comprehensive'],
                                help='Backtest preset to run')
    
    # All strategies test command
    all_parser = subparsers.add_parser('all', help='Test all strategies')
    all_parser.add_argument('--days', type=int, default=30, help='Test duration in days (default: 30)')
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    registry = StrategyRegistry()
    
    if args.command == 'list':
        print_available_strategies(registry)
    
    elif args.command == 'test':
        await test_specific_strategy(args.strategy_name, args.days)
    
    elif args.command == 'impl':
        await test_by_implementation(args.implementation, args.days)
    
    elif args.command == 'risk':
        await test_by_risk_profile(args.risk_profile, args.days)
    
    elif args.command == 'quick':
        await run_quick_test()
    
    elif args.command == 'backtest':
        run_backtest_preset(args.preset)
    
    elif args.command == 'all':
        tester = ComprehensiveStrategyTester()
        results = await tester.test_all_strategies(test_duration_days=args.days)
        report = tester.generate_test_report()
        
        print(f"\n=== Complete Test Results ===")
        print(f"Strategies tested: {len(results)}")
        print(f"Total signals: {sum(r.total_signals for r in results)}")
        print(f"Average accuracy: {sum(r.signal_accuracy for r in results) / len(results):.2%}")
        print(f"Report saved to: strategy_test_report.json")

if __name__ == "__main__":
    asyncio.run(main()) 