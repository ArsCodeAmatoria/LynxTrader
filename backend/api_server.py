from fastapi import FastAPI, HTTPException, BackgroundTasks
from fastapi.middleware.cors import CORSMiddleware
from pydantic import BaseModel
from typing import List, Dict, Optional, Any
import asyncio
import json
import os
import sys
from datetime import datetime, timedelta
import uuid

# Add the current directory to the path to import our backtesting modules
sys.path.append(os.path.dirname(os.path.abspath(__file__)))
sys.path.append(os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', 'ai-modules'))

try:
    from backtest_engine import BacktestEngine, BacktestConfig
    from simple_backtest import SimpleBacktestEngine, SimpleBacktestConfig
    from strategy_tester import StrategyRegistry, ComprehensiveStrategyTester
except ImportError as e:
    print(f"Warning: Could not import backtesting modules: {e}")
    # Create dummy classes for development
    class BacktestEngine:
        pass
    class BacktestConfig:
        pass
    class SimpleBacktestEngine:
        pass
    class SimpleBacktestConfig:
        pass
    class StrategyRegistry:
        pass
    class ComprehensiveStrategyTester:
        pass

app = FastAPI(
    title="LynxTrader Backtesting API",
    description="Advanced backtesting API for LynxTrader trading strategies",
    version="1.0.0"
)

# CORS middleware for frontend integration
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:3000", "http://localhost:3001"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Pydantic models for API requests/responses
class BacktestRequest(BaseModel):
    preset: Optional[str] = None
    symbols: Optional[List[str]] = None
    start_date: Optional[str] = None
    end_date: Optional[str] = None
    initial_capital: Optional[float] = 100000
    data_source: Optional[str] = "yfinance"
    timeframes: Optional[List[str]] = None
    strategies: Optional[List[str]] = None

class BacktestResult(BaseModel):
    id: str
    status: str
    start_time: str
    end_time: Optional[str] = None
    config: Dict[str, Any]
    results: Optional[Dict[str, Any]] = None
    error: Optional[str] = None

class StrategyTestRequest(BaseModel):
    strategy_name: Optional[str] = None
    implementation: Optional[str] = None
    risk_profile: Optional[str] = None
    test_type: str = "single"

class StrategyTestResult(BaseModel):
    strategy_name: str
    implementation: str
    signals_generated: int
    accuracy: float
    execution_time: float
    status: str
    error: Optional[str] = None

# Global storage for running backtests
running_backtests: Dict[str, BacktestResult] = {}
completed_backtests: Dict[str, BacktestResult] = {}

@app.get("/")
async def root():
    return {
        "message": "LynxTrader Backtesting API",
        "version": "1.0.0",
        "endpoints": {
            "backtests": "/backtests",
            "strategies": "/strategies",
            "health": "/health"
        }
    }

@app.get("/health")
async def health_check():
    return {
        "status": "healthy",
        "running_backtests": len(running_backtests),
        "completed_backtests": len(completed_backtests),
        "timestamp": datetime.now().isoformat()
    }

@app.get("/backtests/presets")
async def get_backtest_presets():
    """Get available backtest presets"""
    return {
        "presets": [
            {
                "name": "quick",
                "description": "Quick test with 2 stocks, 30 days, $10K",
                "symbols": ["AAPL", "GOOGL"],
                "duration_days": 30,
                "initial_capital": 10000
            },
            {
                "name": "shorting",
                "description": "Shorting strategies test, 6 months, $100K",
                "symbols": ["AAPL", "GOOGL", "MSFT", "TSLA", "NVDA", "META"],
                "duration_days": 180,
                "initial_capital": 100000
            },
            {
                "name": "crypto",
                "description": "Crypto trading test, 6 months, $50K",
                "symbols": ["BTC-USD", "ETH-USD", "SOL-USD"],
                "duration_days": 180,
                "initial_capital": 50000
            },
            {
                "name": "comprehensive",
                "description": "Full test with 8 stocks, 11 months, $100K",
                "symbols": ["AAPL", "GOOGL", "MSFT", "TSLA", "NVDA", "META", "AMZN", "NFLX"],
                "duration_days": 330,
                "initial_capital": 100000
            }
        ]
    }

@app.post("/backtests/run")
async def run_backtest(request: BacktestRequest, background_tasks: BackgroundTasks):
    """Start a new backtest"""
    backtest_id = str(uuid.uuid4())
    
    # Create backtest result tracking
    result = BacktestResult(
        id=backtest_id,
        status="starting",
        start_time=datetime.now().isoformat(),
        config=request.dict()
    )
    
    running_backtests[backtest_id] = result
    
    # Start backtest in background
    background_tasks.add_task(execute_backtest, backtest_id, request)
    
    return {
        "backtest_id": backtest_id,
        "status": "started",
        "message": "Backtest started successfully"
    }

@app.get("/backtests/{backtest_id}")
async def get_backtest_status(backtest_id: str):
    """Get status of a specific backtest"""
    if backtest_id in running_backtests:
        return running_backtests[backtest_id]
    elif backtest_id in completed_backtests:
        return completed_backtests[backtest_id]
    else:
        raise HTTPException(status_code=404, detail="Backtest not found")

@app.get("/backtests")
async def list_backtests():
    """List all backtests"""
    all_backtests = {**running_backtests, **completed_backtests}
    return {
        "backtests": list(all_backtests.values()),
        "total": len(all_backtests),
        "running": len(running_backtests),
        "completed": len(completed_backtests)
    }

@app.delete("/backtests/{backtest_id}")
async def delete_backtest(backtest_id: str):
    """Delete a backtest"""
    if backtest_id in running_backtests:
        del running_backtests[backtest_id]
        return {"message": "Running backtest cancelled"}
    elif backtest_id in completed_backtests:
        del completed_backtests[backtest_id]
        return {"message": "Backtest deleted"}
    else:
        raise HTTPException(status_code=404, detail="Backtest not found")

@app.get("/strategies")
async def list_strategies():
    """List all available strategies for testing"""
    try:
        registry = StrategyRegistry()
        strategies = registry.list_strategies()
        
        return {
            "strategies": [
                {
                    "name": name,
                    "implementation": strategy["implementation"],
                    "risk_profile": strategy["risk_profile"],
                    "symbols": strategy["symbols"],
                    "timeframes": strategy["timeframes"],
                    "description": strategy.get("description", "")
                }
                for name, strategy in strategies.items()
            ],
            "total": len(strategies)
        }
    except Exception as e:
        raise HTTPException(status_code=500, detail=f"Error loading strategies: {str(e)}")

@app.post("/strategies/test")
async def test_strategy(request: StrategyTestRequest, background_tasks: BackgroundTasks):
    """Test a specific strategy or group of strategies"""
    test_id = str(uuid.uuid4())
    
    # Start strategy test in background
    background_tasks.add_task(execute_strategy_test, test_id, request)
    
    return {
        "test_id": test_id,
        "status": "started",
        "message": "Strategy test started successfully"
    }

@app.get("/strategies/implementations")
async def get_strategy_implementations():
    """Get available strategy implementations"""
    return {
        "implementations": [
            {
                "name": "python_ai",
                "description": "Python AI-based strategies",
                "count": 3
            },
            {
                "name": "haskell_dsl", 
                "description": "Haskell DSL strategies",
                "count": 3
            },
            {
                "name": "backtest_engine",
                "description": "Backtesting engine strategies",
                "count": 2
            }
        ]
    }

@app.get("/strategies/risk-profiles")
async def get_risk_profiles():
    """Get available risk profiles"""
    return {
        "risk_profiles": [
            {
                "name": "conservative",
                "description": "Low risk, steady returns",
                "strategies": 3
            },
            {
                "name": "moderate",
                "description": "Balanced risk/reward",
                "strategies": 3
            },
            {
                "name": "aggressive",
                "description": "High risk, high reward potential",
                "strategies": 2
            }
        ]
    }

async def execute_backtest(backtest_id: str, request: BacktestRequest):
    """Execute backtest in background"""
    try:
        # Update status
        running_backtests[backtest_id].status = "running"
        
        # Prepare configuration
        if request.preset:
            config = get_preset_config(request.preset)
        else:
            config = {
                "symbols": request.symbols or ["AAPL", "GOOGL"],
                "start_date": request.start_date,
                "end_date": request.end_date,
                "initial_capital": request.initial_capital or 100000,
                "data_source": request.data_source or "yfinance",
                "timeframes": request.timeframes or ["1h", "1d"]
            }
        
        # Run simple backtest (using our existing implementation)
        simple_config = SimpleBacktestConfig(
            symbols=config["symbols"],
            timeframes=config["timeframes"],
            start_date=config.get("start_date"),
            end_date=config.get("end_date"),
            initial_capital=config["initial_capital"]
        )
        
        engine = SimpleBacktestEngine(simple_config)
        results = await asyncio.to_thread(engine.run_backtest)
        
        # Update with results
        running_backtests[backtest_id].status = "completed"
        running_backtests[backtest_id].end_time = datetime.now().isoformat()
        running_backtests[backtest_id].results = {
            "total_return": results.total_return,
            "annual_return": results.annual_return,
            "max_drawdown": results.max_drawdown,
            "win_rate": results.win_rate,
            "total_trades": results.total_trades,
            "execution_time": results.execution_time,
            "symbols_processed": len(results.data_info),
            "timeframes_used": config["timeframes"]
        }
        
        # Move to completed
        completed_backtests[backtest_id] = running_backtests.pop(backtest_id)
        
    except Exception as e:
        # Handle error
        running_backtests[backtest_id].status = "error"
        running_backtests[backtest_id].error = str(e)
        running_backtests[backtest_id].end_time = datetime.now().isoformat()
        
        # Move to completed even on error
        completed_backtests[backtest_id] = running_backtests.pop(backtest_id)

async def execute_strategy_test(test_id: str, request: StrategyTestRequest):
    """Execute strategy test in background"""
    try:
        tester = ComprehensiveStrategyTester()
        
        if request.test_type == "single" and request.strategy_name:
            result = await asyncio.to_thread(tester.test_single_strategy, request.strategy_name)
        elif request.test_type == "implementation" and request.implementation:
            result = await asyncio.to_thread(tester.test_implementation, request.implementation)
        elif request.test_type == "risk_profile" and request.risk_profile:
            result = await asyncio.to_thread(tester.test_risk_profile, request.risk_profile)
        else:
            result = await asyncio.to_thread(tester.test_all_strategies)
        
        # Store result (in a real app, you'd want proper storage)
        # For now, just return success
        
    except Exception as e:
        print(f"Strategy test error: {e}")

def get_preset_config(preset_name: str) -> Dict[str, Any]:
    """Get configuration for a preset"""
    presets = {
        "quick": {
            "symbols": ["AAPL", "GOOGL"],
            "timeframes": ["1h", "1d"],
            "initial_capital": 10000,
            "start_date": (datetime.now() - timedelta(days=30)).strftime("%Y-%m-%d"),
            "end_date": datetime.now().strftime("%Y-%m-%d")
        },
        "shorting": {
            "symbols": ["AAPL", "GOOGL", "MSFT", "TSLA", "NVDA", "META"],
            "timeframes": ["1h", "1d"],
            "initial_capital": 100000,
            "start_date": (datetime.now() - timedelta(days=180)).strftime("%Y-%m-%d"),
            "end_date": datetime.now().strftime("%Y-%m-%d")
        },
        "crypto": {
            "symbols": ["BTC-USD", "ETH-USD", "SOL-USD"],
            "timeframes": ["1h", "4h"],
            "initial_capital": 50000,
            "start_date": (datetime.now() - timedelta(days=180)).strftime("%Y-%m-%d"),
            "end_date": datetime.now().strftime("%Y-%m-%d")
        },
        "comprehensive": {
            "symbols": ["AAPL", "GOOGL", "MSFT", "TSLA", "NVDA", "META", "AMZN", "NFLX"],
            "timeframes": ["1h", "1d"],
            "initial_capital": 100000,
            "start_date": (datetime.now() - timedelta(days=330)).strftime("%Y-%m-%d"),
            "end_date": datetime.now().strftime("%Y-%m-%d")
        }
    }
    
    return presets.get(preset_name, presets["quick"])

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(app, host="0.0.0.0", port=8000) 