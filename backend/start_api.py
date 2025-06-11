#!/usr/bin/env python3
"""
LynxTrader Backtesting API Server
Starts the FastAPI server for backtesting functionality
"""

import os
import sys
import subprocess

def main():
    print("🚀 Starting LynxTrader Backtesting API...")
    
    # Change to the backend directory
    backend_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(backend_dir)
    
    try:
        # Start the FastAPI server with uvicorn
        cmd = [
            sys.executable, "-m", "uvicorn", 
            "api_server:app", 
            "--host", "0.0.0.0", 
            "--port", "8000",
            "--reload"
        ]
        
        print(f"Running: {' '.join(cmd)}")
        print("API will be available at: http://localhost:8000")
        print("API docs will be available at: http://localhost:8000/docs")
        print("\nPress Ctrl+C to stop the server\n")
        
        subprocess.run(cmd)
        
    except KeyboardInterrupt:
        print("\n🛑 Server stopped by user")
    except Exception as e:
        print(f"❌ Error starting server: {e}")
        print("\nMake sure you have the required dependencies installed:")
        print("pip install fastapi uvicorn")

if __name__ == "__main__":
    main() 