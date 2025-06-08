#!/bin/bash

# LynxTrader Deployment Script
# Agile. Smart. Precise.

set -e

echo "🚀 Starting LynxTrader deployment..."

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if required tools are installed
check_dependencies() {
    print_status "Checking dependencies..."
    
    # Check Node.js
    if ! command -v node &> /dev/null; then
        print_error "Node.js is not installed"
        exit 1
    fi
    
    # Check Rust
    if ! command -v cargo &> /dev/null; then
        print_error "Rust is not installed"
        exit 1
    fi
    
    # Check Haskell Stack
    if ! command -v stack &> /dev/null; then
        print_error "Haskell Stack is not installed"
        exit 1
    fi
    
    # Check Python
    if ! command -v python3 &> /dev/null; then
        print_error "Python 3 is not installed"
        exit 1
    fi
    
    print_success "All dependencies are installed"
}

# Build frontend
build_frontend() {
    print_status "Building React frontend..."
    cd frontend
    
    # Install dependencies
    npm install
    
    # Build for production
    npm run build
    
    print_success "Frontend built successfully"
    cd ..
}

# Build Rust backend
build_rust_backend() {
    print_status "Building Rust backend..."
    cd rust-backend
    
    # Build in release mode
    cargo build --release
    
    # Run tests
    cargo test
    
    print_success "Rust backend built successfully"
    cd ..
}

# Build Haskell strategy engine
build_haskell_engine() {
    print_status "Building Haskell strategy engine..."
    cd haskell-strategy-engine
    
    # Install dependencies and build
    stack setup
    stack build
    
    # Run tests
    stack test
    
    print_success "Haskell strategy engine built successfully"
    cd ..
}

# Setup Python AI modules
setup_python_ai() {
    print_status "Setting up Python AI modules..."
    
    if [ ! -d "python-ai" ]; then
        mkdir -p python-ai
        cd python-ai
        
        # Create virtual environment
        python3 -m venv venv
        source venv/bin/activate
        
        # Create requirements.txt
        cat > requirements.txt << EOF
numpy>=1.21.0
pandas>=1.3.0
scikit-learn>=1.0.0
tensorflow>=2.8.0
transformers>=4.20.0
torch>=1.12.0
requests>=2.28.0
websockets>=10.3
asyncio-mqtt>=0.11.0
python-dotenv>=0.19.0
fastapi>=0.85.0
uvicorn>=0.18.0
pydantic>=1.9.0
EOF
        
        # Install dependencies
        pip install -r requirements.txt
        
        # Create main.py
        cat > main.py << EOF
"""
LynxTrader Python AI Module
Handles machine learning models and NLP processing
"""

import asyncio
import logging
from fastapi import FastAPI
from pydantic import BaseModel
import uvicorn

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

app = FastAPI(title="LynxTrader AI Module", version="0.1.0")

class SentimentRequest(BaseModel):
    text: str

class SentimentResponse(BaseModel):
    sentiment: str
    confidence: float

@app.get("/health")
async def health_check():
    return {"status": "healthy", "service": "lynx-trader-ai"}

@app.post("/sentiment", response_model=SentimentResponse)
async def analyze_sentiment(request: SentimentRequest):
    # Basic sentiment analysis (replace with actual ML model)
    text = request.text.lower()
    positive_words = ["bullish", "buy", "up", "gain", "profit", "surge"]
    negative_words = ["bearish", "sell", "down", "loss", "drop", "crash"]
    
    positive_score = sum(1 for word in positive_words if word in text)
    negative_score = sum(1 for word in negative_words if word in text)
    
    if positive_score > negative_score:
        sentiment = "positive"
        confidence = min(0.9, 0.5 + (positive_score - negative_score) * 0.1)
    elif negative_score > positive_score:
        sentiment = "negative"
        confidence = min(0.9, 0.5 + (negative_score - positive_score) * 0.1)
    else:
        sentiment = "neutral"
        confidence = 0.5
    
    return SentimentResponse(sentiment=sentiment, confidence=confidence)

if __name__ == "__main__":
    uvicorn.run(app, host="0.0.0.0", port=8001)
EOF
        
        deactivate
        cd ..
    fi
    
    print_success "Python AI modules set up successfully"
}

# Create environment configuration
create_env_config() {
    print_status "Creating environment configuration..."
    
    if [ ! -f ".env" ]; then
        cat > .env << EOF
# LynxTrader Environment Configuration

# Database
DATABASE_URL=postgresql://lynx:password@localhost:5432/lynxtrader
REDIS_URL=redis://localhost:6379

# API Configuration
RUST_BACKEND_PORT=8080
FRONTEND_PORT=3000
PYTHON_AI_PORT=8001
HASKELL_ENGINE_PORT=8002

# Trading Configuration
MAX_DAILY_LOSS=5000
MAX_DRAWDOWN=15000
DEFAULT_POSITION_SIZE=1000
RISK_LEVEL=balanced

# External APIs (replace with your actual keys)
ALPHA_VANTAGE_API_KEY=your_alpha_vantage_key
POLYGON_API_KEY=your_polygon_key
NEWS_API_KEY=your_news_api_key

# Security
JWT_SECRET=your_jwt_secret_key_here
ENCRYPTION_KEY=your_encryption_key_here

# Logging
LOG_LEVEL=info
RUST_LOG=info
EOF
        print_warning "Created .env file with default values. Please update with your actual configuration."
    fi
    
    print_success "Environment configuration ready"
}

# Create Docker configuration
create_docker_config() {
    print_status "Creating Docker configuration..."
    
    # Create Dockerfile for the complete application
    cat > Dockerfile << EOF
# Multi-stage build for LynxTrader
FROM node:18-alpine AS frontend-builder
WORKDIR /app/frontend
COPY frontend/package*.json ./
RUN npm ci --only=production
COPY frontend/ .
RUN npm run build

FROM rust:1.70 AS rust-builder
WORKDIR /app/rust-backend
COPY rust-backend/Cargo.toml rust-backend/Cargo.lock ./
RUN cargo fetch
COPY rust-backend/src ./src
RUN cargo build --release

FROM haskell:9.0 AS haskell-builder
WORKDIR /app/haskell-strategy-engine
COPY haskell-strategy-engine/stack.yaml haskell-strategy-engine/package.yaml ./
RUN stack setup
COPY haskell-strategy-engine/ .
RUN stack build --copy-bins

FROM python:3.9-slim AS python-ai
WORKDIR /app/python-ai
COPY python-ai/requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt
COPY python-ai/ .

# Final production image
FROM ubuntu:22.04
RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy built applications
COPY --from=frontend-builder /app/frontend/.next ./frontend/.next
COPY --from=frontend-builder /app/frontend/public ./frontend/public
COPY --from=frontend-builder /app/frontend/package.json ./frontend/
COPY --from=rust-builder /app/rust-backend/target/release/lynx-trader-backend ./
COPY --from=haskell-builder /root/.local/bin/lynx-trader-exe ./
COPY --from=python-ai /app/python-ai ./python-ai

# Install Node.js for frontend
RUN curl -fsSL https://deb.nodesource.com/setup_18.x | bash - \
    && apt-get install -y nodejs

EXPOSE 3000 8080 8001 8002

# Create startup script
RUN echo '#!/bin/bash\n\
cd /app/frontend && npm start &\n\
cd /app && ./lynx-trader-backend &\n\
cd /app/python-ai && python main.py &\n\
cd /app && ./lynx-trader-exe &\n\
wait' > /app/start.sh && chmod +x /app/start.sh

CMD ["/app/start.sh"]
EOF

    # Create docker-compose.yml
    cat > docker-compose.yml << EOF
version: '3.8'

services:
  postgres:
    image: postgres:15
    environment:
      POSTGRES_DB: lynxtrader
      POSTGRES_USER: lynx
      POSTGRES_PASSWORD: password
    ports:
      - "5432:5432"
    volumes:
      - postgres_data:/var/lib/postgresql/data

  redis:
    image: redis:7-alpine
    ports:
      - "6379:6379"
    volumes:
      - redis_data:/data

  lynxtrader:
    build: .
    ports:
      - "3000:3000"  # Frontend
      - "8080:8080"  # Rust Backend
      - "8001:8001"  # Python AI
      - "8002:8002"  # Haskell Engine
    depends_on:
      - postgres
      - redis
    environment:
      - DATABASE_URL=postgresql://lynx:password@postgres:5432/lynxtrader
      - REDIS_URL=redis://redis:6379
    volumes:
      - ./.env:/app/.env

volumes:
  postgres_data:
  redis_data:
EOF

    print_success "Docker configuration created"
}

# Main deployment function
main() {
    print_status "🎯 LynxTrader - Agile. Smart. Precise."
    print_status "Starting automated deployment process..."
    
    # Check dependencies
    check_dependencies
    
    # Create configurations
    create_env_config
    create_docker_config
    
    # Setup Python AI (create if doesn't exist)
    setup_python_ai
    
    # Build all components
    build_frontend
    build_rust_backend
    build_haskell_engine
    
    print_success "🎉 LynxTrader deployment completed successfully!"
    print_status "Next steps:"
    echo "  1. Update .env file with your configuration"
    echo "  2. Start services: docker-compose up -d"
    echo "  3. Access frontend: http://localhost:3000"
    echo "  4. Access API: http://localhost:8080"
    print_status "Happy trading! 📈"
}

# Run main function
main 