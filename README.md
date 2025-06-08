# LynxTrader

**Tagline: Agile. Smart. Precise.**

An AI-enhanced, high-performance algorithmic trading platform for scalping, day trading, and swing trading—optimized for price action, built with Rust, Haskell, Python, CUDA, and modern web.

## System Architecture

### Frontend (UI/UX)
- **Tech**: Next.js, Tailwind CSS, shadcn/ui, Lucide icons
- **Modules**:
  - `DashboardHome.tsx`: live PnL, trade summary
  - `StrategyBuilder.tsx`: Haskell DSL editor + YAML fallback
  - `TradeLab.tsx`: live/paper simulation toggles, historical replay
  - `BotMonitor.tsx`: status cards (active/paused/errors)
  - `AIAnalytics.tsx`: pattern detection, recommendations
- **Data Flow**: WebSocket live feeds, REST/GraphQL for config

### Strategy Engine (AI + Haskell)
- **LynxDSL**: Embedded DSL in Haskell for strategy building
- **Features**:
  - Candle logic: `when price > EMA20 and RSI < 30 then buy`
  - Strategy trees (composable functions)
  - Backtestable
  - Human-readable export
- **AI Enhancements**:
  - `GPT-trainer.hs`: fine-tunes strategy recommendations
  - AI-assisted strategy mutation with evolutionary tuning

### Execution Engine (Rust)
- **LynxBot-Core**: Handles order routing, time-sensitive logic
- **Modules**:
  - `OrderExecutor.rs`
  - `TradeSignalRouter.rs`
  - `RiskManager.rs`
  - `AlpacaClient.rs`: Commission-free trading via Alpaca Markets
- **Broker Integration**:
  - **Alpaca Markets** - Commission-free stocks, ETFs, crypto, options
  - Interactive Brokers (Canada) - Ready
  - Wealthsimple (limited) - Partial

### CUDA Backtest Engine
- `lynx_cuda_sim.cu`: Mass parallel candle simulations
- GPU-accelerated backtesting
- Integration with Python/Rust for analysis

### AI & Data Modules (Python)
- `model_trainer.py`: Train LSTMs, CNNs, Transformers with TensorBoard visualization
- `tb_monitor.py`: Real-time TensorBoard monitoring of trading performance
- `sentiment_scraper.py`: Social media sentiment analysis
- `recommender.py`: Strategy-instrument pairing recommendations

## Trading Infrastructure

### Alpaca Markets Integration
LynxTrader integrates with [Alpaca Markets](https://alpaca.markets/) for commission-free trading:

- **Paper Trading**: Test strategies risk-free with $100k virtual account
- **Live Trading**: Commission-free stocks, ETFs, options, and crypto
- **Real-time Data**: Market data and price feeds
- **Order Types**: Market, limit, stop, bracket orders with stop-loss/take-profit
- **OAuth Security**: Secure API authentication
- **24/5 Trading**: Extended hours trading support

**Features**:
- Commission-free trading for all supported assets
- Fractional share trading for position sizing
- Advanced order types (bracket, OCO, trailing stop)
- Real-time market data and historical bars
- Paper trading environment for strategy testing
- Margin and short selling capabilities

### TensorBoard Monitoring
Real-time visualization of trading performance and AI model metrics:

```bash
# Start TensorBoard monitoring
cd ai-modules
python tb_monitor.py

# View dashboard
tensorboard --logdir=tensorboard_logs
# Open http://localhost:6006
```

**Metrics Tracked**:
- Trading P&L and performance
- AI model accuracy and predictions
- Bot performance comparison
- Risk metrics and drawdown
- Feature importance analysis

## Supported Instruments

| Asset Type | Recommended Use | Alpaca Support | Reason |
|------------|----------------|----------------|---------|
| Crypto (BTC, ETH, SOL) | Scalping, 24/7 | Yes - Commission-free | Fractional, high-vol |
| TSX/US Stocks | Day/Swing | Yes - Commission-free | With fractional or sim mode |
| Forex | Day trading | No | Leverage, volume |
| ETFs (SPY, QQQ) | Swing | Yes - Commission-free | Safer, predictable |
| Options (paper only) | Swing | Yes - Commission-free | Risky but educational |

## Money Management Model

- **Capital**: $1,000
- **Risk Rules**:
  - Max risk/trade: $20–$30 (2–3%)
  - Max daily loss: 5%
  - Weekly profit target: 10%
- **Bot Governance**: RiskManager.rs enforces stop-outs and cooldowns

## Paper Trade Lab

- Replay real market sessions in "ghost" mode
- Visualization of entry/exit timing and missed trades
- AI adjustment tracking with TensorBoard/Streamlit
- Alpaca paper trading with $100k virtual account

## Hosting Plan

- **Frontend**: Vercel (Next.js + Tailwind)
- **Backend**: Rust API on Fly.io/Railway
- **CUDA**: AWS EC2 (p3/g5 instances)
- **Storage**: PostgreSQL, Redis, S3

## Roadmap

### Phase 1: MVP
- Strategy Builder (manual + YAML)
- Paper trade replay with AI analytics
- Simple dashboard with PnL + bot monitor

### Phase 2: AI & Execution
- Rust bot for live signals
- AI model integration
- Sim-mode execution

### Phase 3: Live Trade + Expansion
- Live broker mode
- CUDA backtesting
- Real account integration

## Quick Start

```bash
# Clone and setup
git clone <repository-url>
cd LynxTrader

# Frontend setup
cd frontend
npm install
npm run dev

# Backend setup
cd ../backend
cargo run

# AI modules setup
cd ../ai-modules
pip install -r requirements.txt

# Start TensorBoard monitoring
python tb_monitor.py

# Train models with visualization
python model_trainer.py

# View TensorBoard dashboard
tensorboard --logdir=tensorboard_logs
```

## Environment Variables

Create `.env` files for API keys:

```bash
# Alpaca API (get from https://alpaca.markets/)
ALPACA_API_KEY=your_api_key
ALPACA_SECRET_KEY=your_secret_key
ALPACA_PAPER_TRADING=true  # Set to false for live trading

# Database
DATABASE_URL=postgresql://localhost/lynxtrader
REDIS_URL=redis://localhost:6379
```

## Project Structure

```
LynxTrader/
├── frontend/           # Next.js UI
├── backend/           # Rust execution engine
│   └── src/
│       ├── alpaca_client.rs  # Alpaca Markets integration
│       ├── risk_manager.rs   # Position sizing & risk
│       └── order_executor.rs # Order routing
├── strategy-engine/   # Haskell DSL
├── ai-modules/        # Python ML components
│   ├── model_trainer.py      # LSTM/CNN training
│   └── tb_monitor.py         # TensorBoard monitoring
├── cuda-engine/       # CUDA backtesting
└── docs/             # Documentation
```

---

**Built with care for algorithmic traders** 