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
- **Broker Integration**:
  - Interactive Brokers (Canada) - Ready
  - Alpaca (Sim mode) - Ready
  - Wealthsimple (limited) - Partial

### CUDA Backtest Engine
- `lynx_cuda_sim.cu`: Mass parallel candle simulations
- GPU-accelerated backtesting
- Integration with Python/Rust for analysis

### AI & Data Modules (Python)
- `model_trainer.py`: Train LSTMs, CNNs, Transformers
- `sentiment_scraper.py`: Social media sentiment analysis
- `recommender.py`: Strategy-instrument pairing recommendations

## Supported Instruments

| Asset Type | Recommended Use | Reason |
|------------|----------------|---------|
| Crypto (BTC, ETH, SOL) | Scalping, 24/7 | Fractional, high-vol |
| TSX/US Stocks | Day/Swing | With fractional or sim mode |
| Forex | Day trading | Leverage, volume |
| ETFs (SPY, QQQ) | Swing | Safer, predictable |
| Options (paper only) | Swing | Risky but educational |

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
python model_trainer.py
```

## Project Structure

```
LynxTrader/
├── frontend/           # Next.js UI
├── backend/           # Rust execution engine
├── strategy-engine/   # Haskell DSL
├── ai-modules/        # Python ML components
├── cuda-engine/       # CUDA backtesting
└── docs/             # Documentation
```

---

**Built with care for algorithmic traders** 