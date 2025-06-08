/** @type {import('tailwindcss').Config} */
module.exports = {
  darkMode: ["class"],
  content: [
    './pages/**/*.{js,ts,jsx,tsx,mdx}',
    './components/**/*.{js,ts,jsx,tsx,mdx}',
    './app/**/*.{js,ts,jsx,tsx,mdx}',
  ],
  theme: {
    container: {
      center: true,
      padding: "2rem",
      screens: {
        "2xl": "1400px",
      },
    },
    extend: {
      colors: {
        border: "hsl(var(--border))",
        input: "hsl(var(--input))",
        ring: "hsl(var(--ring))",
        background: "hsl(var(--background))",
        foreground: "hsl(var(--foreground))",
        primary: {
          DEFAULT: "hsl(var(--primary))",
          foreground: "hsl(var(--primary-foreground))",
        },
        secondary: {
          DEFAULT: "hsl(var(--secondary))",
          foreground: "hsl(var(--secondary-foreground))",
        },
        destructive: {
          DEFAULT: "hsl(var(--destructive))",
          foreground: "hsl(var(--destructive-foreground))",
        },
        muted: {
          DEFAULT: "hsl(var(--muted))",
          foreground: "hsl(var(--muted-foreground))",
        },
        accent: {
          DEFAULT: "hsl(var(--accent))",
          foreground: "hsl(var(--accent-foreground))",
        },
        popover: {
          DEFAULT: "hsl(var(--popover))",
          foreground: "hsl(var(--popover-foreground))",
        },
        card: {
          DEFAULT: "hsl(var(--card))",
          foreground: "hsl(var(--card-foreground))",
        },
        // Cyber color palette
        cyber: {
          50: '#0a0a0f',
          100: '#0f0f1a',
          200: '#1a1a2e',
          300: '#16213e',
          400: '#0f3460',
          500: '#e94560',
          600: '#f39c12',
          700: '#00d4ff',
          800: '#39ff14',
          900: '#ff007f',
        },
        // Neon colors
        neon: {
          blue: '#00ffff',
          green: '#39ff14',
          pink: '#ff007f',
          orange: '#ff6600',
          purple: '#bf00ff',
          yellow: '#ffff00',
        },
        // Matrix green
        matrix: {
          50: '#00ff41',
          100: '#00e838',
          200: '#00d12f',
          300: '#00ba26',
          400: '#00a31d',
          500: '#008c14',
          600: '#00750b',
          700: '#005e02',
          800: '#004700',
          900: '#003000',
        },
        // Crypto purple
        crypto: {
          50: '#f3e8ff',
          100: '#e9d5ff',
          200: '#d8b4fe',
          300: '#c084fc',
          400: '#a855f7',
          500: '#9333ea',
          600: '#7c3aed',
          700: '#6d28d9',
          800: '#5b21b6',
          900: '#4c1d95',
        }
      },
      borderRadius: {
        lg: "var(--radius)",
        md: "calc(var(--radius) - 2px)",
        sm: "calc(var(--radius) - 4px)",
      },
      keyframes: {
        "accordion-down": {
          from: { height: 0 },
          to: { height: "var(--radix-accordion-content-height)" },
        },
        "accordion-up": {
          from: { height: "var(--radix-accordion-content-height)" },
          to: { height: 0 },
        },
        glow: {
          '0%': { 
            boxShadow: '0 0 5px currentColor, 0 0 20px currentColor, 0 0 35px currentColor',
            textShadow: '0 0 5px currentColor'
          },
          '100%': { 
            boxShadow: '0 0 2px currentColor, 0 0 10px currentColor, 0 0 15px currentColor',
            textShadow: '0 0 2px currentColor'
          }
        },
        'matrix-rain': {
          '0%': { transform: 'translateY(-100vh)' },
          '100%': { transform: 'translateY(100vh)' }
        },
        'cyber-pulse': {
          '0%, 100%': { opacity: 1 },
          '50%': { opacity: 0.5 }
        },
        'float': {
          '0%, 100%': { transform: 'translateY(0px)' },
          '50%': { transform: 'translateY(-20px)' }
        },
        'scan-line': {
          '0%': { transform: 'translateX(-100%)' },
          '100%': { transform: 'translateX(100vw)' }
        },
        'data-stream': {
          '0%': { transform: 'translateX(-100%)' },
          '100%': { transform: 'translateX(100%)' }
        }
      },
      animation: {
        "accordion-down": "accordion-down 0.2s ease-out",
        "accordion-up": "accordion-up 0.2s ease-out",
        'glow': 'glow 2s ease-in-out infinite alternate',
        'matrix-rain': 'matrix-rain 3s linear infinite',
        'cyber-pulse': 'cyber-pulse 1.5s ease-in-out infinite',
        'float': 'float 3s ease-in-out infinite',
        'scan-line': 'scan-line 2s linear infinite',
        'data-stream': 'data-stream 8s linear infinite',
      },
      backgroundImage: {
        'cyber-gradient': 'linear-gradient(135deg, #0f0f1a 0%, #1a1a2e 50%, #16213e 100%)',
        'neon-gradient': 'linear-gradient(45deg, #00ffff 0%, #ff007f 50%, #39ff14 100%)',
        'matrix-gradient': 'linear-gradient(180deg, #000000 0%, #001100 100%)',
        'crypto-gradient': 'linear-gradient(135deg, #1a1a2e 0%, #5b21b6 100%)',
      },
      boxShadow: {
        'neon': '0 0 5px currentColor, 0 0 20px currentColor, 0 0 35px currentColor',
        'neon-sm': '0 0 2px currentColor, 0 0 10px currentColor',
        'cyber': '0 0 20px rgba(0, 255, 255, 0.5)',
        'matrix': '0 0 20px rgba(57, 255, 20, 0.5)',
        'crypto': '0 0 20px rgba(147, 51, 234, 0.5)',
      },
      fontFamily: {
        'mono': ['Fira Code', 'Consolas', 'Monaco', 'monospace'],
        'cyber': ['Orbitron', 'system-ui', 'sans-serif'],
        'matrix': ['Share Tech Mono', 'monospace'],
      },
      screens: {
        'xs': '475px',
      },
      backdropBlur: {
        'xs': '2px',
      }
    },
  },
  plugins: [
    require('@tailwindcss/forms'),
    require('@tailwindcss/typography'),
  ],
} 