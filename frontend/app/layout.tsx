import type { Metadata } from 'next'
import { Inter } from 'next/font/google'
import React from 'react'
import './globals.css'

const inter = Inter({ subsets: ['latin'] })

export const metadata: Metadata = {
  title: 'LynxTrader - Agile. Smart. Precise.',
  description: 'AI-enhanced algorithmic trading platform for scalping, day trading, and swing trading',
}

export default function RootLayout({
  children,
}: {
  children: React.ReactNode
}) {
  return (
    <html lang="en" className="dark">
      <body className={`${inter.className} bg-cyber-gradient`}>
        <div className="min-h-screen bg-cyber-gradient relative">
          {/* Matrix rain background layer */}
          <div className="fixed inset-0 bg-cyber-gradient pointer-events-none -z-10" />
          {children}
        </div>
      </body>
    </html>
  )
} 