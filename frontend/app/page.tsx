'use client'

import React, { useState } from 'react'
import DashboardHome from '@/components/DashboardHome'
import ShortingStrategies from '@/components/ShortingStrategies'
import { Badge } from '@/components/ui/badge'
import { TrendingUp, TrendingDown, Activity } from 'lucide-react'
import { motion } from 'framer-motion'

export default function HomePage() {
  const [activeTab, setActiveTab] = useState<'overview' | 'shorts' | 'longs'>('overview')

  const tabs = [
    { id: 'overview', label: 'OVERVIEW', icon: Activity, color: 'neon-blue' },
    { id: 'shorts', label: 'SHORT_STRATEGIES', icon: TrendingDown, color: 'red-500' },
    { id: 'longs', label: 'LONG_STRATEGIES', icon: TrendingUp, color: 'green-500' }
  ]

  return (
    <div className="min-h-screen bg-black text-white">
      {/* High Contrast Navigation Tabs */}
      <div className="sticky top-0 z-50 bg-gray-900 border-b-4 border-white shadow-2xl">
        <div className="flex items-center justify-center p-6">
          <div className="flex gap-4">
            {tabs.map((tab) => (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id as any)}
                className={`
                  flex items-center gap-3 px-8 py-4 rounded-xl font-bold text-lg transition-all duration-200 transform hover:scale-105 border-4
                  ${activeTab === tab.id 
                    ? tab.id === 'overview' 
                      ? 'bg-blue-600 text-white border-blue-300 shadow-lg' 
                      : tab.id === 'shorts'
                      ? 'bg-red-600 text-white border-red-300 shadow-lg'
                      : 'bg-green-600 text-white border-green-300 shadow-lg'
                    : 'bg-gray-700 text-white border-gray-400 hover:bg-gray-600'
                  }
                `}
              >
                <tab.icon className="h-6 w-6" />
                {tab.label.replace('_', ' ')}
                {activeTab === tab.id && (
                  <motion.div
                    layoutId="activeTab"
                    className="absolute inset-0 rounded-xl border-4 border-yellow-400"
                    initial={false}
                    transition={{ type: "spring", bounce: 0.2, duration: 0.6 }}
                  />
                )}
              </button>
            ))}
          </div>
        </div>
        
        {/* Current Tab Indicator */}
        <div className="text-center pb-4">
          <div className="text-2xl font-bold text-yellow-400">
            Currently Viewing: {tabs.find(t => t.id === activeTab)?.label.replace('_', ' ')}
          </div>
        </div>
      </div>

      {/* Content */}
      <motion.div
        key={activeTab}
        initial={{ opacity: 0, x: 20 }}
        animate={{ opacity: 1, x: 0 }}
        exit={{ opacity: 0, x: -20 }}
        transition={{ duration: 0.3 }}
      >
        {activeTab === 'overview' && <DashboardHome />}
        {activeTab === 'shorts' && <ShortingStrategies />}
        {activeTab === 'longs' && (
          <div className="p-6">
            <div className="text-center space-y-6">
              <h2 className="text-4xl font-bold text-green-400">🚀 LONG STRATEGIES</h2>
              <p className="text-xl text-white">Long strategies are available in the OVERVIEW dashboard!</p>
              <div className="space-y-4">
                <Badge className="bg-green-600 text-white border-4 border-green-300 px-6 py-3 text-lg font-bold">
                  ACTIVE IN OVERVIEW TAB
                </Badge>
                <div className="bg-gray-900 border-4 border-white rounded-xl p-6">
                  <h3 className="text-2xl font-bold text-white mb-4">Available Long Strategies:</h3>
                  <div className="grid grid-cols-1 md:grid-cols-2 gap-4 text-left">
                    <div className="bg-green-600 p-4 rounded-lg border-2 border-green-300">
                      <h4 className="font-bold text-white">SCALP: VWAP Bounce</h4>
                      <p className="text-green-100">1-5min • VWAP + Volume Spike</p>
                    </div>
                    <div className="bg-green-600 p-4 rounded-lg border-2 border-green-300">
                      <h4 className="font-bold text-white">DAY: Opening Range Breakout</h4>
                      <p className="text-green-100">5-30min • ORB + Volume Confirmation</p>
                    </div>
                    <div className="bg-green-600 p-4 rounded-lg border-2 border-green-300">
                      <h4 className="font-bold text-white">SWING: Liquidity Grab Reversal</h4>
                      <p className="text-green-100">1-4hr • Smart Money Concepts</p>
                    </div>
                    <div className="bg-green-600 p-4 rounded-lg border-2 border-green-300">
                      <h4 className="font-bold text-white">SWING: Fair Value Gap Fill</h4>
                      <p className="text-green-100">30min-2hr • Imbalance Correction</p>
                    </div>
                  </div>
                </div>
                <button
                  onClick={() => setActiveTab('overview')}
                  className="bg-blue-600 text-white border-4 border-blue-300 px-8 py-4 rounded-xl text-xl font-bold hover:bg-blue-700 transform hover:scale-105 transition-all duration-200"
                >
                  → Go to OVERVIEW to see Long Strategies
                </button>
              </div>
            </div>
          </div>
        )}
      </motion.div>
    </div>
  )
} 