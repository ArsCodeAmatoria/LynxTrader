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
    <div className="min-h-screen bg-cyber-gradient text-white">
      {/* Navigation Tabs */}
      <div className="sticky top-0 z-50 bg-cyber-100/90 backdrop-blur-sm border-b border-neon-blue/30">
        <div className="flex items-center justify-center p-4">
          <div className="flex gap-2">
            {tabs.map((tab) => (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id as any)}
                className={`
                  flex items-center gap-2 px-6 py-3 rounded-lg font-mono text-sm transition-all duration-300
                  ${activeTab === tab.id 
                    ? `bg-${tab.color}/20 text-${tab.color} border border-${tab.color}/30 shadow-lg` 
                    : 'bg-cyber-200/20 text-cyber-300 hover:bg-cyber-200/30 hover:text-white'
                  }
                `}
              >
                <tab.icon className="h-4 w-4" />
                {tab.label}
                {activeTab === tab.id && (
                  <motion.div
                    layoutId="activeTab"
                    className={`absolute inset-0 bg-${tab.color}/10 rounded-lg`}
                    initial={false}
                    transition={{ type: "spring", bounce: 0.2, duration: 0.6 }}
                  />
                )}
              </button>
            ))}
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
            <div className="text-center space-y-4">
              <h2 className="text-2xl font-bold text-green-400">🚀 LONG STRATEGIES</h2>
              <p className="text-gray-400">Long strategies module coming soon...</p>
              <Badge className="bg-green-500/20 text-green-400 border-green-400">
                IN DEVELOPMENT
              </Badge>
            </div>
          </div>
        )}
      </motion.div>
    </div>
  )
} 