{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module defines a cashflow

module Hql.Cashflows.Cashflow (Cashflow(..),
                               SimpleCashflow(..)
                              ) where

import Hql.Time.Date

-- | Cashflow type class
class Cashflow a where
  -- | amount that is received (usually positive) or paid (usually negative)
  amount :: a -> Double
  -- | date on which the amount is paid
  payDate :: a -> Date
  -- | True if first cashflow occurs on a date earlier than the second cashflow
  earlierThan :: a -> a -> Bool
  earlierThan a1 a2 = payDate a1 < payDate a2

-- | Simple cash flow given by amount and pay date
data SimpleCashflow = SimpleCashflow { -- | amount
                                       scAmount :: Double,
                                       -- | pay date
                                       scPayDate :: Date
                                     } deriving (Show, Eq)

instance Cashflow SimpleCashflow where
  amount c = scAmount c
  payDate c = scPayDate c
