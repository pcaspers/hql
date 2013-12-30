module Cashflows.Cashflow (Cashflow(..),
                           SimpleCashflow(SimpleCashflow)
                          ) where

import Time.Date

class Cashflow a where
  amount :: a -> Double
  payDate :: a -> Date
  earlierThan :: a -> a -> Bool
  earlierThan a1 a2 = payDate a1 < payDate a2

data SimpleCashflow = SimpleCashflow { scAmount :: Double, scPayDate :: Date } deriving (Show, Eq)

instance Cashflow SimpleCashflow where
  amount c = scAmount c
  payDate c = scPayDate c

