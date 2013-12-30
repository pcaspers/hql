module Cashflows.Coupon (Coupon) where

import Time.Date
import Time.DayCounter

class Coupon a where
  -- abstract functions
  accrualStartDate :: a -> Date
  accrualEndDate :: a-> Date
  refPeriodStartDate :: a -> Maybe Date
  refPeriodEndDate :: a -> Maybe Date
  nominal :: a -> Double
  rate :: a -> Double
  dayCounter :: a -> DayCounter
  accruedAmount :: a -> Date -> Double
  -- derived functions
  accrualDays :: a -> Int
  accrualPeriod :: a -> Double
  accruedDays :: a -> Date -> Int
  accruedPeriod :: a -> Date -> Double
  -- implementation of derived functions
  accrualDays a = dayCount (dayCounter a) (accrualStartDate a) (accrualEndDate a) Nothing Nothing
  accrualPeriod a = yearFraction (dayCounter a) (accrualStartDate a) (accrualEndDate a)
                    (refPeriodStartDate a) (refPeriodEndDate a)
  accruedDays a d = dayCount (dayCounter a) (accrualStartDate a) (min d (accrualEndDate a)) Nothing Nothing
  accruedPeriod a d = yearFraction (dayCounter a) (accrualStartDate a) (min d (accrualEndDate a))
                      (refPeriodStartDate a) (refPeriodEndDate a)

