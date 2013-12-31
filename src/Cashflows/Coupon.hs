{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module defines coupons

module Cashflows.Coupon (Coupon(..)) where

import Time.Date
import Time.DayCounter

-- | Coupon type class
class Coupon a where
  -- abstract functions
  -- | Start date for accrual computation
  accrualStartDate :: a -> Date
  -- | End date for accrual computation
  accrualEndDate :: a-> Date
  -- | Reference period start date for accrual computation
  refPeriodStartDate :: a -> Maybe Date
  -- | Reference period end date for accrual computation
  refPeriodEndDate :: a -> Maybe Date
  -- | Underlying notional
  nominal :: a -> Double
  -- | Rate of coupon
  rate :: a -> Double
  -- | Day counter for accrual computation
  dayCounter :: a -> DayCounter
  -- | Accruals up to a given date
  accruedAmount :: a -> Date -> Double
  -- derived functions
  -- | Days in accrual period
  accrualDays :: a -> Int
  -- | Year fraction equivalent for accrual period
  accrualPeriod :: a -> Double
  -- | Days up to given date in coupon period
  accruedDays :: a -> Date -> Int
  -- | Year fraction equivalent for period up to given date in coupon period
  accruedPeriod :: a -> Date -> Double
  -- implementation of derived functions
  accrualDays a = dayCount (dayCounter a) (accrualStartDate a) (accrualEndDate a)
  accrualPeriod a = yearFraction (dayCounter a) (accrualStartDate a) (accrualEndDate a)
                    (refPeriodStartDate a) (refPeriodEndDate a)
  accruedDays a d = dayCount (dayCounter a) (accrualStartDate a) (min d (accrualEndDate a))
  accruedPeriod a d = yearFraction (dayCounter a) (accrualStartDate a) (min d (accrualEndDate a))
                      (refPeriodStartDate a) (refPeriodEndDate a)
