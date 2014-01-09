{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module defines floating rate coupons

module Hql.Cashflows.FloatingRateCoupon (FloatingRateCoupon(..)
                                        ) where

import Hql.Cashflows.Cashflow
import Hql.Cashflows.Coupon

data FloatingRateCoupon =
  FloatingRateCoupon {
    cpBasis :: CouponBasis,
    index :: Double -- Dummy implementation
    }

data FloatingRateCouponInstance =
  FloatingRateCouponInstance {
    coupon :: FloatingRateCoupon,
    swapletRate :: FloatginRateCoupon -> Double }

instance Cashflow FloatingRateCouponInstance where
  amount c = (pricer (coupon c)) * (nominal (cpBasis (coupon c))) * (accrualPeriod (coupon c))
  payDate c = cpPayDate (cpBasis (coupon c))

instance Coupon FloatingRateCouponInstance where
  accrualStartDate c = cpAccrualStart (cpBasis (coupon c))
  accrualEndDate c = cpAccrualEnd (cpBasis (coupon c))
  refPeriodStartDate c = cpRefStart (cpBasis (coupon c))
  refPeriodEndDate c = cpRefEnd (cpBasis (coupon c))
  nominal c = cpNominal (cpBasis (coupon c))
  rate c = (swapletRate c) (coupon c)
  dayCounter c = cpDayCounter (cpBasis (coupon c))
  accruedAmount c d
    | d <= accrualStartDate c || d > (cpPayDate (cpBasis (coupon c))) = 0.0
    | otherwise = (cpNominal (cpBasis (coupon c))) * (rate c) *
                  (yearFraction (accrualStartDate c) (min (cpAccrualEnd (cpBasis (coupon c))) d)
                  (cpRefStart (cpBasis (coupon c))) (cpRefEnd (cpBasis (coupon c))))
