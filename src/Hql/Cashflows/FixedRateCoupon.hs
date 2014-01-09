{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module defines fixed rate coupons

module Hql.Cashflows.FixedRateCoupon (FixedRateCoupon(..)
                                     ) where

import Hql.Base.InterestRate
import Hql.Cashflows.Cashflow
import Hql.Cashflows.Coupon

-- | Fixed rate coupon
data FixedRateCoupon =
  -- | Standard Constructor
  FixedRateCoupon {
  -- | Coupon basis information
  cpBasis :: CouponBasis,
  -- | fixed rate of coupon
  fixedRate :: Double }

instance Cashflow FixedRateCoupon where
  amount c = (cpNominal (cpBasis c)) * ( (compoundFactor (fixedRate c) (cpDayCounter (cpBasis c))
                                          Simple Nothing (cpAccrualStart (cpBasis c)) (cpAccrualEnd (cpBasis c))
                                          (cpRefStart (cpBasis c)) (cpRefEnd (cpBasis c))) - 1.0 )
  payDate c = cpPayDate (cpBasis c)

instance Coupon FixedRateCoupon where
  accrualStartDate c = cpAccrualStart (cpBasis c)
  accrualEndDate c = cpAccrualEnd (cpBasis c)
  refPeriodStartDate c = cpRefStart (cpBasis c)
  refPeriodEndDate c = cpRefEnd (cpBasis c)
  nominal c = cpNominal (cpBasis c)
  rate c = fixedRate c
  dayCounter c = cpDayCounter (cpBasis c)
  accruedAmount c d
    | d <= accrualStartDate c || d > (cpPayDate (cpBasis c)) = 0.0
    | otherwise = (cpNominal (cpBasis c)) * ( (compoundFactor (fixedRate c) (cpDayCounter (cpBasis c))
                                          Simple Nothing (cpAccrualStart (cpBasis c)) (min (cpAccrualEnd (cpBasis c)) d)
                                          (cpRefStart (cpBasis c)) (cpRefEnd (cpBasis c))) - 1.0 )
