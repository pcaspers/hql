{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module contains all Cashflows submodules

module Hql.Cashflows(
  module Hql.Cashflows.Cashflow,
  module Hql.Cashflows.Coupon,
  module Hql.Cashflows.FixedRateCoupon,
  ) where

import Hql.Cashflows.Cashflow
import Hql.Cashflows.Coupon
import Hql.Cashflows.FixedRateCoupon
