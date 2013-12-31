{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module defines periods

module Time.Period (Period(Period),
                    Unit(..)) where

-- | Admissable units for period
data Unit = Days | Weeks | Months | Years deriving (Eq, Show)

-- | Period constructed by number of units
data Period = Period { numberOfUnits :: Int, unit :: Unit } deriving Eq

instance Show Period where
     show (Period x u) = show x ++ " " ++ show u
