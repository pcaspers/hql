{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module defines frequencies

module Time.Frequency (Frequency(..),
                       frequencyPerYear) where

-- | Allowed frequencies
data Frequency = NoFrequency |      -- ^null frequency
                 Once |             -- ^only once, e.g., a zero-coupon
                 Annual |           -- ^once a year
                 Semiannual |       -- ^twice a year
                 EveryFourMonth |   -- ^every fourth month
                 Quarterly |        -- ^every third month
                 Bimonthly |        -- ^every second month
                 Monthly |          -- ^once a month
                 EveryFourthWeek |  -- ^every fourth week
                 Biweekly |         -- ^every second week
                 Weekly |           -- ^once a week
                 Daily |            -- ^once a day
                 OtherFrequency     -- ^some other unknown frequency
               deriving (Eq, Show)

-- | Number of events per year for a given frequency
frequencyPerYear :: Frequency -> Int
frequencyPerYear f = case f of
                     NoFrequency -> -1
                     Once -> 0
                     Annual -> 1
                     Semiannual -> 2
                     EveryFourMonth -> 3
                     Quarterly -> 4
                     Bimonthly -> 6
                     Monthly -> 12
                     EveryFourthWeek -> 13
                     Biweekly -> 26
                     Weekly -> 52
                     Daily -> 365
                     OtherFrequency -> 999
