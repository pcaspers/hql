{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module defines interest rates, discounting and compounding

module Base.InterestRate (Compounding(..),
                          discountFactor,
                          compoundFactor) where

import Time.DayCounter
import Time.Frequency
import Time.Date

-- | Coumpounding modes
data Compounding = Simple |                 -- ^ 1+rt
                   Compounded |             -- ^ (1+r/f)**(f**t) with f being the frequency per year
                   Continuous |             -- ^ exp(rt)
                   SimpleThenCompounded     -- ^ Simple within a year, Coumpounded over one year

-- | Compute discount factor
discountFactor :: Double               -- ^ rate
                  -> DayCounter        -- ^ day counter
                  -> Compounding       -- ^ compounding mode
                  -> Maybe Frequency   -- ^ for Compounded mode a frequency must  be given
                  -> Date              -- ^ start date
                  -> Date              -- ^ end date
                  -> Maybe Date        -- ^ reference period start date
                  -> Maybe Date        -- ^ reference period end date
                  -> Double            -- ^ discount factor

discountFactor rate dayCounter compounding frequency date1 date2 refDate1 refDate2 = case compounding of
  Simple -> 1.0 / ( 1.0 + rate * t )
  Compounded -> ( 1.0 + rate / f ) ** ( -t * f )
  Continuous -> exp( -rate * t )
  SimpleThenCompounded -> if t <= 1.0 / f
                          then discountFactor rate dayCounter Simple frequency date1 date2 refDate1 refDate2
                          else discountFactor rate dayCounter Compounded frequency date1 date2 refDate1 refDate2
  where t = yearFraction dayCounter date1 date2 refDate1 refDate2
        f = case frequency of
          Nothing -> error ("no frequency given")
          Just freq -> fromIntegral (frequencyPerYear freq)

-- | Compute compound factor, usage is the same as with discount factor
compoundFactor rate dayCounter compounding frequency date1 date2 refDate1 refDate2 = 
  1.0 / (discountFactor rate dayCounter compounding frequency date1 date2 refDate1 refDate2) 