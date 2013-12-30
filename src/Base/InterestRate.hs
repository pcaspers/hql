module Base.InterestRate (Compounding(..),
                          discountFactor,
                          compoundFactor) where

import Time.DayCounter
import Time.Frequency
import Time.Date

data Compounding = Simple | Compounded | Continuous | SimpleThenCompounded

discountFactor :: Double -> DayCounter -> Compounding -> Maybe Frequency -> Date -> Date -> Maybe Date -> Maybe Date -> Double

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

compoundFactor rate dayCounter compounding frequency date1 date2 refDate1 refDate2 = 
  1.0 / (discountFactor rate dayCounter compounding frequency date1 date2 refDate1 refDate2) 