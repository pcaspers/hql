{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module defines day counters and corresponding day count
--   and time measuring functions.

module Time.DayCounter (DayCounter(..),
                        dayCount,
                        yearFraction) where

import Time.Date

-- | Day counter definitions
data DayCounter =
    Actual360 |     -- ^ Actual\/360 day count convention, also known as \"Act\/360\", \"A\/360\"
    Thirty360 |     {- ^ 30\/360 day count convention
        The 30\/360 day count can be calculated according to US, European, or
        Italian conventions.

        US (NASD) convention: if the starting date is the 31st of a
        month, it becomes equal to the 30th of the same month.
        If the ending date is the 31st of a month and the starting
        date is earlier than the 30th of a month, the ending date
        becomes equal to the 1st of the next month, otherwise the
        ending date becomes equal to the 30th of the same month.
        Also known as \"30\/360\", \"360\/360\", or \"Bond Basis\"

        European convention: starting dates or ending dates that
        occur on the 31st of a month become equal to the 30th of the
        same month.
        Also known as \"30E\/360\", or \"Eurobond Basis\"

        Italian convention: starting dates or ending dates that
        occur on February and are grater than 27 become equal to 30
        for computational sake. -}
    Thirty360USA |
    Thirty360BondBasis |
    Thirty360European |
    Thirty360EurobondBasis |
    Thirty360Italian |
    Actual365Fixed     {- ^Actual\/365 (Fixed) day count convention
       \"Actual\/365 (Fixed)\" day count convention, also know as
        \"Act\/365 (Fixed)\", \"A\/365 (Fixed)\", or \"A\/365F\".

       According to ISDA, \"Actual\/365\" (without \"Fixed\") is
       an alias for \"Actual\/Actual (ISDA)\" (see
       ActualActual.)  If Actual\/365 is not explicitly
       specified as fixed in an instrument specification,
       you might want to double-check its meaning. -}

    deriving (Eq)

instance Show DayCounter where
  show dayCounter = case dayCounter of
    Actual360 -> "Actual/360"
    Thirty360 -> "30/360"
    Thirty360USA -> "30/360 (USA)"
    Thirty360BondBasis -> "30/360 (Bond Basis)"
    Thirty360European -> "30/360 (European)"
    Thirty360EurobondBasis -> "30/360 (Eurobond Basis)"
    Thirty360Italian -> "30/360 (Italian)"
    Actual365Fixed -> "Actual/365 (Fixed)"

-- | Number of days w.r.t. a given day counter for a given period.
dayCount :: DayCounter -> Date -> Date -> Int

dayCount Actual360 (Date serial1) (Date serial2) = fromIntegral(serial2-serial1)

dayCount Thirty360 date1 date2 = 360*(y2-y1) + 30*(m2'-m1-1) + max 0 30-d1 + min 30 d2'
                                                       where y2 = year date2
                                                             y1 = year date1
                                                             m2 = month date2
                                                             m1 = month date1
                                                             d2 = dayOfMonth date2
                                                             d1 = dayOfMonth date1
                                                             (d2',m2') = if d2 == 31 && d1 < 30
                                                                         then (1,m2+1)
                                                                         else (d2,m2)

dayCount Thirty360USA date1 date2 = dayCount Thirty360 date1 date2
dayCount Thirty360BondBasis date1 date2 = dayCount Thirty360 date1 date2

dayCount Thirty360European date1 date2 = 360*(y2-y1) + 30*(m2-m1-1) + max 0 30-d1 + min 30 d2
                                                       where y2 = year date2
                                                             y1 = year date1
                                                             m2 = month date2
                                                             m1 = month date1
                                                             d2 = dayOfMonth date2
                                                             d1 = dayOfMonth date1
dayCount Thirty360EurobondBasis date1 date2 = dayCount Thirty360European date1 date2

dayCount Thirty360Italian date1 date2 = 360*(y2-y1) + 30*(m2-m1-1) + max 0 30-d1' + min 30 d2'
                                                       where y2 = year date2
                                                             y1 = year date1
                                                             m2 = month date2
                                                             m1 = month date1
                                                             d2 = dayOfMonth date2
                                                             d1 = dayOfMonth date1
                                                             d2' = if m2 == 2 && d2 > 27 then 30 else d2
                                                             d1' = if m1 == 2 && d1 > 27 then 30 else d1

dayCount Actual365Fixed (Date serial1) (Date serial2) = serial2-serial1

-- | Yearfraction w.r.t. a given day counter and an optional reference period.
--   The latter is only relevant for some day counters such as Act/Act.
yearFraction :: DayCounter -> Date -> Date -> Maybe Date -> Maybe Date -> Double

yearFraction dayCounter date1 date2 _ _

  | (dayCounter == Actual360 || dayCounter == Thirty360 || dayCounter == Thirty360USA ||
    dayCounter == Thirty360BondBasis || dayCounter == Thirty360European || dayCounter == Thirty360EurobondBasis ||
    dayCounter == Thirty360Italian) = fromIntegral(dayCount dayCounter date1 date2) / 360.0

  | dayCounter == Actual365Fixed = fromIntegral(dayCount dayCounter date1 date2) / 365.0

  | otherwise = error ("unknown day counter " ++ (show dayCounter))
