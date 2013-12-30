module Time.DayCounter (DayCounter(..),
                        dayCount,
                        yearFraction) where
       
import Time.Date

data DayCounter = Actual360 |
                  Thirty360 | Thirty360USA | Thirty360BondBasis | Thirty360European |
                  Thirty360EurobondBasis | Thirty360Italian |
                  Actual365Fixed deriving (Eq)

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

dayCount :: DayCounter -> Date -> Date -> Maybe Date -> Maybe Date -> Int

dayCount Actual360 (Date serial1) (Date serial2) _ _ = fromIntegral(serial2-serial1)

dayCount Thirty360 date1 date2 _ _ = 360*(y2-y1) + 30*(m2'-m1-1) + max 0 30-d1 + min 30 d2'
                                                       where y2 = year date2
                                                             y1 = year date1
                                                             m2 = month date2
                                                             m1 = month date1
                                                             d2 = dayOfMonth date2
                                                             d1 = dayOfMonth date1
                                                             (d2',m2') = if d2 == 31 && d1 < 30
                                                                         then (1,m2+1)
                                                                         else (d2,m2)

dayCount Thirty360USA date1 date2 _ _ = dayCount Thirty360 date1 date2 undefined undefined
dayCount Thirty360BondBasis date1 date2 _ _ = dayCount Thirty360 date1 date2 undefined undefined

dayCount Thirty360European date1 date2 _ _ = 360*(y2-y1) + 30*(m2-m1-1) + max 0 30-d1 + min 30 d2
                                                       where y2 = year date2
                                                             y1 = year date1
                                                             m2 = month date2
                                                             m1 = month date1
                                                             d2 = dayOfMonth date2
                                                             d1 = dayOfMonth date1
dayCount Thirty360EurobondBasis date1 date2 _ _ = dayCount Thirty360European date1 date2 undefined undefined

dayCount Thirty360Italian date1 date2 _ _ = 360*(y2-y1) + 30*(m2-m1-1) + max 0 30-d1' + min 30 d2'
                                                       where y2 = year date2
                                                             y1 = year date1
                                                             m2 = month date2
                                                             m1 = month date1
                                                             d2 = dayOfMonth date2
                                                             d1 = dayOfMonth date1
                                                             d2' = if m2 == 2 && d2 > 27 then 30 else d2
                                                             d1' = if m1 == 2 && d1 > 27 then 30 else d1

dayCount Actual365Fixed (Date serial1) (Date serial2) _ _ = serial2-serial1

yearFraction :: DayCounter -> Date -> Date -> Maybe Date -> Maybe Date -> Double

yearFraction dayCounter date1 date2 _ _

  | (dayCounter == Actual360 || dayCounter == Thirty360 || dayCounter == Thirty360USA ||
    dayCounter == Thirty360BondBasis || dayCounter == Thirty360European || dayCounter == Thirty360EurobondBasis ||
    dayCounter == Thirty360Italian) = fromIntegral(dayCount dayCounter date1 date2 undefined undefined) / 360.0

  | dayCounter == Actual365Fixed = fromIntegral(dayCount dayCounter date1 date2 undefined undefined) / 365.0

  | otherwise = error ("unknown day counter " ++ (show dayCounter))
       
       
       
       