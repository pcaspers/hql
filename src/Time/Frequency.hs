module Time.Frequency (Frequency(..),
                       frequencyPerYear) where

data Frequency = NoFrequency |
                 Once |
                 Annual |
                 Semiannual |
                 EveryFourMonth |
                 Quarterly |
                 Bimonthly |
                 Monthly |
                 EveryFourthWeek |
                 Biweekly |
                 Weekly |
                 Daily |
                 OtherFrequency deriving (Eq, Show)

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
