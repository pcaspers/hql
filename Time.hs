module Time (BusinessDayConvention(..),
             Calendar(..),
             Date(Date),
             DateGenerationRule(..),
             DayCounter(..),
             Frequency(..),
             Period(Period),
             Schedule(..),
             Unit(..),
             adjust,
             advanceByPeriod,
             advanceByUnits,
             date,
             dayCount,
             dayOfMonth,
             dayOfYear,
             endOfMonth,
             frequencyPerYear,
             isBusinessDay,
             isEndOfMonth,
             isHoliday,
             isLeapYear,
             minus,
             minusPeriod,
             month,
             plus,
             plusPeriod,
             schedule,
             serialNumber,
             weekday,
             year,
             yearFraction
            ) where

import Data.List(nub)

newtype Date = Date Int deriving (Eq, Ord)

data Unit = Days | Weeks | Months | Years deriving (Eq, Show)
data Period = Period { numberOfUnits :: Int, unit :: Unit }

instance Show Period where
     show (Period x u) = show x ++ " " ++ show u

instance Show Date where
     show d = monthDesc ++ " " ++ ((show.dayOfMonth) d) ++ ", " ++ ((show.year) d)
       where  m = month d
              monthDesc = case m of
                          1 -> "January"
                          2 -> "February"
                          3 -> "March"
                          4 -> "April"
                          5 -> "May"
                          6 -> "June"
                          7 -> "July"
                          8 -> "August"
                          9 -> "September"
                          10 -> "October"
                          11 -> "November"
                          12 -> "December"

checkSerialNumber :: Date -> Bool
checkSerialNumber (Date serialnumber) = if serialnumber >= 367 && serialnumber <= 109574 then True else False -- Jan 1st 1901 to Dec 31st 2199

serialNumber :: Date -> Int
serialNumber (Date serial) = if checkSerialNumber(Date serial) then serial else error ("invalid serial number " ++ show serial)

weekday :: Date -> Int
weekday date = if wd == 0 then 7 else wd
     where wd = mod (serialNumber date) 7

dayOfYear :: Date -> Int
dayOfYear date = (serialNumber date) - (yearOffset.year) date

dayOfMonth :: Date -> Int
dayOfMonth date = dayOfYear date - monthOffset (month date) ((isLeapYear.year) date)

plus :: Date -> Int -> Date
plus date days = if checkSerialNumber res then res else error ("invalid serial number " ++ show res)
    where res = Date (serialNumber date + days)

minus :: Date -> Int -> Date
minus date days = if checkSerialNumber res then res else error ("invalid serial number " ++ show res)
    where res = Date (serialNumber date - days)

plusPeriod :: Date -> Period -> Date
plusPeriod dat (Period n Days) = dat `plus` n
plusPeriod dat (Period n Weeks) = dat `plus` (7*n)
plusPeriod dat (Period n Months) = if y' < 1900 || y' > 2199
                        then error("year " ++ show y' ++ " out of range [1901,2199]")
                        else date d' m' y'
  where d = dayOfMonth dat
        m = (month dat) + n
        y = (year dat)
        (m',y') = adjust (m,y)
        d' = min d (monthLength m' (isLeapYear y'))
        adjust (mm,yy)
          | mm > 12 = adjust (mm-12,yy+1)
          | mm < 1  = adjust (mm+12,yy-1)
          | otherwise = (mm,yy)
plusPeriod dat (Period n Years) = if y < 1900 || y > 2199
                         then error("year " ++ show y ++ " out of range [1901,2199]")
                         else date d' m y
  where d = dayOfMonth dat
        m = month dat
        y = (year dat) + n
        d'
          | d == 29 && m == 2 && (not (isLeapYear y)) = 28
          | otherwise = d

minusPeriod :: Date -> Period -> Date
minusPeriod d (Period n unit) = plusPeriod d (Period (-n) unit)

year :: Date -> Int
year date = year + correction
    where serial = serialNumber date
          year = ( div serial 365 ) + 1900
          correction = if serial <= yearOffset year then -1 else 0

month :: Date -> Int
month date = m''
    where d = dayOfYear date
          m = ( div d 30 ) + 1
          isLeap = (isLeapYear.year) date
          m' = correctm m
          m'' = correctm' m'
          correctm x
             | (d <= monthOffset x isLeap) = correctm (x-1)
             | otherwise = x
          correctm' x
             | (d > monthOffset (x+1) isLeap) = correctm' (x+1)
             | otherwise = x

date :: Int -> Int -> Int -> Date
date day month year
    | day >= 1 && day <= mLen = Date (day + mOffset + yOffset)
    | otherwise = error ("day " ++ show day ++ " outside month day range 1.." ++ show mLen)
    where isLeap = isLeapYear year
          mLen = monthLength month isLeap
          mOffset = monthOffset month isLeap
          yOffset = yearOffset year

isLeapYear :: Int -> Bool
isLeapYear year
    | year >= 1900 && year <= 2200 = isLeapYearList !! (year-1900)
    | otherwise = error ("year " ++ show year ++ " out of range")
    where isLeapYearList = [
            -- 1900-1909
            True,False,False,False, True,False,False,False, True,False,
            -- 1910-1919
            False,False, True,False,False,False, True,False,False,False,
            -- 1920-1929
             True,False,False,False, True,False,False,False, True,False,
            -- 1930-1939
            False,False, True,False,False,False, True,False,False,False,
            -- 1940-1949
             True,False,False,False, True,False,False,False, True,False,
            -- 1950-1959
            False,False, True,False,False,False, True,False,False,False,
            -- 1960-1969
             True,False,False,False, True,False,False,False, True,False,
            -- 1970-1979
            False,False, True,False,False,False, True,False,False,False,
            -- 1980-1989
             True,False,False,False, True,False,False,False, True,False,
            -- 1990-1999
            False,False, True,False,False,False, True,False,False,False,
            -- 2000-2009
             True,False,False,False, True,False,False,False, True,False,
            -- 2010-2019
            False,False, True,False,False,False, True,False,False,False,
            -- 2020-2029
             True,False,False,False, True,False,False,False, True,False,
            -- 2030-2039
            False,False, True,False,False,False, True,False,False,False,
            -- 2040-2049
             True,False,False,False, True,False,False,False, True,False,
            -- 2050-2059
            False,False, True,False,False,False, True,False,False,False,
            -- 2060-2069
             True,False,False,False, True,False,False,False, True,False,
            -- 2070-2079
            False,False, True,False,False,False, True,False,False,False,
            -- 2080-2089
             True,False,False,False, True,False,False,False, True,False,
            -- 2090-2099
            False,False, True,False,False,False, True,False,False,False,
            -- 2100-2109
            False,False,False,False, True,False,False,False, True,False,
            -- 2110-2119
            False,False, True,False,False,False, True,False,False,False,
            -- 2120-2129
             True,False,False,False, True,False,False,False, True,False,
            -- 2130-2139
            False,False, True,False,False,False, True,False,False,False,
            -- 2140-2149
             True,False,False,False, True,False,False,False, True,False,
            -- 2150-2159
            False,False, True,False,False,False, True,False,False,False,
            -- 2160-2169
             True,False,False,False, True,False,False,False, True,False,
            -- 2170-2179
            False,False, True,False,False,False, True,False,False,False,
            -- 2180-2189
             True,False,False,False, True,False,False,False, True,False,
            -- 2190-2199
            False,False, True,False,False,False, True,False,False,False,
            -- 2200
            False ]

monthOffset :: Int -> Bool -> Int
monthOffset month leap
   | month >= 1 && month <=12 = case leap of
                                   False -> monthOffsetList !! (month-1)
                                   True -> monthLeapOffsetList !! (month-1)
   | otherwise = error ("month " ++ show month ++ " out of range")
   where monthOffsetList = [ 0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ]
         monthLeapOffsetList = [ 0,  31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 ]

monthLength :: Int -> Bool -> Int
monthLength month leap
   | month >= 1 && month <= 12 = case leap of
                                   False -> monthLengthList !! (month-1)
                                   True -> monthLeapLengthList !! (month-1)
   | otherwise = error ("month " ++ show month ++ " out of range")
   where monthLengthList = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
         monthLeapLengthList = [ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

yearOffset :: Int -> Int
yearOffset year
   | year >= 1900 && year <= 2200 = yearOffsetList !! (year-1900)
   | otherwise = error ("year " ++ show year ++ " out of range")
   where yearOffsetList = [ -- 1900-1909
                0,  366,  731, 1096, 1461, 1827, 2192, 2557, 2922, 3288,
            -- 1910-1919
             3653, 4018, 4383, 4749, 5114, 5479, 5844, 6210, 6575, 6940,
            -- 1920-1929
             7305, 7671, 8036, 8401, 8766, 9132, 9497, 9862,10227,10593,
            -- 1930-1939
            10958,11323,11688,12054,12419,12784,13149,13515,13880,14245,
            -- 1940-1949
            14610,14976,15341,15706,16071,16437,16802,17167,17532,17898,
            -- 1950-1959
            18263,18628,18993,19359,19724,20089,20454,20820,21185,21550,
            -- 1960-1969
            21915,22281,22646,23011,23376,23742,24107,24472,24837,25203,
            -- 1970-1979
            25568,25933,26298,26664,27029,27394,27759,28125,28490,28855,
            -- 1980-1989
            29220,29586,29951,30316,30681,31047,31412,31777,32142,32508,
            -- 1990-1999
            32873,33238,33603,33969,34334,34699,35064,35430,35795,36160,
            -- 2000-2009
            36525,36891,37256,37621,37986,38352,38717,39082,39447,39813,
            -- 2010-2019
            40178,40543,40908,41274,41639,42004,42369,42735,43100,43465,
            -- 2020-2029
            43830,44196,44561,44926,45291,45657,46022,46387,46752,47118,
            -- 2030-2039
            47483,47848,48213,48579,48944,49309,49674,50040,50405,50770,
            -- 2040-2049
            51135,51501,51866,52231,52596,52962,53327,53692,54057,54423,
            -- 2050-2059
            54788,55153,55518,55884,56249,56614,56979,57345,57710,58075,
            -- 2060-2069
            58440,58806,59171,59536,59901,60267,60632,60997,61362,61728,
            -- 2070-2079
            62093,62458,62823,63189,63554,63919,64284,64650,65015,65380,
            -- 2080-2089
            65745,66111,66476,66841,67206,67572,67937,68302,68667,69033,
            -- 2090-2099
            69398,69763,70128,70494,70859,71224,71589,71955,72320,72685,
            -- 2100-2109
            73050,73415,73780,74145,74510,74876,75241,75606,75971,76337,
            -- 2110-2119
            76702,77067,77432,77798,78163,78528,78893,79259,79624,79989,
            -- 2120-2129
            80354,80720,81085,81450,81815,82181,82546,82911,83276,83642,
            -- 2130-2139
            84007,84372,84737,85103,85468,85833,86198,86564,86929,87294,
            -- 2140-2149
            87659,88025,88390,88755,89120,89486,89851,90216,90581,90947,
            -- 2150-2159
            91312,91677,92042,92408,92773,93138,93503,93869,94234,94599,
            -- 2160-2169
            94964,95330,95695,96060,96425,96791,97156,97521,97886,98252,
            -- 2170-2179
            98617,98982,99347,99713,100078,100443,100808,101174,101539,101904,
            -- 2180-2189
            102269,102635,103000,103365,103730,104096,104461,104826,105191,105557,
            -- 2190-2199
            105922,106287,106652,107018,107383,107748,108113,108479,108844,109209,
            -- 2200
            109574 ]

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


data BusinessDayConvention = Following | ModifiedFollowing | Preceding | ModifiedPreceding | Unadjusted deriving (Eq,Show)

data Calendar = NullCalendar | WeekendsOnly | TARGET |
                JointCalendar Calendar Calendar deriving (Eq, Show)

isBusinessDay :: Calendar -> Date -> Bool

isBusinessDay NullCalendar _ = True

isBusinessDay WeekendsOnly date = isWeekend WeekendsOnly (weekday date)

isBusinessDay TARGET date
  | isWeekend TARGET (weekday date) = False
  | d == 1 && m == 1 = False
  | dd == em-3 && y >= 2000 = False
  | dd == em && y >= 2000 = False
  | d == 1 && m == 5 && y >= 2000 = False
  | d == 25 && m == 12 = False
  | d == 26 && m == 12 && y >= 2000 = False
  | d == 31 && m == 12 && (y == 1998 || y == 1999 || y == 2001) = False
  | otherwise = True
  where d = dayOfMonth date
        dd = dayOfYear date
        m = month date
        y = year date
        em = easterMondayWestern y

isBusinessDay (JointCalendar calendar1 calendar2) date = (isBusinessDay calendar1 date) && (isBusinessDay calendar2 date)

isHoliday :: Calendar -> Date -> Bool
isHoliday calendar date = not(isBusinessDay calendar date)

isWeekend :: Calendar -> Int -> Bool
isWeekend calendar weekday
  | weekday < 1 || weekday > 7 = error("weekday " ++ show weekday ++ " not allowed")
  | calendar == NullCalendar = False
  | otherwise = weekday == 1 || weekday == 7

easterMondayWestern :: Int -> Int
easterMondayWestern year
  | year < 1901 || year > 2199 = error("year " ++ show year ++ " not admissable")
  | otherwise = easterMonday !! (year-1901)
                where easterMonday = [
                             98,  90, 103,  95, 114, 106,  91, 111, 102,    -- 1901-1909
                        87, 107,  99,  83, 103,  95, 115,  99,  91, 111,    -- 1910-1919
                        96,  87, 107,  92, 112, 103,  95, 108, 100,  91,    -- 1920-1929
                        111,  96,  88, 107,  92, 112, 104,  88, 108, 100,   -- 1930-1939
                        85, 104,  96, 116, 101,  92, 112,  97,  89, 108,    -- 1940-1949
                        100,  85, 105,  96, 109, 101,  93, 112,  97,  89,   -- 1950-1959
                        109,  93, 113, 105,  90, 109, 101,  86, 106,  97,   -- 1960-1969
                        89, 102,  94, 113, 105,  90, 110, 101,  86, 106,    -- 1970-1979
                        98, 110, 102,  94, 114,  98,  90, 110,  95,  86,    -- 1980-1989
                        106,  91, 111, 102,  94, 107,  99,  90, 103,  95,   -- 1990-1999
                        115, 106,  91, 111, 103,  87, 107,  99,  84, 103,   -- 2000-2009
                        95, 115, 100,  91, 111,  96,  88, 107,  92, 112,    -- 2010-2019
                        104,  95, 108, 100,  92, 111,  96,  88, 108,  92,   -- 2020-2029
                        112, 104,  89, 108, 100,  85, 105,  96, 116, 101,   -- 2030-2039
                        93, 112,  97,  89, 109, 100,  85, 105,  97, 109,    -- 2040-2049
                        101,  93, 113,  97,  89, 109,  94, 113, 105,  90,   -- 2050-2059
                        110, 101,  86, 106,  98,  89, 102,  94, 114, 105,   -- 2060-2069
                        90, 110, 102,  86, 106,  98, 111, 102,  94, 114,    -- 2070-2079
                        99,  90, 110,  95,  87, 106,  91, 111, 103,  94,    -- 2080-2089
                        107,  99,  91, 103,  95, 115, 107,  91, 111, 103,   -- 2090-2099
                        88, 108, 100,  85, 105,  96, 109, 101,  93, 112,    -- 2100-2109
                        97,  89, 109,  93, 113, 105,  90, 109, 101,  86,    -- 2110-2119
                        106,  97,  89, 102,  94, 113, 105,  90, 110, 101,   -- 2120-2129
                        86, 106,  98, 110, 102,  94, 114,  98,  90, 110,    -- 2130-2139
                        95,  86, 106,  91, 111, 102,  94, 107,  99,  90,    -- 2140-2149
                        103,  95, 115, 106,  91, 111, 103,  87, 107,  99,   -- 2150-2159
                        84, 103,  95, 115, 100,  91, 111,  96,  88, 107,    -- 2160-2169
                        92, 112, 104,  95, 108, 100,  92, 111,  96,  88,    -- 2170-2179
                        108,  92, 112, 104,  89, 108, 100,  85, 105,  96,   -- 2180-2189
                        116, 101,  93, 112,  97,  89, 109, 100,  85, 105 ]  -- 2190-2199

easterMondayOrthodox :: Int -> Int
easterMondayOrthodox year
  | year < 1901 || year > 2199 = error("year " ++ show year ++ " not admissable")
  | otherwise = easterMonday !! (year-1901)
                where easterMonday = [
                             105, 118, 110, 102, 121, 106, 126, 118, 102,   -- 1901-1909
                        122, 114,  99, 118, 110,  95, 115, 106, 126, 111,   -- 1910-1919
                        103, 122, 107,  99, 119, 110, 123, 115, 107, 126,   -- 1920-1929
                        111, 103, 123, 107,  99, 119, 104, 123, 115, 100,   -- 1930-1939
                        120, 111,  96, 116, 108, 127, 112, 104, 124, 115,   -- 1940-1949
                        100, 120, 112,  96, 116, 108, 128, 112, 104, 124,   -- 1950-1959
                        109, 100, 120, 105, 125, 116, 101, 121, 113, 104,   -- 1960-1969
                        117, 109, 101, 120, 105, 125, 117, 101, 121, 113,   -- 1970-1979
                        98, 117, 109, 129, 114, 105, 125, 110, 102, 121,    -- 1980-1989
                        106,  98, 118, 109, 122, 114, 106, 118, 110, 102,   -- 1990-1999
                        122, 106, 126, 118, 103, 122, 114,  99, 119, 110,   -- 2000-2009
                        95, 115, 107, 126, 111, 103, 123, 107,  99, 119,    -- 2010-2019
                        111, 123, 115, 107, 127, 111, 103, 123, 108,  99,   -- 2020-2029
                        119, 104, 124, 115, 100, 120, 112,  96, 116, 108,   -- 2030-2039
                        128, 112, 104, 124, 116, 100, 120, 112,  97, 116,   -- 2040-2049
                        108, 128, 113, 104, 124, 109, 101, 120, 105, 125,   -- 2050-2059
                        117, 101, 121, 113, 105, 117, 109, 101, 121, 105,   -- 2060-2069
                        125, 110, 102, 121, 113,  98, 118, 109, 129, 114,   -- 2070-2079
                        106, 125, 110, 102, 122, 106,  98, 118, 110, 122,   -- 2080-2089
                        114,  99, 119, 110, 102, 115, 107, 126, 118, 103,   -- 2090-2099
                        123, 115, 100, 120, 112,  96, 116, 108, 128, 112,   -- 2100-2109
                        104, 124, 109, 100, 120, 105, 125, 116, 108, 121,   -- 2110-2119
                        113, 104, 124, 109, 101, 120, 105, 125, 117, 101,   -- 2120-2129
                        121, 113,  98, 117, 109, 129, 114, 105, 125, 110,   -- 2130-2139
                        102, 121, 113,  98, 118, 109, 129, 114, 106, 125,   -- 2140-2149
                        110, 102, 122, 106, 126, 118, 103, 122, 114,  99,   -- 2150-2159
                        119, 110, 102, 115, 107, 126, 111, 103, 123, 114,   -- 2160-2169
                        99, 119, 111, 130, 115, 107, 127, 111, 103, 123,    -- 2170-2179
                        108,  99, 119, 104, 124, 115, 100, 120, 112, 103,   -- 2180-2189
                        116, 108, 128, 119, 104, 124, 116, 100, 120, 112 ]  -- 2190-2199

adjust :: Calendar -> Date -> BusinessDayConvention -> Date
adjust calendar date bdc = case bdc of
  Unadjusted -> date
  Following  -> forwardOverHolidays calendar date
  ModifiedFollowing -> if month(d') /= month(date)
                               then adjust calendar date Preceding
                               else d'
                               where d' = adjust calendar date Following
  Preceding -> backwardOverHolidays calendar date
  ModifiedPreceding -> if month(d') /= month(date)
                               then adjust calendar date Following
                               else d'
                               where d' = adjust calendar date Preceding

isEndOfMonth :: Calendar -> Date -> Bool
isEndOfMonth calendar date = month(date) /= month (adjust calendar (date `plus` 1) Following)

endOfMonth :: Calendar -> Date -> Date
endOfMonth NullCalendar d = date (monthLength (month d) (isLeapYear (year d))) (month d) (year d)
endOfMonth calendar d = adjust calendar (endOfMonth NullCalendar d) Preceding


advanceByUnits :: Calendar -> Date -> Int -> Unit -> BusinessDayConvention -> Bool -> Date
advanceByUnits calendar d 0 _ bdc _ = adjust calendar d bdc
advanceByUnits calendar d n Days bdc _
  | n > 0 = advanceByUnits calendar (forwardOverHolidays calendar d) (n-1) Days bdc False
  | n < 0 = advanceByUnits calendar (backwardOverHolidays calendar d) (n+1) Days bdc False
advanceByUnits calendar d n Weeks bdc _ = adjust calendar (d `plusPeriod` (Period n Weeks)) bdc
advanceByUnits calendar d n unit bdc eom
  | eom && isEndOfMonth calendar d = endOfMonth calendar d'
  | otherwise = adjust calendar d' bdc
  where d' = d `plusPeriod` (Period n unit)

forwardOverHolidays :: Calendar -> Date -> Date
forwardOverHolidays calendar date = if isHoliday calendar date
                                    then forwardOverHolidays calendar (date `plus` 1)
                                    else date

backwardOverHolidays :: Calendar -> Date -> Date
backwardOverHolidays calendar date = if isHoliday calendar date
                                     then backwardOverHolidays calendar (date `minus` 1)
                                     else date

advanceByPeriod :: Calendar -> Date -> Period -> BusinessDayConvention -> Bool -> Date
advanceByPeriod calendar date (Period n unit) bdc endOfMonth = advanceByUnits calendar date n unit bdc endOfMonth

-- businessDaysBetween :: Calendar -> Date -> Date -> Bool -> Bool -> Int

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

data DateGenerationRule = Backward |
                          Forward |
                          Zero |
                          ThirdWednesday |
                          Twentieth |
                          TwentiethIMM |
                          OldCDS |
                          CDS deriving (Eq, Show)

type Schedule = [Date]

schedule :: Date -> Date -> Period -> Calendar -> BusinessDayConvention -> BusinessDayConvention
            -> DateGenerationRule -> Bool -> Maybe Date -> Maybe Date -> Schedule

schedule effective termination _ _ _ _ Zero _ _ _ = [effective, termination]

schedule effective termination (Period n unit) calendar bdc terminationBdc
         Backward eom first nextToLast = nubSorted ([effective] ++ (backward 1) ++
                                         [(adjust calendar seed bdc), (adjust calendar termination bdc)])
  where exitDate = case first of
          Just d -> if d > effective && d < termination
                    then d
                    else error("first date out of effective-termination date range " ++
                               show effective ++ ", " ++ show termination)
          Nothing -> effective
        seed = case nextToLast of
          Just d -> if d > effective && d < termination
                    then d
                    else error("nextToLast date out of effective-termination date range " ++
                               show effective ++ ", " ++ show termination)
          Nothing -> termination
        backward periods
          | advanceByPeriod NullCalendar seed (Period (-periods*n) unit) bdc eom < exitDate =
            [adjust calendar exitDate bdc]
          | otherwise = (backward (periods+1)) ++ [(advanceByPeriod calendar seed (Period (-periods*n) unit) bdc eom)]
