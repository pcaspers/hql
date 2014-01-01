{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module contains the definition of the date type and
--   associated functions.

module Hql.Time.Date (Date(Date),
                      date,
                      dayOfMonth,
                      dayOfYear,
                      isLeapYear,
                      minus,
                      minusPeriod,
                      month,
                      monthLength,
                      plus,
                      plusPeriod,
                      serialNumber,
                      weekday,
                      year
                     ) where

import Hql.Time.Period

-- | Type representing dates in terms of a serial number.
newtype Date = Date Int deriving (Eq, Ord)

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

-- | True if a date object is constructed with a valid serial number
checkSerialNumber :: Date -> Bool
checkSerialNumber (Date serialnumber) = if serialnumber >= 367 && serialnumber <= 109574 then True else False -- Jan 1st 1901 to Dec 31st 2199

-- | The serial number of a date
serialNumber :: Date -> Int
serialNumber (Date serial) = if checkSerialNumber(Date serial) then serial else error ("invalid serial number " ++ show serial)

-- | Weekday of a date, where 1 represents Sunday, 2 Monday, ... , 7 Saturday
weekday :: Date -> Int
weekday date = if wd == 0 then 7 else wd
     where wd = mod (serialNumber date) 7

-- | Day index within a year
dayOfYear :: Date -> Int
dayOfYear date = (serialNumber date) - (yearOffset.year) date

-- | Day index within a month
dayOfMonth :: Date -> Int
dayOfMonth date = dayOfYear date - monthOffset (month date) ((isLeapYear.year) date)

-- | Add an integer to a date's serial number
plus :: Date -> Int -> Date
plus date days = if checkSerialNumber res then res else error ("invalid serial number " ++ show res)
    where res = Date (serialNumber date + days)

-- | Subtract an integer from a date's serial number
minus :: Date -> Int -> Date
minus date days = if checkSerialNumber res then res else error ("invalid serial number " ++ show res)
    where res = Date (serialNumber date - days)

-- | Add a period to a date
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

-- | Subtract a period from a date
minusPeriod :: Date -> Period -> Date
minusPeriod d (Period n unit) = plusPeriod d (Period (-n) unit)

-- | Year of a date
year :: Date -> Int
year date = year + correction
    where serial = serialNumber date
          year = ( div serial 365 ) + 1900
          correction = if serial <= yearOffset year then -1 else 0

-- | Month of a date
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

-- | Construct a date from given day, month, year
date :: Int -> Int -> Int -> Date
date day month year
    | day >= 1 && day <= mLen = Date (day + mOffset + yOffset)
    | otherwise = error ("day " ++ show day ++ " outside month day range 1.." ++ show mLen)
    where isLeap = isLeapYear year
          mLen = monthLength month isLeap
          mOffset = monthOffset month isLeap
          yOffset = yearOffset year

-- | True if given year is a leap year
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

-- | Day offset due to given month. If leap is True February is considered to have 29 days.
monthOffset :: Int -> Bool -> Int
monthOffset month leap
   | month >= 1 && month <=12 = case leap of
                                   False -> monthOffsetList !! (month-1)
                                   True -> monthLeapOffsetList !! (month-1)
   | otherwise = error ("month " ++ show month ++ " out of range")
   where monthOffsetList = [ 0,  31,  59,  90, 120, 151, 181, 212, 243, 273, 304, 334, 365 ]
         monthLeapOffsetList = [ 0,  31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 ]

-- | Length of month. If leap is True February is considered to have 29 days.
monthLength :: Int -> Bool -> Int
monthLength month leap
   | month >= 1 && month <= 12 = case leap of
                                   False -> monthLengthList !! (month-1)
                                   True -> monthLeapLengthList !! (month-1)
   | otherwise = error ("month " ++ show month ++ " out of range")
   where monthLengthList = [ 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]
         monthLeapLengthList = [ 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ]

-- | Day offset due to given year.
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
