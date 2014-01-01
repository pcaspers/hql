{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module contains holiday calendar definitions and
--   functions to apply adjustments to dates
module Hql.Time.Calendar (BusinessDayConvention(..),
                          Calendar(..),
                          adjust,
                          advanceByPeriod,
                          advanceByUnits,
                          endOfMonth,
                          isBusinessDay,
                          isEndOfMonth,
                          isHoliday,
                         ) where

import Hql.Time.Date
import Hql.Time.Period

-- | These conventions specify the algorithm used to adjust a date in case
--   it is not a valid business day.
data BusinessDayConvention = Following |          -- ^ Choose the first business day after the given holiday.
                             ModifiedFollowing |  -- ^ Choose the first business day after
                                                  -- the given holiday unless it belongs
                                                  -- to a different month, in which case
                                                  -- choose the first business day before
                                                  -- the holiday.
                             Preceding |          -- ^ Choose the first business day before
                                                  -- the given holiday.
                             ModifiedPreceding |  -- ^ Choose the first business day before
                                                  -- the given holiday unless it belongs
                                                  -- to a different month, in which case
                                                  -- choose the first business day after
                                                  -- the holiday.
                             Unadjusted           -- ^ Do not adjust.
                           deriving (Eq,Show)

-- | Calendar definitions
data Calendar = NullCalendar |                    -- ^ No holidays
                WeekendsOnly |                    -- ^ Only weekends are holidays
                TARGET |                          -- ^ TARGET calendar, see <http://www.ecb.int>
                JointCalendar Calendar Calendar   -- ^ Calendar composed by two calendars
              deriving (Eq, Show)

-- | True if date is a business day w.r.t. a given calendar
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

-- | True if date is a holiday w.r.t. a given calendar
isHoliday :: Calendar -> Date -> Bool
isHoliday calendar date = not(isBusinessDay calendar date)

-- | True if date is a weekend w.r.t. a given calendar
isWeekend :: Calendar -> Int -> Bool
isWeekend calendar weekday
  | weekday < 1 || weekday > 7 = error("weekday " ++ show weekday ++ " not allowed")
  | calendar == NullCalendar = False
  | otherwise = weekday == 1 || weekday == 7

-- | Day of year index for easter monday in western calendar implementations
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

-- | Day of year index for easter monday in orthodox calendar implementations
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

-- | Adjust date w.r.t. given calendar and business day convention
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

-- | True if date is the last business day in month w.r.t. a given calendar
isEndOfMonth :: Calendar -> Date -> Bool
isEndOfMonth calendar date = month(date) /= month (adjust calendar (date `plus` 1) Following)

-- | Last business day in a month for a given date w.r.t. a given calendar
endOfMonth :: Calendar -> Date -> Date
endOfMonth NullCalendar d = date (monthLength (month d) (isLeapYear (year d))) (month d) (year d)
endOfMonth calendar d = adjust calendar (endOfMonth NullCalendar d) Preceding

-- | Advances a date w.r.t. a given calendar and business day convention by a given period
--   If end of month (eom) is set to True an end of month date will be also an end of month
--   date after the shift.
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
-- | Same as advanceByUnits, only the period is given directly as a period, not in terms of units.
advanceByPeriod :: Calendar -> Date -> Period -> BusinessDayConvention -> Bool -> Date
advanceByPeriod calendar date (Period n unit) bdc endOfMonth = advanceByUnits calendar date n unit bdc endOfMonth
