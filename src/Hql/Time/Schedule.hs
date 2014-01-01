{-
 Copyright (C) 2013 Peter Caspers

 This file is part of hql which is a reimplementation of QuantLib,
 a free-software/open-source library for financial quantitative
 analysts and developers. Please refer to the documentation available
 at <http://quantlib.org/> for the original copyright holders.
-}

-- | This module contains the schedule type definition and helper
--   functions to construct schedules. It also contains standard
--   conventions for schedule constructions.

module Hql.Time.Schedule (Schedule(..),
                          ScheduleConvention,
                          eurSwapFixLegConv,
                          eurSwapFloatLegConv,
                          makeSchedule,
                          schedule
                         ) where

import Hql.Time.Date
import Hql.Time.Period
import Hql.Time.Calendar

-- | Rule determining how a schedule is build
data DateGenerationRule = Backward |         -- ^ Backward from termination date to effective date.
                          Forward |          -- ^ Forward from effective date to termination date.
                          Zero |             -- ^ No intermediate dates between effective date
                                             --   and termination date.
                          ThirdWednesday |   -- ^ All dates but effective date and termination
                                             --   date are taken to be on the third wednesday
                                             --   of their month (with forward calculation.)
                          Twentieth |        -- ^ All dates but the effective date are
                                             --   taken to be the twentieth of their
                                             --   month (used for CDS schedules in
                                             --   emerging markets.)  The termination
                                             --   date is also modified.
                          TwentiethIMM |     -- ^ All dates but the effective date are
                                             --   taken to be the twentieth of an IMM
                                             --   month (used for CDS schedules.)  The
                                             --   termination date is also modified.
                          OldCDS |           -- ^ Same as TwentiethIMM with unrestricted date
                                             --   ends and log/short stub coupon period (old
                                             --   CDS convention).
                          CDS                -- ^ Credit derivatives standard rule since 'Big
                                             --   Bang' changes in 2009.
                        deriving (Eq, Show)

-- | Schedule definition, it is nothing more than a list of dates
type Schedule = [Date]

-- | Schedule construction by effective and termination date, period, calendar, business day convention,
--   business day convention for termination date, date generation rule, end of month flag,
--   first date and next to last date
schedule :: Date                      -- ^ Effective date
            -> Date                   -- ^ Termination date
            -> Period                 -- ^ Period between dates
            -> Calendar               -- ^ Calendar for date adjustment
            -> BusinessDayConvention  -- ^ Business day convention for date adjustment
            -> BusinessDayConvention  -- ^ Business day convention for termination date adjustment
            -> DateGenerationRule     -- ^ Rule for schedule generation
            -> Bool                   -- ^ End of month flag
            -> Maybe Date             -- ^ First date (if not equal to effective date)
            -> Maybe Date             -- ^ Next to last date (if not equal to termination date)
            -> Schedule               -- ^ Resulting schedule

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

-- | Schedule convention data type. Summarizes conventions needed for schedule construction.
data ScheduleConvention = ScheduleConvention { tenor :: Period, calendar :: Calendar, bdc :: BusinessDayConvention, termBdc :: BusinessDayConvention, rule :: DateGenerationRule, eom :: Bool } deriving (Show, Eq)

-- | Eur swap fixed leg standard conventions
eurSwapFixLegConv = ScheduleConvention (Period 1 Years) TARGET ModifiedFollowing ModifiedFollowing Backward False

-- | Eur swap float leg standard conventions
eurSwapFloatLegConv = ScheduleConvention (Period 6 Months) TARGET ModifiedFollowing ModifiedFollowing Backward False

-- | Simplified schedule generation by convention, effective and termination date
makeSchedule :: ScheduleConvention -> Date -> Date -> Schedule
makeSchedule conv from to = schedule from to (tenor conv) (calendar conv)
                            (bdc conv) (termBdc conv) (rule conv) (eom conv) Nothing Nothing


-- | same as nub, but works correct only on a sorted list (more precisely on a list
--   where equal elements must be placed in direct neighbourhood of each other)
nubSorted :: Eq a => [a] -> [a]
nubSorted (x:y:rest) = if x==y then nubSorted (x:rest) else x:(nubSorted (y:rest))
nubSorted otherList = otherList
