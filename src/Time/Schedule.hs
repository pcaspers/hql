module Time.Schedule (Schedule(..),
                      ScheduleConvention,
                      eurSwapFixLegConv,
                      eurSwapFloatLegConv,
                      makeSchedule,
                      schedule) where

import Time.Date
import Time.Period
import Time.Calendar

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

data ScheduleConvention = ScheduleConvention { tenor :: Period, calendar :: Calendar, bdc :: BusinessDayConvention, termBdc :: BusinessDayConvention, rule :: DateGenerationRule, eom :: Bool } deriving (Show, Eq)

eurSwapFixLegConv = ScheduleConvention (Period 1 Years) TARGET ModifiedFollowing ModifiedFollowing Backward False
eurSwapFloatLegConv = ScheduleConvention (Period 6 Months) TARGET ModifiedFollowing ModifiedFollowing Backward False

makeSchedule :: ScheduleConvention -> Date -> Date -> Schedule
makeSchedule conv from to = schedule from to (tenor conv) (calendar conv)
                            (bdc conv) (termBdc conv) (rule conv) (eom conv) Nothing Nothing


-- same as nub, but works correct only on a sorted list (more precisely on a list
-- where equal elements must be placed in direct neighbourhood of each other)
nubSorted :: Eq a => [a] -> [a]
nubSorted (x:y:rest) = if x==y then nubSorted (x:rest) else x:(nubSorted (y:rest))
nubSorted otherList = otherList
