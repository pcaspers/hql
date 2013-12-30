module Time.Period (Period(Period),
                    Unit(..)) where

data Unit = Days | Weeks | Months | Years deriving (Eq, Show)
data Period = Period { numberOfUnits :: Int, unit :: Unit } deriving Eq

instance Show Period where
     show (Period x u) = show x ++ " " ++ show u
