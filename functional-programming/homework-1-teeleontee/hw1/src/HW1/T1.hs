module HW1.T1
  ( Day (..)
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

import Numeric.Natural (Natural)

data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

nextDay :: Day -> Day
nextDay day  = case day of
    Monday    -> Tuesday
    Tuesday   -> Wednesday
    Wednesday -> Thursday
    Thursday  -> Friday
    Friday    -> Saturday
    Saturday  -> Sunday
    Sunday    -> Monday
    
applyNTimes :: (a -> a) -> a -> Natural -> a
applyNTimes _ a 0 = a
applyNTimes f a 1 = f a
applyNTimes f a n = applyNTimes f (f a) (n - 1)

afterDays :: Natural -> Day -> Day
afterDays num curDay 
  = applyNTimes nextDay curDay num

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

isFriday :: Day -> Bool
isFriday Friday = True
isFriday _      = False

calcDays :: Day -> Natural -> Natural
calcDays day n 
  = if isFriday day
    then n
    else calcDays (nextDay day) (n + 1)

daysToParty :: Day -> Natural
daysToParty day = calcDays day 0

