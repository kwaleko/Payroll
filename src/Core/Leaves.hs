module Core.Leaves where

import Core.Types

import Data.Time
import Data.Tuple.Extra
import Time.Types
import Data.Hourglass


class Monad m => HasLeave m where
  createLeave :: LeaveRequest -> m (Maybe LeaveId)

mm = dateMonth
yyyy = dateYear
dd = dateDay

adjCreation :: LeaveRequest -> [AbsenceJournal]
adjCreation = undefined


{-mkAbsence :: Day -> Day -> [(Day,Day)]
mkAbsence fdate tdate  | sameM fdate tdate && sameY fdate tdate = [(fdate,tdate)]
  where
    sameM :: Day -> Day -> Bool
    sameM fdate tdate = mm fdate == mm tdate
    sameY :: Day -> Day -> Bool
    sameY fdate tdate = yyyy fdate == yyyy tdate
    mm :: Day -> Int
    mm date = snd3 $ toGregorian date
    yyyy date = fst3 $ toGregorian date
-}

fn :: FromDate -> ToDate -> Either String [(FromDate,ToDate)]
fn fdate tdate | not (isDateValid fdate && isDateValid tdate) = Left "err"
fn fdate tdate |  sameMMYYYY =   return [ (,) fdate tdate]
  where
    sameMMYYYY  = sameMonth && sameYear
    sameMonth   = dateMonth fdate == dateMonth tdate
    sameYear    = dateYear fdate  == dateYear tdate
fn fdate tdate  =  (:)  tuple <$>  res
  where
    tuple = (,) fdate (endOfMonth fdate)
    res =  fn (nDate fdate) tdate
    nDate d = case  (dateMonth d ) of
      December -> Date ( (yyyy d) + 1) (month) 01
      _        -> Date (yyyy d) (nMonth month) 01
      where
        month = mm d
        years = yyyy d

isDateValid :: Date -> Bool
isDateValid date = undefined 

    --endOfMonth = undefined
endOfMonth :: Date -> Date
endOfMonth date = Date (yyyy date) (mm date) maxDay
  where
    maxDay = daysInMonth (yyyy date) (mm date)

nMonth :: Month -> Month
nMonth mm = case mm of
  January   -> February
  February  -> March
  March     -> April
  April     -> May
  May       -> June
  June      -> July
  July      -> August
  August    -> September
  September -> October
  October   -> November
  November  -> December
  December  -> January
--nextMonth :: Date -> Date
--nextMonth date = Date (dateYear date) (dateMonth date) (nDays
--  where
 --   nDays = daysInMonth (dateYear date) (dateMonth date)