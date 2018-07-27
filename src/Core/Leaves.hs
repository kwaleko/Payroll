{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Core.Leaves where

import Core.Types

import Data.Time(addGregorianMonthsClip
                ,gregorianMonthLength
                ,Day(..)
                ,toGregorian
                ,fromGregorian)
import Data.Tuple.Extra
--import Time.Types
--import Data.Hourglass


class Monad m => HasLeave m where
  createLeave :: LeaveRequest -> m (Maybe LeaveId)


absCreation :: LeaveRequest -> Maybe AbsenceJournal
absCreation leave = case reqWorkflowState leave of
  Approved ->
    let
      empNb   = reqEmpl leave
      fromDate = reqFromDate leave
      toDate   = reqToDate   leave
      days     = split fromDate toDate
    in
      Just $ AbsenceJournal  "" []  ""
  _        -> Nothing

-- the date for a leave request should be splited on
-- a monthly basis so the payrun could deduct the
-- appropriate amount based on the number of days
split :: Day -> Day -> [(Day,Day)]
split fdate tdate = if lstMDay fdate >= tdate
  then (:) (fdate,tdate)  []
  else (:) tuple rem
  where
    tuple = (,) fdate $ lstMDay fdate
    rem   = split (addGregorianMonthsClip 1 $ fstMDay fdate) tdate

setDay :: Int -> (Int -> Int) -> Day -> Day
setDay nb fn day = let
  (year,month,_) = toGregorian day
  in fromGregorian year month $ fn nb

fstMDay :: Day -> Day
fstMDay day  = setDay 01 id day

lstMDay :: Day -> Day
lstMDay day =
  let (year,month,_) = toGregorian day
  in fromGregorian year month $ gregorianMonthLength year month



