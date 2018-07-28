{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Core.Leaves where

import Core.Types

import Data.Time(addGregorianMonthsClip
                ,gregorianMonthLength
                ,Day(..)
                ,toGregorian
                ,fromGregorian)


class Monad m => HasLeave m where
  createLeave :: LeaveRequest -> m (Maybe LeaveId)
  getLeaveSetup :: LeaveType -> m (Maybe LeaveSetup)

class Monad m => HasAbsence m where
  createAbsence :: AbsenceJournal -> m ()
  getAbsence :: PeriodFrom -> PeriodTo -> EmpNumber -> m [AbsenceJournal]

class Monad m => HasFormula m where
  getFormulas :: m [Formula]


calcDeduction :: (HasAbsence m) => Employee -> PayrollConfig -> m [PayrollRecord]
calcDeduction emp pconf = return  [PayrollRecord  (fromGregorian 2019 01 01) (fromGregorian 2019 01 01) (Paycode " " "" Deduction GOSI ) "" 10]

absGen :: LeaveRequest -> Maybe AbsenceJournal
absGen LeaveRequest{..} =
  let
    days     = split reqFromDate reqToDate
    desc     = "Create absence journal for " ++ show reqLeaveType
  in if reqWorkflowState /= Approved then Nothing else Just $ AbsenceJournal  reqEmpl days desc

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



