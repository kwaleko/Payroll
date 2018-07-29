{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Core.Leaves where

import Core.Types

import Control.Monad
import Data.Maybe(isJust,fromJust)
import Data.Time (addGregorianMonthsClip
                ,gregorianMonthLength
                ,toGregorian
                ,fromGregorian
                ,Day)


class Monad m => HasLeave m where
  createLeave :: LeaveRequest -> m (Maybe LeaveId)

class Monad m => HasLeaveSetup m where
  getLeaveSetup :: LeaveType -> m (Maybe LeaveSetup)

class Monad m => HasAbsence m where
  createAbsence :: AbsenceJournal -> m ()
  getAbsence    :: PeriodFrom -> PeriodTo -> EmpNumber -> m [AbsenceJournal]

class Monad m => HasFormula m where
  getFormulas :: LeaveType -> m [Formula]

 -- return  [PayrollRecord  (fromGregorian 2019 01 01) (fromGregorian 2019 01 01) (Paycode " " "" Deduction GOSI ) "" 10]
calcDeduction :: (HasLeave m,HasAbsence m,HasFormula m) => Employee -> PayrollConfig -> m [PayrollRecord]
calcDeduction emp PayrollConfig{..} = do
  let empNb = empNumber emp
  abs <- filter (isJust . absPaycode ) <$> getAbsence confFromDate confToDate empNb
  recs <- forM abs $ \absence -> do
    let leavePcode = absPaycode absence
    amount <- calcFormula absence
    return $ PayrollRecord confFromDate confToDate (Leave (fromJust leavePcode )) empNb amount
  return $ filter ((/=0) . recAmount ) recs

calcFormula ::(HasLeave m,HasFormula m) => AbsenceJournal -> m Amount
calcFormula absence = undefined

absGen :: (HasLeaveSetup m,HasAbsence m) => LeaveRequest -> m (Maybe AbsenceJournal)
absGen LeaveRequest{..} = do
  let
    days     = split reqFromDate reqToDate
    desc     = "Create absence journal for " ++ show reqLeaveType
  getLeaveSetup reqLeaveType >>= \case
    Just val -> do
      return $ if reqWorkflowState /= Approved
        then Nothing
        else do
          --lift $ createAbsence $  AbsenceJournal  reqEmpl days (lsPaycode val) desc
          Just $ AbsenceJournal  reqEmpl days (lsPaycode val) desc
         
    Nothing -> return Nothing

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
