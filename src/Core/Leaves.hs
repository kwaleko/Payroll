{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Core.Leaves where

import Core.Types
import Core.Utils(dayYear
                 ,fstMDay
                 ,lstMDay)
import Control.Lens((%~)
                   ,(^.)
                   ,over)
import Data.Time.Calendar(isLeapYear) --,diffGregorianDurationClip)
import Data.Bool(bool)
import Control.Monad
--import Data.Tuple.Extra
import Data.Maybe(isJust,fromJust,mapMaybe )
import Data.Time (addGregorianMonthsClip
                ,gregorianMonthLength
                ,toGregorian
                ,fromGregorian
                ,diffDays
                ,UTCTime(..)
                ,Day)
import Data.Function((&))

class Monad m => MonadTime m where
  getCurrentDateTime :: m UTCTime
  getCurrentDate    :: m Day

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
  abs <-  mapMaybe  (\x -> bool Nothing (Just x) (isJust $ x ^. absPaycode )) <$> getAbsence confFromDate confToDate empNb
  recs <- forM abs $ \absence -> do
    let leavePcode =  absence ^. absPaycode
    amount <- calcFormula absence
    return $ PayrollRecord confFromDate confToDate (Leave (fromJust leavePcode )) empNb amount
  return $ filter ((/=0) . recAmount ) recs

leaveDaysRem :: Day -> Employment -> [AbsenceJournal] -> [AbsenceAdjJournal] -> LeaveSetup  -> Int
leaveDaysRem tillDate Employment{..} abs adj LeaveSetup{..} =
  -- foldl plus 0 $ (\year -> ( sum ( adjDays <$> adj )) + lsYearlyVacation -  (sum $ (absBalance year) <$> abs ) )  <$> years
  --min lsMaxBalance $ sum $ map  (\year -> ( sum ( adjDays <$> adj )) + lsYearlyVacation -  (sum $ (absBalance year) <$> abs ) )  years
  min lsMaxBalance $ sum $ yrBalance lsYearlyVacation abs adj <$> years
  where
    years = [yfrom..yto]
    yfrom = dayYear . utctDay $ employmentValidFrom
    yto = dayYear tillDate

yrBalance :: Int -> [AbsenceJournal] -> [AbsenceAdjJournal] -> Year -> Int
yrBalance oBalance abs adj year =
    oBalance
  - (sum $ absBalance year <$> abs )
  + (sum $ adjDays <$> adj)

absBalance :: Year -> AbsenceJournal -> Int
absBalance year abs =
  abs ^. absLines
  & filter (\(_,b) -> dayYear b == year)
  & map    (\(a,b) -> (+1) . fromIntegral $ diffDays b a)
  & sum

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
