{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Core.Leaves where

import Core.Types
import Core.Utils(dayYear
                 ,fstMDay
                 ,lstMDay)
import Core.CInterfaces
import Control.Lens((%~)
                   ,(^.)
                   ,over)
import Data.Time.Calendar(isLeapYear) --,diffGregorianDurationClip)
import Data.Bool(bool)
import Control.Monad
--import Data.Tuple.Extra
import Data.Maybe(isJust,fromJust,mapMaybe,isNothing )
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
  getCurrentDate     :: m Day

class (Monad m) => HasLeave m where
  createLeave :: LeaveRequest -> m (Maybe LeaveId)

class Monad m => HasLeaveSetup m where
  getLeaveSetup :: LeaveType -> m (Maybe LeaveSetup)

class Monad m => HasAdjustment m where
  getAdjustmentBy :: QueryBy -> LeaveType  -> m [AbsenceAdjJournal]

class Monad m => HasAbsence m where
  createAbsence :: AbsenceJournal -> m ()
  getAbsence    :: PeriodFrom -> PeriodTo -> EmpNumber -> m [AbsenceJournal]
  getAbsenceBy  :: QueryBy -> LeaveType -> m [AbsenceJournal]

data QueryBy = QBEmployee String

class Monad m => HasFormula m where
  getFormulas :: LeaveType -> m [Formula]

calcDeduction :: (HasLeave m,HasAbsence m,HasFormula m) => Employee -> PayrollConfig -> m [PayrollRecord]
calcDeduction emp PayrollConfig{..} = do
  let empNb = empNumber emp
  abs <-  mapMaybe  (\x -> bool Nothing (Just x) (isJust $ x ^. absPaycode )) <$> getAbsence confFromDate confToDate empNb
  recs <- forM abs $ \absence -> do
    let leavePcode =  absence ^. absPaycode
    amount <- calcFormula absence
    return $ PayrollRecord confFromDate confToDate (Leave (fromJust leavePcode )) empNb amount
  return $ filter ((/=0) . recAmount ) recs

leaveDaysRem :: (HasAbsence m,HasAdjustment m,HasLeaveSetup m,HasEmployment m) => Employee -> LeaveType -> ToDate -> m (Maybe Int)
leaveDaysRem Employee{..} ltype tillDate = do
  let yto   = dayYear tillDate
  employment <- getEmployment empNumber
  abs        <- getAbsenceBy    (QBEmployee empNumber) ltype
  adj        <- getAdjustmentBy (QBEmployee empNumber) ltype
  lvSetup    <- getLeaveSetup ltype
  return $ do
    lvSetup'    <-  lvSetup
    employment' <- employment
    let yfrom        = dayYear . utctDay $ employmentValidFrom employment'
        maxBalance   = lsMaxBalance lvSetup'
        yearVacation = lsYearlyVacation lvSetup'
    Just $ min maxBalance $ sum $ yrBalance yearVacation abs adj <$> [yfrom..yto]

yrBalance :: Int -> [AbsenceJournal] -> [AbsenceAdjJournal] -> Year -> Int
yrBalance oBalance abs adj year =
    oBalance
  - (sum $ absBalance year <$> abs )
  + (sum $ map  adjDays $ filter (\x -> (dayYear . adjTransDate) x == year) adj)

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
