{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Core.SocialSecurity where

import qualified Data.Map as M

import           Core.Types
import           Control.Monad.Reader
import           Control.Monad.State 
import           Data.Either.Extra(maybeToEither)
import           Time.Types



class Monad m => GosiRules m where
  getRules :: m [Rule]

class Monad m => GosiFormulas m where
  getFormuls :: Paycode -> Nationality -> GosiType -> m  [Formula]

class Monad m => EmployeeData m where
  getEmployee   :: EmpNumber -> m (Maybe Employee)
  getPycdAmnt   :: EmpNumber -> Paycode -> m (Maybe Amount)

empShare :: (GosiRules m ,EmployeeData m,GosiFormulas m) => Employee -> PayrollConfig -> m [PayrollRecord]
empShare Employee{..} pconf = do
      let empNb = empNumber 
      let age   = 10 -- empBirthDate emp
      let nat   = empNationality 
      let ldate = empLastWorkingDate 
      let pfrom = confFromDate pconf
      let pto   = confToDate pconf
      rules  <- getRules
      pcodes <-  eligibleFor Employee{..} EmployeeShare pto
      recs <- forM pcodes $ \pcode-> do
        amount <- gosiCalc Employee{..} pcode
        return $ PayrollRecord pfrom pto pcode empNb amount
      return $ filter ((/= 0) . recAmount) recs


-- To determine the Gosi that an employee is eligible for
-- Based on the formula specified in the setup of GOSI
--eligibleFor :: (GosiRules m) => Age -> Nationality -> GosiType -> LastWorkingDate -> PeriodTo -> m [Paycode]
eligibleFor :: (GosiRules m) => Employee -> GosiType -> PeriodTo -> m [Paycode]
eligibleFor Employee{..} gType pTo  = do
  let age     = 10
  let nat     = empNationality
  let lstDate = empLastWorkingDate
  map rPaycode . filter (checkRules age nat lstDate) <$> getRules
  where
   checkRules :: Age -> Nationality -> LastWorkingDate -> Rule -> Bool
   checkRules  age nat lstDate rule
     | rule1 && rule2 && rule3 && rule4 = True
     | True                             = False
     where
       rule1 = rFromAge rule <= age && age <= rToAge rule
       rule2 = rNationality rule == nat
       rule3 = rGosiType rule    == gType
       rule4 = lstDate > pTo

-- Why
gosiCalc   :: (EmployeeData m,GosiFormulas m) => Employee -> Paycode -> m Amount
gosiCalc Employee{..} pcode = do
  let empNb = empNumber 
      nat   = empNationality
  f   <- getFormuls pcode nat EmployeeShare
  res <- traverse (applyF empNb) f
  return $ foldl (+) 0 res
  where
    applyF ::(EmployeeData m) => EmpNumber -> Formula -> m Amount
    applyF empNb formula = do
      let fixPaycode = fPaycode formula
          percent    = fRate formula
      maybe 0 (\x -> x * percent `div` 100) <$> getPycdAmnt empNb fixPaycode

