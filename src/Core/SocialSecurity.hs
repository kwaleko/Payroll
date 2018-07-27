{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
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

empShare :: (GosiRules    m
            ,EmployeeData m
            ,GosiFormulas m
            --,MonadReader PayrollConfig m
            )
            => EmpNumber
            -> PayrollConfig
            -> m [PayrollRecord]
empShare empNb pconf = do
  --emp <- getEmployee empNb
  --case emp of
  getEmployee empNb >>= \case 
    Nothing -> return []
    Just x -> do
      let age =10 -- empBirthDate emp
      let nat = empNationality x
      --let gtype = EmployeeShare
      let jdate =  empHireDate x
      let ldate = empLastWorkingDate x
      let pfrom = confFromDate pconf
      let pto = confToDate pconf
      rules  <- getRules 
      pcodes <-  eligibleFor
        age nat EmployeeShare (Just jdate) ldate pto 
      recs <- forM pcodes $ \pcode-> do
        amount <- gosiCalc empNb nat pcode
        return $ PayrollRecord pfrom pto pcode empNb amount
      return $ filter predicate recs
      where
        predicate :: PayrollRecord -> Bool
        predicate recrd = recAmount recrd /= 0
        --predicate recrd = if recAmount recrd == 0 then False
        -- else  True


-- To determine the Gosi that an employee is eligible for
-- Based on the formula specified in the setup of GOSI
eligibleFor :: (GosiRules m)
            => Age
            -> Nationality
            -> GosiType
            -> Maybe JoiningDate
            -> LastWorkingDate
            -> PeriodTo
            -> m [Paycode]
eligibleFor age nat gType jdate lstDate pTo  = do
  rules <- getRules
  return $ rPaycode <$> filter checkRules rules
  where
   checkRules :: Rule -> Bool
   checkRules rule
     | rule1 && rule2 && rule3 && rule4 = True
     | True                             = False
     where
       rule1 = rFromAge rule <= age && age <= rToAge rule
       rule2 = rNationality rule == nat
       rule3 = rGosiType rule    == gType
       rule4 = lstDate > pTo

-- What
-- Why 
gosiCalc   :: (EmployeeData m
              ,GosiFormulas m)
           => EmpNumber
           -> Nationality
           -> Paycode -- GOSI Paycode 
           -> m Amount
gosiCalc empNb nat pcode = do
  f   <- getFormuls pcode nat EmployeeShare
  res <- traverse applyF f
  return $ foldl (+) 0 res
  where
    applyF ::(EmployeeData m) => Formula -> m Amount
    applyF formula = do
      let fixPaycode = fPaycode formula
          percent    = fRate formula
      --amnt <- getPycdAmnt empNb fixPaycode
      --case amnt of
      getPycdAmnt empNb fixPaycode >>= \case
        Nothing -> return 0
        Just x -> return $ x * percent `div` 100
