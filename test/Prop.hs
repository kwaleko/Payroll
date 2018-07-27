{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Prop where

import Test.QuickCheck
import qualified  Data.Map as M
import Control.Monad.State 

import Core.SocialSecurity
import Core.Types 

data FakeDb = FakeDb
  {employeeData :: M.Map EmpNumber Employee
  ,paycodeAmount :: M.Map (EmpNumber,Paycode) Amount
  ,formula :: M.Map (Paycode,Nationality,GosiType) [Formula]
  }deriving(Show)


instance EmployeeData (State FakeDb) where
  getEmployee empNb = M.lookup empNb . employeeData <$> get
  getPycdAmnt empNb pcode = M.lookup (empNb,pcode) . paycodeAmount <$> get 

instance GosiFormulas (State FakeDb) where
  getFormuls pcode nat gtype = do
    val <- M.lookup (pcode,nat,gtype) . formula <$> get
    case val of
      Nothing -> return []
      Just lst -> return lst

prop_reverse :: [Int] -> Bool
prop_reverse xs =reverse (reverse xs) == xs

--prop_eligibleFor ::
--             Age
  --          -> Nationality
 --           -> GosiType
  --          -> Maybe JoiningDate
  --          -> LastWorkingDate
   --         -> PeriodTo
   --         ->  [Paycode]
   --         -> Bool
--prop_eligibleFor :: age nat gType jDate lstDate pTo t =
--  eligibleFor  age nat gType jDate lstDate pTo == []
