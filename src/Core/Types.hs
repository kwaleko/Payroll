{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Core.Types where

import Data.Map
import Time.Types
import GHC.Generics
import Test.QuickCheck

type EmpNumber       = String
type Age             = Int
type Percentage      = Int
type Amount          = Int
type FromDate        = Date
type ToDate          = Date
type PayGroup        = String
type PayrunNumber    = String
type JoiningDate     = Date
type LastWorkingDate = Date
type PeriodFrom      = Date
type PeriodTo        = Date

--data Paycode = FPaycode | GosiPaycode

data Employee = Employee
  {empNumber          :: EmpNumber
  ,empName            :: String
  ,empNationality     :: Nationality
  ,empPayGroup        :: String
  ,empHireDate        :: Date
  ,empBirthDate       :: Date
  ,empLastWorkingDate :: Date
  ,empStatus          :: EmploymentStatus
  ,empExcludeFromGosi :: Bool
  }deriving (Show,Eq,Ord)

data Employment = Employment
  {employmentEmpNumber :: EmpNumber
  ,employmentValidFrom :: DateTime
  ,employmentValidTo   :: DateTime
  }deriving(Show,Eq,Ord)

data PayrollConfig = PayrollConfig
  {confFromDate  :: FromDate
  ,confToDate    :: ToDate
  ,confPayrunNum :: PayrunNumber
  ,confEmployees :: EmpNumber 
  ,congPayGroup  :: PayGroup 
  }deriving(Show,Eq,Ord)

------------------ G O S I ------------------

data Nationality = None
                 | Others
                 deriving (Show,Eq,Ord)

data EmploymentStatus = Activated
                      | Deactivated
                      deriving (Show,Eq,Ord)

data PaycodeType = Deduction
                 | Earning
                 deriving(Show,Eq,Ord)

data GosiType = EmployeeShare
              | EmployerShare
              deriving (Show,Eq,Ord)

data PaycodeGroup = GOSI
                  | EOS
                  | Vacation
                  | FixAllowance
                  deriving(Show,Eq,Ord)

data Paycode = Paycode
  {fPaycodeId    :: String
  ,fPaycodeName  :: String
  ,fPaycodeType  :: PaycodeType
  ,fPaycodeGroup :: PaycodeGroup
  }deriving (Show,Eq,Ord)

--data GosiPaycode = GosiPaycode
--  {gPaycodeId   :: String
--  ,gPaycodeName :: String
--  }deriving (Show,Eq,Ord)


data Rule = Rule
  {rFromAge     :: Age
  ,rToAge       :: Age
  ,rNationality :: Nationality
  ,rPaycode     :: Paycode
  ,rGosiType    :: GosiType
  }deriving (Show,Eq,Ord)

data EmpPaycode = EmpPaycode
  {empPayEmployee      :: EmpNumber
  ,empPayPaycode       :: Paycode
  ,empPayEffectiveDate :: Date
  ,empPayAmount        :: Amount
  }deriving (Show,Eq,Ord)


data Formula = Formula
  {fPaycode        :: Paycode
  ,fRate          :: Percentage
  --,fMaxPay        :: Maybe Amount
  --,fDateEffective :: Maybe Date
  }deriving(Show,Eq,Ord)

data PayrollRecord = PayrollRecord
  {recFromDate :: FromDate
  ,recToDate   :: ToDate
  ,recPaycode  :: Paycode
  ,recEmployee :: EmpNumber
  ,recAmount   :: Amount
  }deriving(Show,Eq,Ord)

--------------- L E A V E S ------------------
data LeaveSetup = LeaveSetup
  {lsAccrued        :: Bool
  ,lsYearlyVacation :: Int
  ,lsMaxBalance     :: Int
  ,lsMaxYears       :: Int
  ,lsOneTime        :: Bool
  }deriving(Show,Eq,Ord)

data PayrollJournal = PayrollJournal
  {pjEmployee     :: EmpNumber
  ,pjTransDate    :: Date
  ,pjPayrunNumber :: PayrunNumber
  ,pjPaycode      :: Paycode
  ,pjAmount       :: Amount
  ,pjPosted       :: Bool
  }deriving(Show,Eq,Ord)

data LeaveType = Yearly
               | Sick
               | Unpaid
               | Marriage
               | Study
               | Maternity
               deriving(Show,Eq,Ord)

data LeaveRequest = LeaveRequest
  {reqNumber        :: String
  ,reqTransDate     :: Date
  ,reqEmpl          :: EmpNumber
  ,reqFromDate      :: Date
  ,reqToDate        :: Date
  ,reqLeaveType     :: LeaveType
  ,reqWorkflowState :: WorkflowState
  }deriving(Eq,Show,Ord)

data WorkflowState = Approved | Rejected | Pending
                   deriving(Show,Eq,Ord)

type LeaveId = Int 
data AbsenceJournal = AbsenceJournal
  {absNumber      :: String
  ,absEmpl        :: EmpNumber
  ,absFromDate    :: Date
  ,absToDate      :: Date
  ,absNbDays      :: Int
  ,absDescription :: String
  }deriving(Show,Eq,Ord)
