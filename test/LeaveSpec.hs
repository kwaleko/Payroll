{-# LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE FlexibleInstances #-}
module LeaveSpec(spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH

import           Data.Time
import           Test.Hspec
import           Test.QuickCheck


import           Core.Leaves
import           Core.Types


mkFixture "Fixture" [ts| HasFormula,HasLeave,HasAbsence |]

spec :: Spec
spec = do
  describe "Test split function " $
   it "should return a non empty list when the from & to date are in a different month" $ do
    let d1 = ModifiedJulianDay 15000
    let d2 = ModifiedJulianDay 18000
    split d1 d2 `shouldNotBe` []

  describe "test split function for date in the same month" $
   it "should return a singelton list when the both date are in the same month" $ do
    let d1 = fromGregorian 2018 01 28
    let d2 = fromGregorian 2018 01 31
    split d1 d2 `shouldBe` [(d1,d2)]

  describe "test fstMDay for January" $
   it "should return (2018 01 01)" $ do
    fstMDay (fromGregorian 2018 01 28) `shouldBe` fromGregorian 2018 01 01

  describe "test lstMDay for February" $
   it "should return the end of month " $ do
    lstMDay (fromGregorian 2018 02 02) `shouldBe` fromGregorian 2018 02 28

  describe "absCreation should create an absence journal based on a leave request" $
   it "should fail if the leave request is not approved" $ property $
    \leave -> case reqWorkflowState leave of
      Pending  -> absGen leave  == Nothing
      Rejected -> absGen leave  == Nothing
      _        -> absGen leave == absGen leave

  describe "calcDeduction : calcule the amount that should be deducted in the payrun process" $
   it "should return an empty list if the employee does not have any absence journal in the month of payrun" $ property $ do
    let fixture = def {
          _getAbsence = \_ _ _ -> return []
                  }
    arb <- mkaCalcDeduction 
    pure (unTestFixture arb fixture === [])

mkaCalcDeduction :: (HasAbsence m) => Gen (m [PayrollRecord])
mkaCalcDeduction = calcDeduction
  <$> (arbitrary :: Gen Employee)
  <*> (arbitrary :: Gen PayrollConfig)


