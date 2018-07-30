{-# LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE FlexibleInstances #-}
module LeaveSpec(spec) where

import           Control.Monad.TestFixture
import           Control.Monad.TestFixture.TH


import           Data.Tuple.Extra
import           Data.Time
import           Test.Hspec
import           Test.QuickCheck


import           Core.Leaves
import           Core.Types


mkFixture "Fixture" [ts| HasFormula,HasLeave,HasAbsence,HasLeaveSetup |]
fn = True 
spec :: Spec
spec = do
  describe "split" $ do
   it "all start date except the first one should be day 1 of their month" $ property $
     \date1 date2 -> let
       f = (==1) . thd3 . toGregorian . fst
       lst = tail $ split date1 date2
       in (==False) `any` (f <$> lst) `shouldBe` False

   it "should return a non empty list when the from & to date are in a different month" $ do
    let d1 = ModifiedJulianDay 15000
    let d2 = ModifiedJulianDay 18000
    split d1 d2 `shouldNotBe` []

   it "should return a singelton list when the both date are in the same month" $ do
    let d1 = fromGregorian 2018 01 28
    let d2 = fromGregorian 2018 01 31
    split d1 d2 `shouldBe` [(d1,d2)]

  describe "fstMDay" $
   it "should return (2018 01 01)" $ do
    fstMDay (fromGregorian 2018 01 28) `shouldBe` fromGregorian 2018 01 01

  describe "lstMDay" $
   it "should return the end of month " $ do
    lstMDay (fromGregorian 2018 02 02) `shouldBe` fromGregorian 2018 02 28

  {-describe "absCreation should create an absence journal based on a leave request" $
   it "should fail if the leave request is not approved" $ property $
    let fixture = def {
          _getLeaveSetup = \_ -> return Nothing
                      }
    \leave -> case reqWorkflowState leave of
      Pending  -> absGen leave  == Nothing
      Rejected -> absGen leave  == Nothing
      _        -> absGen leave == absGen leave -}

  describe "calcDeduction" $
   it "should return an empty list if the employee does not have any absence journal in the month of payrun" $ property $ do
    let fixture = def {
          _getAbsence = \_ _ _ -> return []
                  }
    arb <- mkaCalcDeduction 
    pure (unTestFixture arb fixture === [])

  describe "LeaveDaysRem" $
   it "the calculated balance of a given leave should not exceed the maximum balance in the leave setup" $
    property $ \x y z d -> ((leaveDaysRem x y z d) <= (lsMaxBalance d)) == True
    where dn =1



mkaCalcDeduction :: (HasAbsence m,HasLeave m,HasFormula m) => Gen (m [PayrollRecord])
mkaCalcDeduction = calcDeduction
  <$> (arbitrary :: Gen Employee)
  <*> (arbitrary :: Gen PayrollConfig)


