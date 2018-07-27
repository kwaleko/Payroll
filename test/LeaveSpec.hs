module LeaveSpec(spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Time 
import Core.Leaves

import Core.Types

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
