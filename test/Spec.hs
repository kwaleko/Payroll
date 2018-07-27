{-# LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE DeriveGeneric #-}
import Control.Monad.TestFixture
import Control.Monad.TestFixture.TH
import  Test.Hspec
import Time.Types
import Test.QuickCheck

import Core.Types
import Core.SocialSecurity


mkFixture "Fixture" [ts| GosiRules,EmployeeData,GosiFormulas |]


pcode' = Paycode  "Basic"  "basic salary" Deduction GOSI
formula1' = Formula pcode' 5
formula2' =  Formula pcode' 10
nat' = Saudi

emp1 = Employee
 {empNumber          = "081"
 ,empName            = "TEST NAME"
 ,empNationality     = Others
 ,empPayGroup        = "ss"
 ,empHireDate        = Date 2017 January 01
 ,empBirthDate       =  Date 2000 February 01
 ,empLastWorkingDate =  Date 2018 April 01
 ,empStatus          = Activated
 ,empExcludeFromGosi = False
 }

payrllConf = PayrollConfig (Date 2000 January 12) (Date 2000 January 12) "" "001" ""

spec :: Spec
spec = do
  describe "test eligibleFor: retrieve the paycode for a given tule" $
   it "should return empty list when the nationality does not match" $ do

     let age = 35
     let nat = Others
     let gtype = EmployeeShare
     let jdate = Nothing
     let ldate = Date 2040 February 01
     let pto = Date 2000 February 01
     let saned = Paycode "saned" "saned" Deduction GOSI
     let fixture = def {
          _getRules = return [Rule 20 61 Others saned EmployeeShare]
                      }

     unTestFixture (eligibleFor age nat gtype jdate ldate pto) fixture `shouldBe` []
  
  describe "test eligibleFor: retrieve paycods based on a give rules" $
   it "should return empty list when the period to is greater than the last working date" $ do
    let age = 35
    let nat = Others
    let gtype = EmployeeShare
    let jdate = Nothing
    let ldate = Date 2040 February 01
    let pto = Date 2050 February 01
    let saned = Paycode "saned" "saned" Deduction GOSI
    let fixture = def {
          _getRules = return [Rule 20 61 Others saned EmployeeShare]
                      }

    unTestFixture (eligibleFor age nat gtype jdate ldate pto) fixture `shouldBe` []

    
  describe "test eligibleFor: retrieve the paycodes based on a given rules" $
    it "should return empty list when GOSI type does not match" $ do
    let age = 35
    let nat = Others
    let gtype = EmployerShare
    let jdate = Nothing
    let ldate = Date 2040 February 01
    let pto = Date 2000 February 01
    let saned = Paycode "saned" "saned" Deduction GOSI
    let fixture = def {
          _getRules = return [Rule 20 61 Others saned EmployeeShare]
                      }

    unTestFixture (eligibleFor age nat gtype jdate ldate pto) fixture `shouldBe` []


  describe "test eligibleFor: retrieve the paycode for a given tule" $
   it "should return empty list when the nationality does not match" $ do

    let age = 35
    let nat = Others
    let gtype = EmployeeShare
    let jdate = Nothing
    let ldate = Date 2040 February 01
    let pto = Date 2000 February 01
    let saned = Paycode "saned" "saned" Deduction GOSI
    let fixture = def {
          _getRules = return [Rule 20 61 Others saned EmployeeShare]
                      }

    unTestFixture (eligibleFor age nat gtype jdate ldate pto) fixture `shouldBe` []

  describe "test eligibleFor: retrieve the paycode for a given rules" $
   it "should return empty list when the age is not in range" $ do
    let age = 16
    let nat = Others
    let gtype = EmployeeShare
    let jdate = Nothing
    let ldate = Date 2040 February 01
    let pto = Date 2000 February 01
    let saned = Paycode "saned" "saned" Deduction GOSI
    let fixture = def {
          _getRules = return [Rule 20 61 Others saned EmployeeShare]
                      }

    unTestFixture (eligibleFor age nat gtype jdate ldate pto) fixture `shouldBe` []


  describe "calculate the Gosi amount for a given paycode" $
   it "should return zero when no formula is provided" $ do
    let saned = Paycode "saned" "saned" Deduction GOSI
    let fixture = def{
          _getPycdAmnt = \_ _ -> return $ Just 10000
          ,_getFormuls = \_ _ _ -> return []
                     }
    unTestFixture (gosiCalc "001" Others saned) fixture `shouldBe` 0


  describe "calculate the GOSI amount for a given paycode" $
   it "should calculate GOSI amount based on the formula " $ do

    let basic = Paycode "basic" "Basic salary" Earning GOSI
    let saned = Paycode "saned" "Saned" Deduction GOSI
    let housing = Paycode "housing" "housing" Earning FixAllowance
    let trans = Paycode "trans" "Transport" Earning FixAllowance
    let fixture = def{
          _getPycdAmnt = \emp pcode -> case fPaycodeId pcode of
              "basic" ->  return $ Just 15000
              "trans" -> return $ Just 2000
              "housing" -> return $ Just 1000
          ,_getFormuls = \pcode nat gtype -> case pcode of
              saned -> return [formula1,formula2]
                where
                  formula1 = Formula basic 10
                  formula2 = Formula trans 5

                     }

    unTestFixture (gosiCalc "001" Others saned) fixture `shouldBe` 1600


  describe "calculate the gosi amount based on the rules" $
   it "return an amount that is less the the orginal paycode amount" $ do
    let pcode = Paycode "Basic" "Basic Salary" Deduction GOSI
    let formula = Formula pcode 10
    let nat = Others

    let fixture = def { _getPycdAmnt = \_ _ -> return $ Just 1000
                       ,_getFormuls = \ _ _ _ ->return  [formula]}

    unTestFixture ( (\x -> x < 1000)  <$> gosiCalc "001" nat pcode  ) fixture `shouldBe`  True


  describe "generate a list of payroll record for the GOSI for a given employee" $
   it "should not return a record with amount equal zero" $ do
    let fixture = def { _getPycdAmnt = \_ _ -> return $ Just 0
                       ,_getFormuls = \ _ _ _ ->return  [formula1',formula2']
                       ,_getEmployee = \_ -> return $ Just emp1
                       ,_getRules = return []
                       }

    unTestFixture ( filteredRecs <$> empShare "001" payrllConf)  fixture `shouldBe` []
    where
      filteredRecs recs = filter (\rec -> if recAmount rec == 0 then True else False ) recs




--main :: IO ()
--main = hspec spec
main = quickCheck (withMaxSuccess 100000 prop)  

prop = do
  let saned = Paycode "saned" "saned" Deduction GOSI
  let fixture = def {
          _getRules = return [Rule 20 61 Others saned EmployeeShare]
                  }
  ablah <- mkArbBlah
  pure ((unTestFixture ablah fixture )=== [])

instance Arbitrary Date where
  arbitrary= return $ Date 2018 January 15

instance Arbitrary GosiType where
  arbitrary = return EmployeeShare

instance Arbitrary Nationality where
  arbitrary = return Others

mkArbBlah :: (GosiRules m) => Gen (m [Paycode])
mkArbBlah = eligibleFor
  <$>  (arbitrary :: Gen Age)
  <*>  (arbitrary :: Gen Nationality)
  <*>  (arbitrary :: Gen GosiType)
  <*>  (arbitrary :: Gen (Maybe Date ))
  <*>  (arbitrary :: Gen Date )
  <*>  (arbitrary :: Gen PeriodTo)
