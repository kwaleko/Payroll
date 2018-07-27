module LeaveSpec where

import Test.QuickCheck

import Time.Types
import Core.Leaves

import Core.Types

prop_adjCreation :: LeaveRequest -> [AbsenceJournal] -> Bool
prop_adjCreation req jur = undefined


--spec :: Date -> Date -> prop 
--spec fdate tdate  =  do
--  let lst = fn fdate tdate
--  let (f,t) = head lst
--  let (h,d) = head ( reverse lst)
 -- let cond = f == fdate
 -- let cond2 = d == tdate
 -- ((cond && cond2) === True )

--instance Arbitrary Date where
--  arbitrary= return $ Date 2018 January 15

--main = quickCheck spec 
