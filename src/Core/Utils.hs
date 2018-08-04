module Core.Utils where

import Data.Time
import Data.Tuple.Extra

dayYear :: Day -> Int
dayYear = fromIntegral . fst3 . toGregorian

setDay :: Int -> (Int -> Int) -> Day -> Day
setDay nb fn day = let
  (year,month,_) = toGregorian day
  in fromGregorian year month $ fn nb

fstMDay :: Day -> Day
fstMDay day  = setDay 01 id day

lstMDay :: Day -> Day
lstMDay day =
  let (year,month,_) = toGregorian day
  in fromGregorian year month $ gregorianMonthLength year month
