module Bot.Math
    ( roundPrice
    , convert
    ) where

import           BasicPrelude
import           Bot.Types

roundPrice :: Double -> Double
roundPrice = fromIntegral . floor

convert :: Rule -> Double -> Double
convert XBt_to_XBT = (* (1 / 100000000))
convert XBT_to_XBt = (*) 100000000
convert USD_to_XBT = (/) 1
convert XBT_to_USD = (*) 1
