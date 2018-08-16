module Bot.Math
    ( roundPrice
    , convert
    , getPrice
    , getId
    ) where

import           BasicPrelude
import           Bot.Types    (Rule (..))

roundPrice :: Double -> Double
roundPrice = fromIntegral . floor

convert :: Rule -> Double -> Double
convert XBt_to_XBT = (* (1 / 100000000))
convert XBT_to_XBt = (*) 100000000
convert USD_to_XBT = (/) 1
convert XBT_to_USD = (*) 1

getPrice :: Integer -> Double
getPrice pricId = ((100000000 * 88) - fromIntegral pricId) * 0.01

getId :: Double -> Integer
getId price = truncate $ (100000000 * 88) - (price / 0.01)
