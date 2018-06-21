module Bot.Math
    ( roundPrice
    ) where

import           BasicPrelude

roundPrice :: Double -> Double
roundPrice = fromIntegral . floor
