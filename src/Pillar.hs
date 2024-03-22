module Pillar 
    ( Pillar(..)
    , generatePillar
    ) where

import System.Random (randomRIO)
import Graphics.Gloss

data Pillar = Pillar { 
    pillarColor :: Color,
    height      :: Float,
    width       :: Float,
    gap         :: Float, 
    gapPosition :: Float
} deriving (Eq)

-- generate Pillar with a random gap position
generatePillar :: IO Pillar
generatePillar = do
    randomGapPosition <- randomRIO (-300, 300)
    return Pillar { pillarColor = green, height = 1000, width = 80, gap = 300, gapPosition = randomGapPosition }
