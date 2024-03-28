module Pillar 
    ( Pillar(..)
    , generatePillar
    , generatePillars
    , movePillars
    ) where

import System.Random (randomRIO)
import Graphics.Gloss (Color, dark, green)

data Pillar = Pillar { 
    pillarColor   :: Color,
    xAxisPosition :: Float,
    height        :: Float,
    width         :: Float,
    gap           :: Float, 
    gapPosition   :: Float
} deriving (Eq, Show)

-- generate Pillar with a random gap position
generatePillar :: Float -> IO Pillar
generatePillar xAxisPos = do
    -- random position for the gap's center point
    randomGapPosition <- randomRIO (-250, 250)
    return Pillar { pillarColor   = dark green,
                    xAxisPosition = xAxisPos,
                    height        = 1000,
                    width         = 80,
                    gap           = 300,
                    gapPosition   = randomGapPosition }

-- move every pillar
movePillars :: Float -> [Pillar] -> [Pillar]
movePillars move = map movePillar
    where 
        movePillar :: Pillar -> Pillar
        movePillar pillar = pillar { xAxisPosition = xAxisPosition pillar - move }


-- generate 1000 pillars
generatePillars :: Int -> Int -> IO [Pillar]
generatePillars min max
    | min >= max = return []
    | otherwise  = do
        let xAxisPos = 800 + fromIntegral min * 700
        newPillar <- generatePillar xAxisPos
        restPillars <- generatePillars (min + 1) max
        return (newPillar : restPillars)



