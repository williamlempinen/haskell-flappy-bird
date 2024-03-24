module Pillar 
    ( Pillar(..)
    , generatePillar
    , generateInitialPillars
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
    return Pillar { pillarColor = dark green,
                    xAxisPosition = xAxisPos,
                    height = 1000,
                    width = 80,
                    gap = 300,
                    gapPosition = randomGapPosition }

-- move every pillar
movePillars :: Float -> [Pillar] -> [Pillar]
movePillars move = map movePillar
    where 
        movePillar :: Pillar -> Pillar
        movePillar pillar = pillar { xAxisPosition = xAxisPosition pillar - move }


-- generate 1000 pillars
generateInitialPillars :: IO [Pillar]
generateInitialPillars = generateNewPillars 0 1000

generateNewPillars :: Int -> Int -> IO [Pillar]
generateNewPillars count maxPillars
    | count >= maxPillars = return []
    | otherwise           = do
        let xAxisPos = 800 + fromIntegral count * 700
        newPillar <- generatePillar xAxisPos
        restPillars <- generateNewPillars (count + 1) maxPillars
        return (newPillar : restPillars)