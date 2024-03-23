module Pillar 
    ( Pillar(..)
    , generatePillar
    , generateInitialPillars
    ) where

import System.Random (randomRIO)
import Graphics.Gloss

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
    randomGapPosition <- randomRIO (-300, 300)
    return Pillar { pillarColor = dark green,
                    xAxisPosition = xAxisPos,
                    height = 1000,
                    width = 80,
                    gap = 300,
                    gapPosition = randomGapPosition }


movePillars :: Float -> [Pillar] -> [Pillar]
movePillars move = map movePillarsHelper
    where 
        movePillarsHelper :: Pillar -> Pillar
        movePillarsHelper pillar = pillar { xAxisPosition = xAxisPosition pillar - move }


generateInitialPillars :: IO [Pillar]
generateInitialPillars = sequence [generatePillar (800 + fromIntegral i * 400) | i <- [0..4]]
