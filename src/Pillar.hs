module Pillar ( Pillar(..)
              , generatePillar
              , generatePillars
              , movePillars
              ) where

import System.Random (StdGen, randomR)
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
generatePillar :: Float -> StdGen -> (Pillar, StdGen)
generatePillar xAxisPos gen = 
    let (randomGapPosition, newGen) = randomR (-250, 250) gen
    in (Pillar { pillarColor        = dark green,
                 xAxisPosition      = xAxisPos,
                 height             = 1000,
                 width              = 120,
                 gap                = 300,
                 gapPosition        = randomGapPosition }, newGen)

-- generate 1000 pillars
generatePillars :: Int -> Int -> StdGen -> ([Pillar], StdGen)
generatePillars minPillars maxPillars gen
    | minPillars >= maxPillars      = ([], gen)
    | otherwise                     =
        let xAxisPos                = 800 + fromIntegral minPillars * 700
            (newPillar, newGen)     = generatePillar xAxisPos gen
            (restPillars, finalGen) = generatePillars (minPillars + 1) maxPillars newGen
        in  (newPillar : restPillars, finalGen)

-- move every pillar
movePillars :: Float -> [Pillar] -> [Pillar]
movePillars move = map movePillar
    where 
        movePillar :: Pillar -> Pillar
        movePillar pillar = pillar { xAxisPosition = xAxisPosition pillar - move }




