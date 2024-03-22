module Main (main) where

import UI
import Pillar
import Graphics.Gloss 

overFlowDefaultColor :: Color
overFlowDefaultColor = white

main :: IO ()
main = do
    bg <- loadBgImage
    pillar <- generatePillar

    let scene = Pictures [bg, drawGround, drawPillar pillar]
    
    display window overFlowDefaultColor scene
