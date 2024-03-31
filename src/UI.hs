module UI (window
          , loadBgImage
          , drawPillar
          , drawBird
          , drawGameOver
          , drawScore
          , drawMenu
          ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Pillar (Pillar(..))
import Bird (Bird(..))

-- init window
window :: Display
window = InWindow "Flappy Bird" (1800, 1000) (500, 500)

-- scale background image
loadBgImage :: IO Picture
loadBgImage = fmap (scale 2 2) (loadBMP "src/assets/flappy_bird_bg.bmp")

-- draw elements
drawBird :: Bird -> Picture
drawBird (Bird (xPos, yPos) _) = Pictures [translate xPos yPos (color (dark yellow) (circleSolid 40)),
                                           translate xPos (yPos + 15) (color white (circleSolid 10)),
                                           translate (xPos + 10) (yPos + 15) (color white (circleSolid 10)),
                                           translate (xPos + 5) (yPos + 10) (color black (circleSolid 5)),
                                           translate (xPos + 15) (yPos + 10) (color black (circleSolid 5)),
                                           color orange (polygon [(xPos + 5, yPos + 5), (xPos + 30, yPos - 10), (xPos + 5, yPos - 15)])]

drawScore :: Int -> Picture
drawScore score = Pictures [translate 0 (-400) (scale 0.5 0.5 (color black (Text (show score)))),
                            translate 1 (-400) (scale 0.5 0.5 (color black (Text (show score)))),
                            translate (-1) (-400) (scale 0.5 0.5 (color black (Text (show score)))),
                            translate 0 (-401) (scale 0.5 0.5 (color black (Text (show score)))),
                            translate 0 (-399) (scale 0.5 0.5 (color black (Text (show score))))]

drawGameOver :: Picture
drawGameOver = Pictures [translate (-380) 100 (color (dark black) (Text "GAME OVER")), 
                         translate (-381) 101 (color (dark red) (Text "GAME OVER")),
                         translate (-379) 99 (color (dark red) (Text "GAME OVER")),
                         translate (-380) 101 (color (dark red) (Text "GAME OVER")),
                         translate (-380) 99 (color (dark black) (Text "GAME OVER")),
                         translate (-420) (-180) (scale 0.5 0.5(color (dark black) (Text "PRESS ENTER TO CONTINUE")))]

drawMenu :: Picture
drawMenu = Pictures [translate (-400) 100 (color (dark black) (Text "FLAPPY BIRD")), 
                     translate (-401) 101 (color (dark yellow) (Text "FLAPPY BIRD")),
                     translate (-399) 99 (color (dark black) (Text "FLAPPY BIRD")),
                     translate (-400) 101 (color (dark black) (Text "FLAPPY BIRD")),
                     translate (-400) 99 (color (dark yellow) (Text "FLAPPY BIRD")),
                     translate (-370) (-180) (scale 0.5 0.5(color (dark black) (Text "PRESS ENTER TO PLAY"))),
                     translate (-371) (-181) (scale 0.5 0.5(color (dark blue) (Text "PRESS ENTER TO PLAY"))),
                     translate (-180) (-430) (scale 0.3 0.3(color (dark black) (Text "PRESS ESC TO EXIT")))]

drawPillar :: Pillar -> Picture
drawPillar (Pillar pillarColor xAxisPosition height width gap gapPosition) = color pillarColor (Pictures [top, bottom])
  where
    gapHalf = gap / 2
    gapTop = gapPosition + gapHalf
    gapBottom = gapPosition - gapHalf

    -- top pillar
    topHeight = height / 2 - gapTop
    top = translate xAxisPosition (gapTop + topHeight / 2) (rectangleSolid width topHeight)

    -- bottom pillar
    bottomHeight = gapBottom + height / 2
    bottom = translate xAxisPosition (-(height / 2) + bottomHeight / 2) (rectangleSolid width bottomHeight)


