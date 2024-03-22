module UI where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Pillar (Pillar(..), generatePillar)

--init window
window :: Display
window = InWindow "Flappy Bird" (1800, 1000) (500, 500)

-- scale background image
loadBgImage :: IO Picture
loadBgImage = fmap (scale 2 2) (loadBMP "src/assets/flappy_bird_bg.bmp")

drawGround :: Picture
drawGround = translate 0 (-500) (rectangleSolid 1800 10)

-- function to draw a pillar
drawPillar :: Pillar -> Picture
drawPillar (Pillar pillarColor height width gap gapPosition) = color pillarColor (Pictures [top, bottom])
  where
    gapHalf = gap / 2
    gapTop = gapPosition + gapHalf
    gapBottom = gapPosition - gapHalf

    -- top pillar
    topHeight = height / 2 - gapTop
    top = translate 0 (gapTop + topHeight / 2) (rectangleSolid width topHeight)

    -- bottom pillar
    bottomHeight = gapBottom + height / 2
    bottom = translate 0 (-height / 2 + bottomHeight / 2) (rectangleSolid width bottomHeight)