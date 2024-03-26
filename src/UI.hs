module UI where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap (loadBMP)
import Pillar (Pillar(..), generatePillar)
import Bird (Bird(..))

--init window
window :: Display
window = InWindow "Flappy Bird" (1800, 1000) (500, 500)

-- scale background image
loadBgImage :: IO Picture
loadBgImage = fmap (scale 2 2) (loadBMP "src/assets/flappy_bird_bg.bmp")

--draw static elements
drawGround :: Picture
drawGround = translate 0 (-500) (rectangleSolid 1800 10)

drawCeiling :: Picture
drawCeiling = translate 0 500 (rectangleSolid 1800 10)

drawRightWall :: Picture
drawRightWall = rotate 90 (translate 0 900 (rectangleSolid 1000 10))

drawLeftWall :: Picture
drawLeftWall = rotate 90 (translate 0 (-900) (rectangleSolid 1000 10))

drawBird :: Bird -> Picture
drawBird (Bird (xPos, yPos) _) = translate xPos yPos (circleSolid 40)

drawGameOver :: Picture
drawGameOver = translate 0 0 (circleSolid 60)


-- function to draw a pillar
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


