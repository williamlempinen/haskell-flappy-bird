module UI where

import Graphics.Gloss 
import Graphics.Gloss.Data.Bitmap (loadBMP)

window :: Display
window = InWindow "Flappy Bird" (1000, 800) (500, 500)

loadBgImage :: IO Picture
loadBgImage = loadBMP "src/assets/flappy_bird_bg.bmp"

drawing :: Picture
drawing = rectangleSolid 100 200

