module Main (main) where

import UI (window, loadBgImage, drawing)
import Graphics.Gloss 

main :: IO ()
main = do
    bg <- loadBgImage
    let scene = Pictures [bg, drawing]
    display window white scene
