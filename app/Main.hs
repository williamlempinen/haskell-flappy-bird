module Main (main) where

import UI (loadBgImage, window)
import Pillar (generatePillars)
import Graphics.Gloss (Color, white, play)
import GameState (initialState, renderGame, handleInput, updateGameState, GameState (Playing, pillars))
import Bird (generateBird)
import System.Random (mkStdGen)


overFlowDefaultColor :: Color
overFlowDefaultColor = white
    
fps :: Int
fps = 120

main :: IO ()
main = do
    -- use function to load the background image
    bg          <- loadBgImage
    -- generator, generate pillars 
    let generator = mkStdGen 42
    let (gamePillars, _) = generatePillars 0 1000 generator
    -- initialize game
    play window overFlowDefaultColor fps (initialState gamePillars generateBird) (renderGame bg) handleInput updateGameState


