module Main (main) where

import UI (loadBgImage, window)
import Pillar (generateInitialPillars)
import Graphics.Gloss (Color, white, play)
import GameState (initialState, renderGame, handleInput, updateGameState)
import Bird (generateInitialBird)


overFlowDefaultColor :: Color
overFlowDefaultColor = white
    
fps :: Int
fps = 120

main :: IO ()
main = do
    -- use function to load the background image
    bg          <- loadBgImage
    -- use function to generate the pillars for the game
    gamePillars <- generateInitialPillars
    -- use function to generate the bird
    gameBird    <- generateInitialBird
    -- initialize game
    play window overFlowDefaultColor fps (initialState gamePillars gameBird) (renderGame bg) handleInput updateGameState


