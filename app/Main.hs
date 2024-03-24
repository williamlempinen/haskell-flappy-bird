module Main (main) where

import UI (loadBgImage, window)
import Pillar (generateInitialPillars)
import Graphics.Gloss (Color, white, play)
import GameState (initialState, renderGame, handleInput, updateGameState)

overFlowDefaultColor :: Color
overFlowDefaultColor = white

main :: IO ()
main = do
    -- use function to load the background image
    bg <- loadBgImage
    -- use function to generate the pillars for the game
    gamePillars <- generateInitialPillars
    -- initialize game
    play window overFlowDefaultColor 60 (initialState gamePillars) (renderGame bg) handleInput updateGameState


