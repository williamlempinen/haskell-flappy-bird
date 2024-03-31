module Main (main) where

import UI (loadBgImage, window)
import Graphics.Gloss (Color, white, play)
import GameState (initialState, renderGame, handleInput, updateGameState)


overFlowDefaultColor :: Color
overFlowDefaultColor = white
    
fps :: Int
fps = 120

main :: IO ()
main = do
    -- load the background image
    bg <- loadBgImage
    -- initialize game
    play window overFlowDefaultColor fps initialState (renderGame bg) handleInput updateGameState


