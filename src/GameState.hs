module GameState (GameState(..), initialState, renderGame, handleInput, updateGameState) where

import Pillar (Pillar(..), generateInitialPillars, generatePillar, movePillars)
import Graphics.Gloss (Picture(Pictures))
import Graphics.Gloss.Interface.Pure.Game (Event)
import UI (drawGround, drawPillar, drawCeiling, drawLeftWall, drawRightWall)

data GameState = Menu
               | Playing  { pillars :: [Pillar], score :: Int }
               | GameOver { endScore :: Int }
               deriving Show

-- initial state when app is launched
initialState :: [Pillar] -> GameState
initialState gamePillars = Playing { pillars = gamePillars, score = 0 }

-- draw static elements 
renderGame :: Picture -> GameState -> Picture
renderGame bg (Playing pillars _ ) = Pictures (bg 
                                              : drawGround
                                              : drawCeiling
                                              : drawLeftWall
                                              : drawRightWall 
                                              : map drawPillar pillars)


updateGameState :: Float -> GameState -> GameState
updateGameState seconds (Playing pillars score) = Playing (updatePillars seconds pillars) score
    where
        updatePillars :: Float -> [Pillar] -> [Pillar]
        updatePillars seconds pillars = filter (not . isPillarOutOfScreen) (movePillars (seconds * 150) pillars)

        isPillarOutOfScreen :: Pillar -> Bool
        isPillarOutOfScreen pillar = xAxisPosition pillar < -900
-- placeholder
updateGameState _ Menu = Menu
updateGameState _ (GameOver score) = GameOver score


-- user input handler, does nothing
handleInput :: Event -> GameState -> GameState
handleInput _ gameState = gameState
