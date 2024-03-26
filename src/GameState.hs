module GameState (GameState(..), initialState, renderGame, handleInput, updateGameState) where

import Pillar (Pillar(..), generatePillars, generatePillar, movePillars)
import Graphics.Gloss (Picture(Pictures))
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import UI (drawGround, drawPillar, drawCeiling, drawLeftWall, drawRightWall, drawBird, drawGameOver)
import Bird (Bird(..), gravityOnBird, birdCollision)

data GameState = Menu
               | Playing  { pillars :: [Pillar], score :: Int, bird :: Bird }
               | GameOver { endScore :: Int }
               deriving Show

-- initial state when app is launched
initialState :: [Pillar] -> Bird -> GameState
initialState gamePillars bird = Playing { pillars = gamePillars, score = 0, bird = bird }

-- draw static elements 
renderGame :: Picture -> GameState -> Picture
renderGame bg (Playing pillars _ bird ) = Pictures (bg 
                                                   : drawGround
                                                   : drawCeiling
                                                   : drawLeftWall
                                                   : drawRightWall 
                                                   : drawBird bird
                                                   : map drawPillar pillars)
renderGame bg (GameOver score) = Pictures (bg : drawGameOver : [])
renderGame bg Menu = Pictures (bg : drawCeiling : [])


updateGameState :: Float -> GameState -> GameState
updateGameState seconds (Playing pillars score bird) = if birdCollision bird pillars then GameOver score else Playing (updatePillars seconds pillars) score (gravityOnBird seconds bird)
    where
        updatePillars :: Float -> [Pillar] -> [Pillar]
        updatePillars seconds pillars = filter (not . isPillarOutOfScreen) (movePillars (seconds * 150) pillars)

        isPillarOutOfScreen :: Pillar -> Bool
        isPillarOutOfScreen pillar = xAxisPosition pillar <= -900
-- placeholder
updateGameState _ Menu = Menu
updateGameState _ (GameOver score) = GameOver score


-- user input handler, space equals jump
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (Playing pillars score bird) = 
    let jump = 500
        updatedBird = bird { velocity = (fst (velocity bird), jump) }
    in Playing pillars score updatedBird
--handleInput (EventKey (SpecialKey KeyEnter) Down _ _) (GameOver score) = initialState gamePillars bird
-- placeholder
handleInput _ gameState = gameState

