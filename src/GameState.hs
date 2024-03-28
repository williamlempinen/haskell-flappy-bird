module GameState (GameState(..), initialState, renderGame, handleInput, updateGameState) where

import Pillar (Pillar(..), movePillars)
import Graphics.Gloss (Picture(Pictures))
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import UI (drawGround, drawPillar, drawCeiling, drawLeftWall, drawRightWall, drawBird, drawGameOver, drawScore)
import Bird (Bird(..), gravityOnBird, birdCollision, generateBird)

data GameState = Menu
               | Playing  { pillars :: [Pillar], score :: Int, bird :: Bird }
               | GameOver { endScore :: Int }
               | Restart
               deriving Show

-- initial state when app is launched
initialState :: [Pillar] -> Bird -> GameState
initialState gamePillars bird = Playing { pillars = gamePillars, score = 0, bird = bird }

-- draw static elements 
renderGame :: Picture -> GameState -> Picture
renderGame bg (Playing pillars score bird ) = Pictures (bg 
                                                       : drawGround
                                                       : drawCeiling
                                                       : drawLeftWall
                                                       : drawRightWall 
                                                       : drawBird bird
                                                       : drawScore score
                                                       : map drawPillar pillars)
renderGame bg Restart          = Pictures [bg, drawGameOver]
renderGame bg (GameOver score) = Pictures [bg, drawGameOver, drawScore score]
renderGame bg Menu             = Pictures [bg, drawCeiling]

updateGameState :: Float -> GameState -> GameState
updateGameState _ Restart = Restart
updateGameState seconds (Playing pillars score bird) = if birdCollision bird pillars then GameOver score
        else Playing (updatePillars seconds pillars) (updateScore score) (gravityOnBird seconds bird)
    where
        updatePillars :: Float -> [Pillar] -> [Pillar]
        updatePillars seconds pillars = filter (not . isPillarOutOfScreen) (movePillars (seconds * 150) pillars)

        isPillarOutOfScreen :: Pillar -> Bool
        isPillarOutOfScreen pillar = xAxisPosition pillar <= -900

        updateScore :: Int -> Int
        updateScore prev = prev + 1
-- placeholder
updateGameState _ Menu = Menu
updateGameState _ (GameOver score)  = GameOver score


-- user input handler, space equals jump
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (Playing pillars score bird) = 
    let jump = 500
        updatedBird = bird { velocity = (fst (velocity bird), jump) }
    in Playing pillars score updatedBird
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) (GameOver _) = Restart
handleInput _ gameState = gameState


