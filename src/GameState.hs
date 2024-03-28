module GameState (GameState(..), initialState, renderGame, handleInput, updateGameState) where

import Pillar (Pillar(..), movePillars, generatePillars)
import Graphics.Gloss (Picture(Pictures))
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import UI (drawGround, drawPillar, drawCeiling, drawBird, drawGameOver, drawScore, drawMenu)
import Bird (Bird(..), gravityOnBird, birdCollision, generateBird)
import System.Random (mkStdGen)

data GameState = Menu
               | Playing  { pillars :: [Pillar], score :: Int, bird :: Bird }
               | GameOver { endScore :: Int }
               deriving Show

-- initial state when app is launched
initialState :: GameState
initialState = Menu

-- draw static elements 
renderGame :: Picture -> GameState -> Picture
renderGame bg (Playing pillars score bird ) = Pictures (bg 
                                                       : drawGround
                                                       : drawCeiling
                                                       : drawBird bird
                                                       : drawScore score
                                                       : map drawPillar pillars)
renderGame bg (GameOver score)              = Pictures [bg, drawGameOver, drawScore score]
renderGame bg Menu                          = Pictures [bg, drawMenu]

updateGameState :: Float -> GameState -> GameState
updateGameState seconds (Playing pillars score bird) = if birdCollision bird pillars then GameOver score
        else Playing (updatePillars seconds pillars) (updateScore score (updatePillars seconds pillars)) (gravityOnBird seconds bird)
    where
        updatePillars :: Float -> [Pillar] -> [Pillar]
        updatePillars seconds pillars = filter (not . isPillarOutOfScreen) (movePillars (seconds * 150) pillars)

        isPillarOutOfScreen :: Pillar -> Bool
        isPillarOutOfScreen pillar = xAxisPosition pillar <= -900

        updateScore :: Int -> [Pillar] -> Int
        updateScore prev pillars = 
            let pastPillars = filter isPillarPast pillars
            in prev + length pastPillars
        
        isPillarPast :: Pillar -> Bool
        isPillarPast pillar = xAxisPosition pillar <= -70 && xAxisPosition pillar > -71
updateGameState _ Menu = Menu
updateGameState _ (GameOver score) = GameOver score

-- user input handler, space equals jump
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (Playing pillars score bird) = 
    let jump = 500
        updatedBird = bird { velocity = (fst (velocity bird), jump) }
    in Playing pillars score updatedBird
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) (GameOver _) = Menu
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu = Playing { pillars = fst (generatePillars 0 1000 (mkStdGen 42)), score = 0, bird = generateBird }
handleInput _ gameState = gameState


