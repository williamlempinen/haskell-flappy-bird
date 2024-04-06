module GameState (GameState(..)
                 , initialState
                 , renderGame
                 , handleInput
                 , updateGameState
                 ) where

import Pillar (Pillar(..), movePillars, generatePillars)
import Graphics.Gloss (Picture(Pictures))
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))
import UI (drawPillar, drawBird, drawGameOver, drawScore, drawMenu)
import Bird (Bird(..), gravityOnBird, birdCollision, generateBird)
import System.Random (mkStdGen)

data GameState = Menu
               | Playing  { pillars :: [Pillar], score :: Int, bird :: Bird }
               | GameOver { score :: Int }
               deriving Show

-- initial state when app is launched
initialState :: GameState
initialState = Menu

-- draw static elements 
renderGame :: Picture -> GameState -> Picture
renderGame bg (Playing p s b) = Pictures (bg 
                                         : drawBird b
                                         : drawScore s
                                         : map drawPillar p)
renderGame bg (GameOver s)    = Pictures [bg, drawGameOver, drawScore s]
renderGame bg Menu            = Pictures [bg, drawMenu]

-- game state handler
updateGameState :: Float -> GameState -> GameState
updateGameState seconds (Playing p s b) = if birdCollision b p then GameOver s
        else Playing (updatePillars seconds p) (updateScore s (updatePillars seconds p)) (gravityOnBird seconds b)
    where
        updatePillars :: Float -> [Pillar] -> [Pillar]
        updatePillars seconds p = filter (not . isPillarOutOfScreen) (movePillars (seconds * 150) p)

        isPillarOutOfScreen :: Pillar -> Bool
        isPillarOutOfScreen pillar = xAxisPosition pillar <= -900

        updateScore :: Int -> [Pillar] -> Int
        updateScore prev p = 
            let pastPillars = filter isPillarPast p
            in prev + length pastPillars
        
        isPillarPast :: Pillar -> Bool
        isPillarPast pillar = xAxisPosition pillar <= -70 && xAxisPosition pillar > -71
updateGameState _ Menu         = Menu
updateGameState _ (GameOver s) = GameOver s

-- user input handler, space equals jump, enter equals continue and play
handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeySpace) Down _ _) (Playing p s b) = 
    let jump = 550
        updatedBird = b { velocity = jump }
    in Playing p s updatedBird
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) (GameOver _) = Menu
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) Menu         = Playing { pillars = fst (generatePillars 0 1000 (mkStdGen 42)), 
                                                                               score   = 0, 
                                                                               bird    = generateBird }
handleInput _ gameState                                            = gameState


