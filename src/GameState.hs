module GameState where

import Pillar (Pillar(..), generateInitialPillars)
import Graphics.Gloss

data GameState = Menu
               | Playing  { pillars :: [Pillar], score :: Int }
               | GameOver { endScore :: Int }
               deriving Show

initialState :: GameState
initialState = Menu

startGame :: GameState -> IO GameState
startGame Menu = do
    initialPillars <- generateInitialPillars
    return $ Playing initialPillars 0
startGame state = return state

endGame :: GameState -> GameState
endGame (Playing _ score) = GameOver score
endGame state = state

incrementScore :: GameState -> GameState
incrementScore (Playing pillars score) = Playing pillars (score + 1)
incrementScore state = state

-- notice the placeholder
renderGameState :: GameState -> Picture
renderGameState gameState = rectangleSolid 1 1