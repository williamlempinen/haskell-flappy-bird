module Bird (Bird(..), generateInitialBird, gravityOnBird) where

data Bird = Bird { location :: (Float, Float), velocity :: (Float, Float) } deriving Show

generateInitialBird :: IO Bird
generateInitialBird = return (Bird (0, 0) (0, 0))

gravityOnBird :: Float -> Bird -> Bird
gravityOnBird seconds bird = bird { location = (xPos, yPos + moveY), velocity = (xVel, yVel + gravity * seconds) }
    where
        (xPos, yPos) = location bird
        (xVel, yVel) = velocity bird
        gravity      = -300
        moveY        = yVel * seconds + 0.5 * gravity * seconds * seconds