module Bird (Bird(..), generateBird, gravityOnBird, birdCollision) where

import Pillar (Pillar(..))

data Bird = Bird { location :: (Float, Float), velocity :: Float } deriving Show

generateBird :: Bird
generateBird = Bird (0, 0) 0 

gravityOnBird :: Float -> Bird -> Bird
gravityOnBird seconds bird = bird { location = (xPos, yPos + moveY), velocity = yVel + gravity * seconds }
    where
        (xPos, yPos) = location bird
        yVel         = velocity bird
        gravity      = -600
        moveY        = yVel * seconds + 0.5 * gravity * seconds * seconds

birdCollision :: Bird -> [Pillar] -> Bool
birdCollision bird pillars = any (collisionWithBird bird) pillars

collisionWithBird :: Bird -> Pillar -> Bool
collisionWithBird (Bird (xPos, yPos) _) pillar =
    let birdLeft            = xPos - 30
        birdRight           = xPos + 30
        birdTop             = yPos + 30
        birdBottom          = yPos - 30
        
        pillarLeft          = xAxisPosition pillar - width pillar / 2
        pillarRight         = xAxisPosition pillar + width pillar / 2
        gapTop              = gapPosition pillar + gap pillar / 2
        gapBottom           = gapPosition pillar - gap pillar / 2

        horizontalCollision = birdRight >= pillarLeft && birdLeft <= pillarRight
        verticalCollision   = birdTop >= gapTop || birdBottom <= gapBottom
        wallCollision       = birdTop >= 500 || birdBottom <= -500
    in (horizontalCollision && verticalCollision) || wallCollision
