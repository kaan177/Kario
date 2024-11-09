{-# LANGUAGE NamedFieldPuns #-}
module KarioLogic(stepKario, karioInput) where

import Model
import Collision
import Movable
import Accelerable
import Positioning
import GHC.Float

--movement modifiers
karioSpeed :: Float
karioSpeed = 75
karioJumpStrength :: Float
karioJumpStrength = 300
karioGroundFriction :: Float
karioGroundFriction = 20
karioAirFriction :: Float
karioAirFriction = 3

--applies all the functions to kario that make up a step
stepKario :: Float -> [Platform] -> [Enemy] -> Kario -> Kario
stepKario secs platforms enemies kario@Kario{karHitbox = Hitbox{pos = prevPos}} =
  (handleExistence secs . handleOutOfBounds . moveAndCollide secs platforms . applyFriction secs . accelerateKario secs . applyGravity secs . handleEnemyCollisions enemies) kario 

handleOutOfBounds :: Kario -> Kario
handleOutOfBounds kario | posY < bottomBound = kario{karioExist = RemoveIn 0}
                        | posX < leftBound   = updatePos kario (leftBound, posY) 
                        | otherwise          = kario
  where  
    (posX, posY) = getPos kario
    leftBound    = -int2Float(fst screenSize `div` 2)
    bottomBound  = -int2Float(snd screenSize `div` 2)

handleExistence :: Float -> Kario -> Kario
handleExistence _ k@Kario{karioExist = Exist}         = k
handleExistence secs k@Kario{karioExist = RemoveIn x} = k{karioExist = RemoveIn (max 0 (x - secs))}

handleEnemyCollisions :: [Enemy] -> Kario -> Kario
handleEnemyCollisions enemies kario = foldr handleEnemyCollision kario $ getOverlaps kario enemies

handleEnemyCollision :: Overlap -> Kario -> Kario    
handleEnemyCollision Hitbox{width = overlapX, height = overlapY} kario 
  | overlapX < overlapY = kario{karioExist = RemoveIn 0}            --horizontal collision, should die here                                                                     
  | velY < 0            = updateVel (velX, karioJumpStrength) kario --jumps on enemy                                                                    
  | otherwise           = kario{karioExist = RemoveIn 0}            --vertical but not from above, also dies
  where (velX,velY) = getVel kario

--by moving over the x-axis and y-axis seperately we avoid some bugs that arose from our collision implementation
moveAndCollide :: Float -> [Platform] -> Kario -> Kario
moveAndCollide secs platforms kario = handlePlatformCollisions (getPos kario') platforms . moveY secs $ kario'
  where
    kario' = handlePlatformCollisions (getPos kario) platforms . moveX secs $ kario

-- handles the collision between kario and all the platforms. Determines whether collisions are horizontal or vertical and acts accordingly.
handlePlatformCollisions :: Position -> [Platform] -> Kario -> Kario
handlePlatformCollisions (prevX, prevY) platforms movedKario = foldr handlePlatformCollision movedKario{airborne = Falling} $ getOverlaps movedKario platforms
  where
    handlePlatformCollision :: Hitbox -> Kario -> Kario
    handlePlatformCollision Hitbox{width = overlapX, height = overlapY} kario@Kario{karHitbox = hitbox@Hitbox{pos = (newX, newY)}, karVel = (vx, vy)}
      | overlapX < overlapY = kario{karHitbox = hitbox{pos = (prevX, newY)}, karVel = (0, vy)}                      --when the horizontal overlap is smaller we treat the collision as a horizontal one
      | vy > 0              = kario{karHitbox = hitbox{pos = (newX, prevY)}, karVel = (vx, 0)}                      --if it is not horizontal and there is upwards velocity kario bumps its head
      | otherwise           = kario{karHitbox = hitbox{pos = (newX, prevY)}, karVel = (vx, 0), airborne = Grounded} --otherwise we treat it as a vertical collision where kario falls

{- --moves kario using its current velocity
moveKario :: Float -> Kario -> Kario
moveKario secs kario@Kario{karHitbox = hitbox@Hitbox{pos = (px, py)}, karVel = (vx, vy)} = kario{karHitbox = hitbox{pos = (px + secs * vx, py + secs * vy)}} -}

-- !! currently not in use !! Adds velocity to kario based on its acceleration
accelerateKario :: Float -> Kario -> Kario
accelerateKario secs kario@Kario{karAccel = (accX, accY), karVel = (velX, velY)} = kario{karVel = (velX + secs * accX, velY + secs *accY)}

--adds the right amount of velocity to kario based on its desired velocity and the current friction
applyFriction :: Float -> Kario -> Kario
applyFriction secs kario@Kario{karVel = (velX, velY), airborne, desiredHorizontalVelocity} = kario{karVel = (velX + (desiredHorizontalVelocity - velX) * (secs * friction airborne), velY)}
  where
    friction Grounded = karioGroundFriction
    friction _        = karioAirFriction 

-----------------------------------------------------------------------------------------------------------------------------------
--{INPUT}

--should probably find another way of doing this as handling all combinations quickly becomes impossible with more possible inputs
karioInput :: Inputs -> Kario -> Kario
karioInput inputState kario@Kario{karVel = (velX, velY), airborne}
  | aPressed && dPressed && jump = kario{desiredHorizontalVelocity = 0, karVel = (velX, velY + karioJumpStrength), airborne = Rising}
  | aPressed && dPressed         = kario{desiredHorizontalVelocity = 0}
  | aPressed && jump             = kario{desiredHorizontalVelocity = -karioSpeed, karVel = (velX, velY + karioJumpStrength), airborne = Rising}
  | dPressed && jump             = kario{desiredHorizontalVelocity = karioSpeed, karVel = (velX, velY + karioJumpStrength), airborne = Rising}
  | aPressed                     = kario{desiredHorizontalVelocity = -karioSpeed}
  | dPressed                     = kario{desiredHorizontalVelocity = karioSpeed}
  | jump                         = kario{karVel = (velX, velY + karioJumpStrength), airborne = Rising}
  | otherwise                    = kario{desiredHorizontalVelocity = 0}
    where
      aPressed = 'a' `elem` inputState
      dPressed = 'd' `elem` inputState
      jump = 'w' `elem` inputState && airborne == Grounded