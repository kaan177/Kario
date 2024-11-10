{-# LANGUAGE NamedFieldPuns #-}
module KarioLogic(stepKario, karioInput) where

import Model
import Collision
import Movable
import Accelerable
import GHC.Float
import Animation
import Data.Tuple

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
stepKario :: [PowerUp] -> Float -> [Platform] -> [Enemy] -> Kario -> Kario
stepKario powerUps secs platforms enemies kario@Kario{karHitbox = Hitbox{pos = prevPos}} =
    handleExistence secs 
  . handleOutOfBounds
  . moveAndCollide secs platforms
  . applyFriction secs 
  . applyGravity secs 
  . handleEnemyCollisions enemies 
  . handleAnimations
  . updateAnimation secs
  . handlePowerUpCollisions powerUps
  . handleInvincibility secs
  $ kario 

handleAnimations :: Kario -> Kario

handleAnimations k@Kario{karAnim = Dying _ _}                        = k --nothing should change when dying
handleAnimations k@Kario{airborne = Grounded, karAnim = Walking _ _} | closeToStill $ fst (getVel k) = k{karAnim = Idle} --change to idle, standing still
                                                                     | otherwise                     = k                 --do nothing,     already walking
handleAnimations k@Kario{airborne = Grounded, karAnim = _}           | closeToStill $ fst (getVel k) = k{karAnim = Idle}                    --change to idle, standing still     
                                                                     | otherwise                     = k{karAnim = Walking 0 frameDuration} --change to walking, moving on the ground
handleAnimations k@Kario{airborne = Airborne, karAnim = Jumping}     = k                    --in air, but already jumping, do nothing                                                
handleAnimations k@Kario{airborne = Airborne}                        | closeToStill $ snd (getVel k) = k                    --barely vertical momentum, not really falling/jumping yet.
                                                                     | otherwise                     = k{karAnim = Jumping} --in air with momentum but not jumping so change animation to jumping
--handleAnimations k                                                   = k

closeToStill :: Float -> Bool
closeToStill velX = velX < 2 && velX > -2

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
handleEnemyCollision Hitbox{width = overlapX, height = overlapY} kario@Kario{powerUp}
  | overlapX < overlapY = hitKario kario            --horizontal collision, should die here                                                                     
  | velY < 0            = updateVel (velX, karioJumpStrength) kario --jumps on enemy                                                                    
  | otherwise           = hitKario kario            --vertical but not from above, also dies
  where (velX,velY) = getVel kario

hitKario :: Kario -> Kario
hitKario k@Kario{invincibleState = Invincible _} = k
hitKario k@Kario{powerUp = Big, karHitbox} = k{powerUp = Small, invincibleState = Invincible 0.5, karHitbox = karHitbox{height = height karHitbox / 1.5}}
hitKario k@Kario{} = k{karioExist = RemoveIn 0}

handlePowerUpCollisions :: [PowerUp] -> Kario -> Kario
handlePowerUpCollisions powerups kario = foldr handlePowerUpCollision kario (filter (isColliding kario) powerups)

handlePowerUpCollision :: PowerUp -> Kario -> Kario
handlePowerUpCollision Mushroom {} kario@Kario{karHitbox, powerUp = Small} = kario{powerUp = Big, karHitbox = karHitbox{height = height karHitbox * 1.5, pos = (\(x,y) -> (x ,y + 23)) (pos karHitbox)}  }
handlePowerUpCollision Mushroom {} kario@Kario{karHitbox} = kario
handlePowerUpCollision Star {} kario = kario{invincibleState = Invincible 10}

handleInvincibility :: Float -> Kario -> Kario
handleInvincibility secs k@Kario{invincibleState = Invincible x} | x <= 0 = k{invincibleState = Vulnerable}
                                                                 | otherwise = k{invincibleState = Invincible (x - secs)}
handleInvincibility secs k@Kario{} = k

moveAndCollide :: Float -> [Platform] -> Kario -> Kario
moveAndCollide secs platforms kario = foldl collisionFailSafe (handlePlatformCollisions (getPos kario) platforms . move secs $ kario) platforms

-- handles the collision between kario and all the platforms. Determines whether collisions are horizontal or vertical and acts accordingly.
handlePlatformCollisions :: Position -> [Platform] -> Kario -> Kario
handlePlatformCollisions (prevX, prevY) platforms movedKario = foldr handlePlatformCollision movedKario{airborne = Airborne} $ getOverlaps movedKario platforms
  where
    handlePlatformCollision :: Hitbox -> Kario -> Kario
    handlePlatformCollision Hitbox{width = overlapX, height = overlapY} kario@Kario{karHitbox = hitbox@Hitbox{pos = (newX, newY)}, karVel = (vx, vy)}
      | overlapX < overlapY = kario{karHitbox = hitbox{pos = (prevX, newY)}, karVel = (0, vy)}                      --when the horizontal overlap is smaller we treat the collision as a horizontal one
      | vy > 0              = kario{karHitbox = hitbox{pos = (newX, prevY)}, karVel = (vx, 0)}                      --if it is not horizontal and there is upwards velocity kario bumps its head
      | otherwise           = kario{karHitbox = hitbox{pos = (newX, prevY)}, karVel = (vx, 0), airborne = Grounded} --otherwise we treat it as a vertical collision where kario falls


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
  | aPressed && dPressed && jump = kario{desiredHorizontalVelocity = 0, karVel = (velX, velY + karioJumpStrength), airborne = Airborne}
  | aPressed && dPressed         = kario{desiredHorizontalVelocity = 0}
  | aPressed && jump             = kario{desiredHorizontalVelocity = -karioSpeed, karVel = (velX, velY + karioJumpStrength), airborne = Airborne}
  | dPressed && jump             = kario{desiredHorizontalVelocity = karioSpeed, karVel = (velX, velY + karioJumpStrength), airborne = Airborne}
  | aPressed                     = kario{desiredHorizontalVelocity = -karioSpeed}
  | dPressed                     = kario{desiredHorizontalVelocity = karioSpeed}
  | jump                         = kario{karVel = (velX, velY + karioJumpStrength), airborne = Airborne}
  | otherwise                    = kario{desiredHorizontalVelocity = 0}
    where
      aPressed = 'a' `elem` inputState
      dPressed = 'd' `elem` inputState
      jump = 'w' `elem` inputState && airborne == Grounded