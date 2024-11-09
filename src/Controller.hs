{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import LevelImporter (levelBuilder)
import Data.Maybe ( mapMaybe )
import Data.List (delete)

--movement modifiers
karioSpeed :: Float
karioSpeed = 75
karioJumpStrength :: Float
karioJumpStrength = 300
karioGroundFriction :: Float
karioGroundFriction = 20
karioAirFriction :: Float
karioAirFriction = 3
karioGravity :: Float
karioGravity = 500
karioMaxFallSpeed :: Float
karioMaxFallSpeed = 300

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs (GameMenu menuState s l)   = return (GameMenu (stepMenu secs menuState) s l)
step secs (GameLevel levelState s l) = return (GameLevel (stepLevel secs (handleLoggedInputs levelState)) s l) --first handles the logged inputs and then handles all the other level logic

-- | Handle one iteration of the menu
stepMenu :: Float -> MenuState -> MenuState
stepMenu secs menuState = menuState

-- | Handle one iteration of the level
stepLevel :: Float -> LevelState -> LevelState
stepLevel secs levelState@(LevelState {kario, elapsedGameTime, platforms}) = levelState {
    kario = stepKario secs platforms kario,                                                --manipulate kario
    elapsedGameTime = elapsedGameTime + secs
    }

-- | kario step logic
------------------------------------------------------------------------------------------

--applies all the functions to kario that make up a step
stepKario :: Float -> [Platform] -> Kario -> Kario
stepKario secs platforms kario@Kario{hitbox = Hitbox{pos = prevPos}} =
    ({-karioHandleVerticalVelocity .-} handleKarioPlatformCollisions prevPos platforms . moveKario secs platforms . applyFrictionToKario secs . accelerateKario secs . applyGravityToKario secs) kario

-- handles the collision between kario and all the platforms. Determines whether collisions are horizontal or vertical and acts accordingly.
handleKarioPlatformCollisions :: Position -> [Platform] -> Kario -> Kario
handleKarioPlatformCollisions (prevX, prevY) platforms movedKario = foldr handleKarioPlatformCollision movedKario{airborne = Falling} $ getOverlaps movedKario platforms
  where
    handleKarioPlatformCollision :: Hitbox -> Kario -> Kario
    handleKarioPlatformCollision Hitbox{width = overlapX, height = overlapY} kario@Kario{hitbox = hitbox@Hitbox{pos = (newX, newY)}, dirVelocity = (vx, vy)}
      | overlapX < overlapY = kario{hitbox = hitbox{pos = (prevX, newY)}, dirVelocity = (0, vy)}                      --when the horizontal overlap is smaller we treat the collision as a horizontal one
      | vy > 0              = kario{hitbox = hitbox{pos = (newX, prevY)}, dirVelocity = (vx, 0)}                      --if it is not horizontal and there is upwards velocity kario bumps its head
      | otherwise           = kario{hitbox = hitbox{pos = (newX, prevY)}, dirVelocity = (vx, 0), airborne = Grounded} --otherwise we treat it as a vertical collision where kario falls

--moves kario using its current velocity
moveKario :: Float -> [Platform] -> Kario -> Kario
moveKario secs platforms kario@Kario{hitbox = hitbox@Hitbox{pos = (px, py)}, dirVelocity = (vx, vy)} = kario{hitbox = hitbox{pos = (px + secs * vx, py + secs * vy)}}

-- !! currently not in use !! Adds velocity to kario based on its acceleration
accelerateKario :: Float -> Kario -> Kario
accelerateKario secs kario@Kario{dirAccel = (accX, accY), dirVelocity = (velX, velY)} = kario{dirVelocity = (velX + secs * accX, velY + secs *accY)}

--adds the right amount of velocity to kario based on its desired velocity and the current friction
applyFrictionToKario :: Float -> Kario -> Kario
applyFrictionToKario secs kario@Kario{dirVelocity = (velX, velY), airborne, desiredHorizontalVelocity} = kario{dirVelocity = (velX + (desiredHorizontalVelocity - velX) * (secs * friction airborne), velY)}
  where
    friction Grounded = karioGroundFriction
    friction _        = karioAirFriction

--accelerates kario by the set gravity. Does not accelerate past the max fall speed
applyGravityToKario :: Float -> Kario -> Kario
applyGravityToKario secs kario@Kario{dirVelocity = (velX, velY)} = kario{dirVelocity = (velX, max (velY - secs * karioGravity) (-karioMaxFallSpeed))}


------------------------------------------------------------------------------------------
-- | Handle user input

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

--handle special inputs and log normal inputs
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameMenu _ sprites l) = GameLevel (levelBuilder (head l)) sprites l                     --switching to level
inputKey (EventKey (Char c) ks _ _) (GameLevel levelState@(LevelState {inputState}) sprites l) = GameLevel (levelState {
    inputState = logInput c ks inputState                                                                                                   --logging level input
    }) sprites l
inputKey _ gstate = gstate                                                                                                                  --edge cases without handling

logInput :: Char -> KeyState -> Inputs -> Inputs
logInput c Down = (c:)
logInput c Up   = delete c

--handle the logged inputs
handleLoggedInputs :: LevelState -> LevelState
handleLoggedInputs levelState@LevelState{inputState, kario} = levelState{
    kario = karioInput inputState kario
}

--should probably find another way of doing this as handling all combinations quickly becomes impossible with more possible inputs
karioInput :: Inputs -> Kario -> Kario
karioInput inputState kario@Kario{dirVelocity = (velX, velY), airborne}
  | aPressed && dPressed && jump = kario{desiredHorizontalVelocity = 0, dirVelocity = (velX, velY + karioJumpStrength), airborne = Rising}
  | aPressed && dPressed         = kario{desiredHorizontalVelocity = 0}
  | aPressed && jump             = kario{desiredHorizontalVelocity = -karioSpeed, dirVelocity = (velX, velY + karioJumpStrength), airborne = Rising}
  | dPressed && jump             = kario{desiredHorizontalVelocity = karioSpeed, dirVelocity = (velX, velY + karioJumpStrength), airborne = Rising}
  | aPressed                     = kario{desiredHorizontalVelocity = -karioSpeed}
  | dPressed                     = kario{desiredHorizontalVelocity = karioSpeed}
  | jump                         = kario{dirVelocity = (velX, velY + karioJumpStrength), airborne = Rising}
  | otherwise                    = kario{desiredHorizontalVelocity = 0}
    where
      aPressed = 'a' `elem` inputState
      dPressed = 'd' `elem` inputState
      jump = 'w' `elem` inputState && airborne == Grounded

removeLast :: String -> String
removeLast [] = []
removeLast [_] = []
removeLast (x:xs) = x : removeLast xs

--------------------------------------------------------------------------------------------
getOverlaps :: (Collidable a, Collidable b) => a -> [b] -> [Hitbox]
getOverlaps subject = mapMaybe (getOverlap (getBox subject) . getBox)

--calculates the intersecting part of the hitboxes, useful for more advanced collision detection for movement
getOverlap :: Hitbox -> Hitbox -> Maybe Hitbox
getOverlap b1@(Hitbox (x1, y1) w1 h1) b2@(Hitbox (x2, y2) w2 h2) | isOverlapping b1 b2 = Just $ Hitbox (xmin, xmax) (xmax - xmin) (ymax - ymin)
                                                                 | otherwise           = Nothing
  where
    xmin = max x1 x2
    xmax = min (x1 + w1) (x2 + w2)
    ymin = max y1 y2
    ymax = min (y1 + h1) (y2 + h2)

isOverlapping :: Hitbox -> Hitbox -> Bool
isOverlapping hitbox1 hitbox2 = any (isWithin hitbox1) (hitboxToPoints hitbox2) || any (isWithin hitbox2) (hitboxToPoints hitbox1)

isWithin :: Hitbox -> Position -> Bool
isWithin Hitbox{pos = (hx, hy), width = w, height = h} (px, py) =
    px >= hx - (1/2 * w) &&
    px <= hx + (1/2 * w) &&
    py >= hy - (1/2 * h) &&
    py <= hy + (1/2 * h)

hitboxToPoints :: Hitbox -> [Position]
hitboxToPoints Hitbox{pos = (x, y), width = w, height = h} = [(x - (1/2 * w),y - (1/2 * h)), (x + (1/2 * w), y - (1/2 * h)), (x - (1/2 * w), y + (1/2 * h)),  (x + (1/2 * w), y + (1/2 * h))]