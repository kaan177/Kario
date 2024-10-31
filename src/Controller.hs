{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.Maybe ( mapMaybe )

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
step secs (GameMenu menuState s)   = do menu <- stepMenu secs menuState
                                        return (GameMenu menu s)
step secs (GameLevel levelState s) = do level <- stepLevel secs levelState
                                        return (GameLevel level s)

-- | Handle one iteration of the menu
stepMenu :: Float -> MenuState -> IO MenuState
stepMenu secs menuState = return menuState

-- | Handle one iteration of the level
stepLevel :: Float -> LevelState -> IO LevelState
stepLevel secs levelState@(LevelState {kario, elapsedGameTime, platforms}) = return levelState {
    kario = stepKario secs platforms kario,                                   --manipulate kario
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
      | otherwise           = kario{hitbox = hitbox{pos = (newX, prevY)}, dirVelocity = (vx, 0), airborne = Grounded} --otherwise we treat it as a vertical collision

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

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameMenu _ sprites) = GameLevel initialLevelState sprites                        --switching to level
inputKey (EventKey (SpecialKey KeyDelete) Down _ _) (GameMenu (MenuState s) sprites) = GameMenu (MenuState (removeLast s)) sprites   --removing characters
inputKey (EventKey (Char c) Down _ _) (GameMenu (MenuState s) sprites) = GameMenu (MenuState (s ++ [c])) sprites                     --typing characters
inputKey (EventKey (Char c) ks _ _) (GameLevel levelState@(LevelState {kario}) sprites) = GameLevel levelState {                     --handling level input
    kario = karioInput c ks kario                                                                                                    --handling kario related input
    } sprites
inputKey _ gstate = gstate                                                                                                           --edge cases without handling

karioInput :: Char -> KeyState -> Kario -> Kario
karioInput 'a' Up kario                                                       = kario {desiredHorizontalVelocity = 0}
karioInput 'd' Up kario                                                       = kario {desiredHorizontalVelocity = 0}
karioInput 'a' _ kario                                                        = kario {desiredHorizontalVelocity = -karioSpeed}
karioInput 'd' _ kario                                                        = kario {desiredHorizontalVelocity = karioSpeed}
karioInput 'w' _ kario@Kario{dirVelocity = (velX, velY), airborne = Grounded} = kario {dirVelocity = (velX, velY + karioJumpStrength), airborne = Rising}
karioInput  _  _ kario                                                        = kario

removeLast :: String -> String
removeLast [] = []
removeLast [_] = []
removeLast (x:xs) = x : removeLast xs

--------------------------------------------------------------------------------------------
-- | helper functions
sign :: (Ord a, Num a) => a -> a
sign x | x < 0     = -1
       | otherwise = 1

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