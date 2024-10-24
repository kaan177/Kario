{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

--movement modifiers
karioMaxSpeed :: Float
karioMaxSpeed = 100
karioGroundAcceleration :: Float
karioGroundAcceleration = 180
karioAirAcceleration :: Float
karioAirAcceleration = 90
karioGroundedFriction :: Float
karioGroundedFriction = 100
karioAirFriction :: Float
karioAirFriction = 30

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
stepLevel secs levelState@(LevelState {kario, elapsedGameTime}) = return levelState {
    kario = stepKario secs kario ,                                   --manipulate kario
    elapsedGameTime = elapsedGameTime + secs
    }

-- | kario step logic
------------------------------------------------------------------------------------------
stepKario :: Float -> Kario -> Kario
stepKario secs kario@Kario{hitbox ,dirVelocity} = kario {
    hitbox = hitbox{pos = moveKario secs kario},
    dirVelocity = accelerateKario secs kario{dirVelocity = applyFrictionToKario secs kario}
    }

moveKario :: Float -> Kario -> Position
moveKario secs Kario {hitbox = hitbox@Hitbox{pos = (px, py)}, dirVelocity = (vx, vy)} = (px + secs * vx, py + secs * vy)

accelerateKario :: Float -> Kario -> DirectionalVelocity
accelerateKario secs Kario{dirAccel = (accX, accY), dirVelocity = (velX, velY)} | accX < 0  = (max (-karioMaxSpeed) (velX + secs * accX), velY + secs *accY)
                                                                                | otherwise = (min karioMaxSpeed (velX + secs * accX), velY + secs * accY)

applyFrictionToKario :: Float -> Kario -> DirectionalAcceleration
applyFrictionToKario secs Kario{dirVelocity = (velX, velY), airborne = Grounded} | velX < 0  = (min (velX + secs * karioGroundedFriction) 0, velY) --when grounded
                                                                                 | otherwise = (max (velX - secs * karioGroundedFriction) 0, velY)
applyFrictionToKario secs Kario{dirVelocity = (velX, velY)}                      | velX < 0  = (min (velX + secs * karioAirFriction) 0, velY)      --when airborne
                                                                                 | otherwise = (max (velX - secs * karioAirFriction) 0, velY)

applyGravityToKario :: Float -> Kario -> DirectionalAcceleration
applyGravityToKario = undefined


------------------------------------------------------------------------------------------

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameMenu _ sprites) = GameLevel initialLevelState sprites                        --switching to level
inputKey (EventKey (SpecialKey KeyDelete) Down _ _) (GameMenu (MenuState s) sprites) = GameMenu (MenuState (removeLast s)) sprites   --removing characters
inputKey (EventKey (Char c) Down _ _) (GameMenu (MenuState s) sprites) = GameMenu (MenuState (s ++ [c])) sprites                     --typing characters
inputKey (EventKey (Char c) ks _ _) (GameLevel levelState@(LevelState {lilInt, kario}) sprites) = GameLevel levelState {              --handling level input
    kario = karioInput c ks kario,                                                                                                      --handling kario related input
    lilInt = lilInt + 1
    } sprites 
inputKey _ gstate = gstate                                                                                                           --edge cases without handling

karioInput :: Char -> KeyState -> Kario -> Kario
karioInput 'a' Up kario@Kario{dirAccel = (accX,accY), airborne = Grounded} = kario {dirAccel = (0 ,accY) }
karioInput 'd' Up kario@Kario{dirAccel = (accX,accY), airborne = Grounded} = kario {dirAccel = (0 ,accY) }
karioInput 'a' _ kario@Kario{dirAccel = (accX,accY), airborne = Grounded} = kario {dirAccel = (-karioGroundAcceleration ,accY) }
karioInput 'd' _ kario@Kario{dirAccel = (accX,accY), airborne = Grounded} = kario {dirAccel = (karioGroundAcceleration ,accY) }
karioInput 'a' _ kario@Kario{dirAccel = (accX,accY)}                      = kario {dirAccel = (-karioAirAcceleration ,accY) }
karioInput 'd' _ kario@Kario{dirAccel = (accX,accY)}                      = kario {dirAccel = (karioAirAcceleration ,accY) }


removeLast :: String -> String
removeLast [] = []
removeLast [_] = []
removeLast (x:xs) = x : removeLast xs