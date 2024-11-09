{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import LevelImporter (levelBuilder)
import Data.Data (ConstrRep(FloatConstr))
import Data.Maybe ( fromMaybe )
import GHC.Clock (getMonotonicTimeNSec)

--movement modifiers
karioSpeed :: Float
karioSpeed = 50
karioJumpStrength :: Float
karioJumpStrength = 250
karioGroundFriction :: Float
karioGroundFriction = 20
karioAirFriction :: Float
karioAirFriction = 2
karioGravity :: Float
karioGravity = 500
karioMaxFallSpeed :: Float
karioMaxFallSpeed = 250

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs (GameMenu menuState s l)   = do
    menu <- stepMenu secs menuState
    return (GameMenu menu s l)
step secs (GameLevel levelState s l) = do
    level <- stepLevel secs levelState
    return (GameLevel level s l)


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
stepKario secs = moveKario secs . applyFrictionToKario secs . accelerateKario secs . applyGravityToKario secs

moveKario :: Float -> Kario -> Kario
moveKario secs kario@Kario{hitbox = hitbox@Hitbox{pos = (px, py)}, dirVelocity = (vx, vy)} = kario{hitbox = hitbox{pos = (px + secs * vx, py + secs * vy)}}

accelerateKario :: Float -> Kario -> Kario
accelerateKario secs kario@Kario{dirAccel = (accX, accY), dirVelocity = (velX, velY)} = kario{dirVelocity = (velX + secs * accX, velY + secs *accY)}

applyFrictionToKario :: Float -> Kario -> Kario
applyFrictionToKario secs kario@Kario{dirVelocity = (velX, velY), airborne, desiredHorizontalVelocity} = kario{dirVelocity = (velX + (desiredHorizontalVelocity - velX) * (secs * friction airborne), velY)}
  where
    friction Grounded = karioGroundFriction
    friction _        = karioAirFriction

applyGravityToKario :: Float -> Kario -> Kario
applyGravityToKario _ kario@Kario{airborne = Grounded}                               = kario                                                          --when grounded no gravity acceleration applied.
applyGravityToKario secs kario@Kario{dirVelocity = (velX, velY)} = kario{dirVelocity = (velX, max (velY - secs * karioGravity) (-karioMaxFallSpeed))} --when airborne gravity is appliead.


------------------------------------------------------------------------------------------
-- | Handle user input

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey e menu@GameMenu {} = menuStateInput e menu         --switching to level
inputKey _ (GameMenu menu@MenuState {} sprites l) = GameMenu menu sprites l                                                          --typing characters
inputKey (EventKey (Char c) ks _ _) (GameLevel levelState@(LevelState {kario}) sprites l) = GameLevel levelState {              --handling level input
    kario = karioInput c ks kario                                                                                                    --handling kario related input
    } sprites l
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

menuStateInput :: Event -> GameState -> GameState
menuStateInput (EventKey (SpecialKey KeyRight) Down _ _)  = menuRightPress
menuStateInput (EventKey (SpecialKey KeyLeft) Down _ _)  = menuLeftPress
menuStateInput (EventKey (SpecialKey KeyEnter) Down _ _)  = menuEnterPress
menuStateInput e = id

menuRightPress :: GameState -> GameState
menuRightPress g@(GameMenu MenuState {selectedLevel = Just i} _ (LoadedLevels l)) | i >= length l - 1 = g
menuRightPress g@(GameMenu m@MenuState {selectedLevel = Just i} p (LoadedLevels l))  = GameMenu m{selectedLevel = Just (i + 1), selector = Just (Selector (Hitbox (buttonPosition (i + 1)) 0 0))} p (LoadedLevels l)
menuRightPress g = g

menuLeftPress :: GameState -> GameState
menuLeftPress g@(GameMenu MenuState {selectedLevel = Just i} _ (LoadedLevels l)) | i <= 0 = g
menuLeftPress g@(GameMenu m@MenuState {selectedLevel = Just i} p (LoadedLevels l))  = GameMenu m{selectedLevel = Just (i - 1), selector = Just (Selector (Hitbox (buttonPosition (i - 1)) 0 0))} p (LoadedLevels l)
menuLeftPress g = g

menuEnterPress :: GameState -> GameState
menuEnterPress (GameMenu MenuState {selectedLevel = Just i} p (LoadedLevels l)) = GameLevel (levelBuilder (l !! i)) p (LoadedLevels l)
menuEnterPress g = g
--------------------------------------------------------------------------------------------
-- | helper functions
sign :: (Ord a, Num a) => a -> a
sign x | x < 0     = -1
       | otherwise = 1