{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import KarioLogic
import Graphics.Gloss
import EnemyLogic
import Graphics.Gloss.Interface.IO.Game
import LevelImporter (levelBuilder)
import Data.Data (ConstrRep(FloatConstr))
import Data.Maybe ( fromMaybe, mapMaybe )
import GHC.Clock (getMonotonicTimeNSec)
import Data.List (delete)
import Collision (isOverlapping, isColliding)
--movement modifiers
karioSpeed :: Float
karioSpeed = 50
karioJumpStrength :: Float
karioJumpStrength = 250
karioGroundFriction :: Float
karioGroundFriction = 20
karioAirFriction :: Float
karioAirFriction = 2
karioMaxFallSpeed :: Float
karioMaxFallSpeed = 250

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs (GameMenu menuState s l)   = return (GameMenu (stepMenu secs menuState) s l )
step secs (GameLevel levelState@LevelState{kario, flagPole, coinLevelScore} s l )| isColliding kario flagPole = do
    _ <- writeFile "Coins\\Coins.txt" (show coinLevelScore)
    return (GameMenu (initialMenuState l coinLevelScore) s l )
step secs (GameLevel levelState s l ) = return (GameLevel (stepLevel secs (handleLoggedInputs levelState)) s l ) --first handles the logged inputs and then handles all the other level logic

-- | Handle one iteration of the menu
stepMenu :: Float -> MenuState -> MenuState
stepMenu secs menuState = menuState

-- | Handle one iteration of the level
stepLevel :: Float -> LevelState -> LevelState
stepLevel secs levelState@(LevelState {kario, elapsedGameTime, platforms, enemies, coins, coinLevelScore}) =    levelState {
    kario = stepKario secs platforms kario,  --manipulate kario
    elapsedGameTime = elapsedGameTime + secs,
    enemies = map (stepEnemy secs platforms kario) enemies,
    coins = filter (not . isColliding kario) coins,
    coinLevelScore = coinLevelScore + length (filter (isColliding kario) coins)
    }



------------------------------------------------------------------------------------------
-- | Handle user input

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

--handle special inputs and log normal inputs
inputKey :: Event -> GameState -> GameState
inputKey e menu@GameMenu {} = menuStateInput e menu                                                                                  --menu input logic                                                    
inputKey (EventKey (Char c) ks _ _) (GameLevel levelState@(LevelState {inputState}) sprites l ) = GameLevel (levelState {
    inputState = logInput c ks inputState                                                                                --logging level input
    }) sprites l
inputKey _ gstate = gstate

logInput :: Char -> KeyState -> Inputs -> Inputs
logInput c Down = (c:)
logInput c Up   = delete c

--handle the logged inputs
handleLoggedInputs :: LevelState -> LevelState
handleLoggedInputs levelState@LevelState{inputState, kario} = levelState{
    kario = karioInput inputState kario
}

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
menuRightPress g@(GameMenu MenuState {selectedLevel = Just i} _ l) | i >= length l - 1 = g
menuRightPress g@(GameMenu m@MenuState {selectedLevel = Just i} p l )  = GameMenu m{selectedLevel = Just (i + 1), selector = Just (Selector (Hitbox (buttonPosition (i + 1)) 0 0))} p l
menuRightPress g = g

menuLeftPress :: GameState -> GameState
menuLeftPress g@(GameMenu MenuState {selectedLevel = Just i} _ l ) | i <= 0 = g
menuLeftPress g@(GameMenu m@MenuState {selectedLevel = Just i} p l )  = GameMenu m{selectedLevel = Just (i - 1), selector = Just (Selector (Hitbox (buttonPosition (i - 1)) 0 0))} p l
menuLeftPress g = g

menuEnterPress :: GameState -> GameState
menuEnterPress (GameMenu MenuState {selectedLevel = Just i, coinMenuScore} p l ) = GameLevel (levelBuilder (l !! i) coinMenuScore) p l
menuEnterPress g = g
--------------------------------------------------------------------------------------------

