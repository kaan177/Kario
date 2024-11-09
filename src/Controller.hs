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
import Data.Maybe ( mapMaybe )
import Data.List (delete)

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs (GameMenu menuState s l)   = return (GameMenu (stepMenu secs menuState) s l)
step secs (GameLevel levelState s l) = return (GameLevel (stepLevel secs (handleLoggedInputs levelState)) s l) --first handles the logged inputs and then handles all the other level logic

-- | Handle one iteration of the menu
stepMenu :: Float -> MenuState -> MenuState
stepMenu secs menuState = menuState

-- | Handle one iteration of the level
stepLevel :: Float -> LevelState -> LevelState
stepLevel secs levelState@(LevelState {kario, elapsedGameTime, platforms, enemies}) = levelState {
    kario = stepKario secs platforms kario,  --manipulate kario
    elapsedGameTime = elapsedGameTime + secs,
    enemies = map (stepEnemy secs platforms kario) enemies
    }

------------------------------------------------------------------------------------------
-- | Handle user input

input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

--handle special inputs and log normal inputs
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameMenu _ sprites l) = GameLevel (levelBuilder (head l)) sprites l  --switching to level
inputKey (EventKey (Char c) ks _ _) (GameLevel levelState@(LevelState {inputState}) sprites l) = GameLevel (levelState {
    inputState = logInput c ks inputState                                                                                --logging level input
    }) sprites l
inputKey _ gstate = gstate                                                                                               --edge cases without handling

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

--------------------------------------------------------------------------------------------

