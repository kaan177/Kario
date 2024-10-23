-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate = return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameMenu (MenuState s)) = GameLevel (LevelState 0)
inputKey (EventKey (SpecialKey KeyDelete) Down _ _) (GameMenu (MenuState s)) = GameMenu (MenuState (removeLast s))
inputKey (EventKey (Char c) Down _ _) (GameMenu (MenuState s)) = GameMenu (MenuState (s ++ [c]))
inputKey (EventKey (Char c) Down _ _) (GameLevel (LevelState n)) = GameLevel (LevelState (n + 1))
inputKey _ gstate = gstate

removeLast :: String -> String
removeLast [] = []
removeLast [_] = []
removeLast (x:xs) = x : removeLast xs