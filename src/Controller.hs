{-# language NamedFieldPuns #-}
-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

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
stepLevel secs levelState@(LevelState {kario})
    = return levelState {kario = moveKario secs kario}

moveKario :: Float -> Kario -> Kario
moveKario secs kario@Kario{hitbox = hitbox@Hitbox{pos = (px, py)} ,dirVelocity = (vx, vy)}
    = kario {hitbox = hitbox{pos = (px + secs * vx, py + secs * vy)} }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameMenu _ sprites) = GameLevel initialLevelState sprites
inputKey (EventKey (SpecialKey KeyDelete) Down _ _) (GameMenu (MenuState s) sprites) = GameMenu (MenuState (removeLast s)) sprites
inputKey (EventKey (Char c) Down _ _) (GameMenu (MenuState s) sprites) = GameMenu (MenuState (s ++ [c])) sprites
inputKey (EventKey (Char c) Down _ _) (GameLevel levelState@(LevelState {lilInt}) sprites) = GameLevel levelState {lilInt = lilInt + 1} sprites
inputKey _ gstate = gstate

removeLast :: String -> String
removeLast [] = []
removeLast [_] = []
removeLast (x:xs) = x : removeLast xs