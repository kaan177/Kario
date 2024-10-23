-- | This module contains the data types
--   which represent the state of the game
module Model where

data GameState = GameLevel LevelState | GameMenu MenuState

data LevelState = LevelState Int

data MenuState = MenuState GameName

type GameName = String

initialState :: GameState
initialState = GameMenu (MenuState "Kario")