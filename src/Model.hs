{-# language NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data GameState = GameLevel LevelState | GameMenu MenuState

data LevelState = LevelState {
    kario :: Kario
    , lilInt :: Int
    }

data MenuState = MenuState GameName

data Kario = Kario {
    hitbox :: Hitbox
    ,dirVelocity :: DirectionalVelocity
}
data Hitbox = Hitbox Position Width Height --origin in centre
type Position = Point
type Width = Float
type Height = Float
type DirectionalVelocity = Vector

type GameName = String

initialState :: GameState
initialState = GameMenu (MenuState "Kario")

initialLevelState :: LevelState
initialLevelState = LevelState (Kario (Hitbox (0,0) 20 20) (0, 0)) 1