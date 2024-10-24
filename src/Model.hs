{-# language NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data Sprites = Sprites{
    karioImage :: Picture
}

data GameState = GameLevel LevelState Sprites | GameMenu MenuState Sprites

data LevelState = LevelState {
    kario :: Kario,
    lilInt :: Int
    }

data MenuState = MenuState GameName

type Position = Point
type Width = Float
type Height = Float
type DirectionalVelocity = Vector
type DirectionalAcceleration = Vector

data Kario = Kario {
    hitbox :: Hitbox
    ,dirVelocity :: DirectionalVelocity
    ,dirAccel :: DirectionalAcceleration
    ,airborne :: Airborne
}
data Hitbox = Hitbox {
    pos :: Position,
    width :: Width,
    height :: Height
    }  --origin in centre
data Airborne = Grounded | Falling | Rising

type GameName = String

initialState :: Sprites -> GameState
initialState = GameMenu (MenuState "Kario")

initialLevelState :: LevelState
initialLevelState = LevelState (Kario (Hitbox (0,0) 20 20) (10, 0) (0,0) Grounded) 1