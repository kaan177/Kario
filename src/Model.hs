{-# language NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data Sprites = Sprites{
    karioImage :: Picture, 
    groundImage :: Picture
}


data GameState = GameLevel LevelState Sprites | GameMenu MenuState Sprites

data LevelState = LevelState {
    kario :: Kario,
    lilInt :: Int,
    platforms :: [Platform]
    }

data MenuState = MenuState GameName

data Kario = Kario {
    hitbox :: Hitbox
    ,dirVelocity :: DirectionalVelocity
}
data Platform = Ground Hitbox | Brick Hitbox | BreakBrick Hitbox ShouldExist | ItemBox Hitbox PowerUpType | EmptyItemBox Hitbox

data PowerUpType = Mushroom | Star

data Hitbox = Hitbox {
    pos :: Position,
    width :: Width,
    height :: Height
    }  --origin in centre

type Position = Point
type Width = Float
type Height = Float
type DirectionalVelocity = Vector
data ShouldExist = Exist | RemoveIn Int

type GameName = String

initialState :: Sprites -> GameState
initialState = GameMenu (MenuState "Kario")

initialLevelState :: LevelState
initialLevelState = LevelState (Kario (Hitbox (0,0) 20 20) (10, 0)) 1 [Ground (Hitbox (-30,0) 40 40)]