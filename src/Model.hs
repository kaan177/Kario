{-# language NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss
import GHC.Float (int2Float)
import Data.Sequence
import Data.Foldable

data Sprites = Sprites{
    karioImage :: Picture,
    groundImage :: Picture,
    brickImage :: Picture,
    questionMarkImage :: Picture,
    brokenQuestionMarkImage :: Picture,
    coinPictures :: [Picture],
    menuImage :: Picture,
    levelBoxImage :: Picture,
    selectionRingImage :: Picture
}

data LoadedLevels = LoadedLevels [String]

data GameState = GameLevel LevelState Sprites LoadedLevels | GameMenu MenuState Sprites LoadedLevels

data LevelState = LevelState {
    kario :: Kario,
    platforms :: [Platform],
    coins :: [Coin],
    elapsedGameTime :: Float
    }

data MenuState = MenuState {
    loadedLevels :: LoadedLevels,
    selectedLevel :: Maybe Int,
    gameScreen :: GameScreen,
    levelButtons :: [LevelButton],
    selector :: Maybe Selector
}

type Position = Point
type Width = Float
type Height = Float
type DirectionalVelocity = Vector
type DirectionalAcceleration = Vector
data ShouldExist = Exist | RemoveIn Int

data Kario = Kario {
    hitbox :: Hitbox
    ,desiredHorizontalVelocity :: Float
    ,dirVelocity :: DirectionalVelocity
    ,dirAccel :: DirectionalAcceleration
    ,airborne :: Airborne
}
data Platform = Ground Hitbox | Brick Hitbox | BreakBrick Hitbox ShouldExist | ItemBox Hitbox PowerUpType | EmptyItemBox Hitbox

data PowerUpType = Mushroom | Star

data Coin = Coin Hitbox CoinAnimation ShouldExist

data Hitbox = Hitbox {
    pos :: Position,
    width :: Width,
    height :: Height
    }  --origin in centre

data Airborne = Grounded | Falling | Rising

data CoinAnimation = Bling | Collecting Float

gridSize :: Float
gridSize = 30
type GameName = String

initialState :: Sprites -> [String] -> GameState
initialState s l = GameMenu (initialMenuState l) s (LoadedLevels l)

initialMenuState :: [String] -> MenuState
initialMenuState l =
    let selectedLevelInt = if Prelude.null l then Nothing
            else Just 0 in
    let selectedLevelObject = if Prelude.null l then Nothing
            else Just generateSelector in
    MenuState{
    loadedLevels = LoadedLevels l,
    selectedLevel = selectedLevelInt,
    gameScreen = GameScreen $ Hitbox ((\(x,y) -> (0, 0)) screenSize) 0 0,
    levelButtons = generateLevelButtons l,
    selector = selectedLevelObject
}

initialLevelState :: LevelState
initialLevelState = LevelState {
  kario = Kario (Hitbox (0,0) 20 20) 0 (10, 0) (0,0) Grounded,
  platforms = [Ground (Hitbox (0,(-1) * gridSize) 30 30), Brick (Hitbox (0, 4 * gridSize) 30 30), ItemBox (Hitbox (1 * gridSize, 4 * gridSize) 30 30) Mushroom, EmptyItemBox (Hitbox (2 * gridSize, 4 * gridSize) 30 30)] ,
  coins = [Coin (Hitbox (0 * gridSize, 0 * gridSize) 30 30) Bling Exist , Coin (Hitbox (6 * gridSize, 4 * gridSize) 30 30) Bling Exist , Coin (Hitbox (7 * gridSize, 4 * gridSize) 30 30) Bling Exist],
  elapsedGameTime = 0
  }

screenSize :: (Int,Int)
screenSize = (600,600)

----------------------------------------UI STUFF------------------------
data GameScreen = GameScreen Hitbox
data LevelButton = LevelButton Hitbox Int
data Selector = Selector Hitbox

buttonSize :: Float
buttonSize = 60

buttonPadding :: Float
buttonPadding = 30

buttonStartPosition :: (Float,Float)
buttonStartPosition = (\(x,y) -> ((int2Float x)/4 - (int2Float x)/2,(int2Float y)/2 - (int2Float y)/2)) screenSize

buttonAmountX :: Float
buttonAmountX = 4

buttonAmountY :: Float
buttonAmountY = 2

generateLevelButtons :: [String] -> [LevelButton]
generateLevelButtons l = toList (mapWithIndex (\i _-> LevelButton (Hitbox (buttonPosition i) buttonSize buttonSize) i ) (fromList l))

generateSelector :: Selector
generateSelector = Selector (Hitbox buttonStartPosition 0 0)

buttonPosition :: Int -> (Float, Float)
buttonPosition i = (\(x,y) i -> (x + ((buttonSize + buttonPadding) * int2Float(mod (floor i) (floor buttonAmountX))), y - ((buttonSize + buttonPadding) * int2Float(mod (floor (i/buttonAmountX)) (floor buttonAmountY))))) buttonStartPosition (int2Float i)