{-# language NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game

module Model where
import Graphics.Gloss

---------------------------------------------------------------
-- | General constants

screenSize :: (Int,Int)
screenSize = (600,600)

----------------------------------------------------------------
-- | Top layer Model

data GameState = GameLevel LevelState Sprites [LevelContents] | GameMenu MenuState Sprites [LevelContents]

data Sprites = Sprites{
    karioImage :: Picture,
    karioWalkingImages :: [Picture],
    karioJumpingImage :: Picture,
    groundImage :: Picture,
    brickImage :: Picture,
    questionMarkImage :: Picture,
    brokenQuestionMarkImage :: Picture,
    coinPictures :: [Picture],
    menuImage :: Picture,
    levelBoxImage :: Picture,
    selectionRingImage :: Picture,
    koombaMovingImages :: [Picture],
    koopaMovingImages :: [Picture],
    shellImage :: Picture,
    flagPoleImage :: Picture,
    starImage :: Picture,
    mushroomImage :: Picture
}

type LevelContents = String

---------------------------------------------------------------------------------------------
-- | General prerequisites for data types in LevelState

type Position = Point
type DirectionalVelocity = Vector
type FrameNr = Int
data ShouldExist = Exist 
                 | RemoveIn Float


data Hitbox = Hitbox {
    pos :: Position,
    width :: Width,
    height :: Height
    }  --origin in centre
type Width = Float
type Height = Float

----------------------------------------------------------------------------------------------
-- | LevelState

data LevelState = LevelState {
    kario           :: Kario,
    platforms       :: [Platform],
    coins           :: [Coin],
    elapsedGameTime :: Float,
    inputState      :: Inputs,
    enemies         :: [Enemy],
    powerups        :: [PowerUp],
    flagPole        :: FlagPole,
    coinLevelScore  :: CoinScore,
    camera          :: Camera
    }

---------------------
---Kario Related-----

data Kario = Kario {
    karHitbox                  :: Hitbox
    ,desiredHorizontalVelocity :: Float
    ,powerUp                   :: PowerUpType
    ,karVel                    :: DirectionalVelocity
    ,airborne                  :: Airborne
    ,karioExist                :: ShouldExist
    ,karAnim                   :: KarioAnimation
}

data Airborne = Grounded | Airborne deriving Eq

data PowerUpType = Big | Invincible | Small

data KarioAnimation = Idle 
                    | Walking FrameNr Float
                    | Jumping 
                    | Dying   FrameNr Float

---Kario Related-----
---------------------

data Platform = Ground Hitbox | Brick Hitbox | BreakBrick Hitbox ShouldExist | ItemBox Hitbox PowerUp | EmptyItemBox Hitbox

---------------------
---Coin related------

data Coin = Coin Hitbox CoinAnimation ShouldExist

data CoinAnimation = Bling FrameNr Float  

---Coin related------
---------------------

type Inputs = [Char] --all keys that are currently down.

data Enemy = Koomba      { enemyBox :: Hitbox, enemyVel :: DirectionalVelocity, enemyExist :: ShouldExist, enemyAnim :: EnemyAnimation }
           | KoopaTroopa { enemyBox :: Hitbox, enemyVel :: DirectionalVelocity, enemyExist :: ShouldExist, enemyAnim :: EnemyAnimation }     
           | KoopaShell  { enemyBox :: Hitbox, enemyVel :: DirectionalVelocity, enemyExist :: ShouldExist, enemyAnim :: EnemyAnimation }

data PowerUp = Mushroom { hitbox :: Hitbox, powerUpvel :: DirectionalVelocity, powerShouldExist :: ShouldExist} 
             | Star {hitbox :: Hitbox, powerUpvel :: DirectionalVelocity, powerShouldExist :: ShouldExist}

data EnemyAnimation = EnemyMoving FrameNr Float

data FlagPole = FlagPole Hitbox

type CoinScore = Int

data Camera = Camera Hitbox DirectionalVelocity

---------------------------------------------------------------------------------------------
-- | MenuState

data MenuState = MenuState {
    selectedLevel :: Maybe Int,
    gameScreen :: GameScreen,
    levelButtons :: [LevelButton],
    selector :: Maybe Selector,
    coinMenuScore :: CoinScore
}

---------------------------------------------------------------------------------------------
-- | UI
data GameScreen = GameScreen Hitbox
data LevelButton = LevelButton Hitbox Int
data Selector = Selector Hitbox

---------------------------------------------------------------------------------------------
-- | Collision

type Overlap = Hitbox
