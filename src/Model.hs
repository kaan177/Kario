{-# language NamedFieldPuns #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss
import GHC.Float (int2Float)
import Data.Sequence
import Data.Foldable
import GHC.RTS.Flags (ProfFlags)

----------------------------------------------------------------
-- | Model

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

data GameState = GameLevel LevelState Sprites [LevelContents] | GameMenu MenuState Sprites [LevelContents]

type Inputs = [Char] --all keys that are currently down.

type CoinScore = Int

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

data MenuState = MenuState {
    selectedLevel :: Maybe Int,
    gameScreen :: GameScreen,
    levelButtons :: [LevelButton],
    selector :: Maybe Selector,
    coinMenuScore :: CoinScore
}

type Position = Point
type Width = Float
type Height = Float
type DirectionalVelocity = Vector
type DirectionalAcceleration = Vector
data ShouldExist = Exist 
                 | RemoveIn Float

data Kario = Kario {
    karHitbox   :: Hitbox
    ,desiredHorizontalVelocity :: Float
    ,powerUp    :: PowerUpState
    ,invincibleState :: InvincibleState
    ,karVel     :: DirectionalVelocity
    ,karAccel   :: DirectionalAcceleration
    ,airborne   :: Airborne
    ,karioExist :: ShouldExist
    ,karAnim    :: KarioAnimation
}

type FrameNr = Int

data KarioAnimation = Idle 
                    | Walking FrameNr Float
                    | Jumping 
                    | Dying   FrameNr Float

data Camera = Camera Hitbox DirectionalVelocity

data Enemy = Koomba      { enemyBox :: Hitbox, enemyVel :: DirectionalVelocity, enemyExist :: ShouldExist, enemyAnim :: EnemyAnimation }
           | KoopaTroopa { enemyBox :: Hitbox, enemyVel :: DirectionalVelocity, enemyExist :: ShouldExist, enemyAnim :: EnemyAnimation }     
           | KoopaShell  { enemyBox :: Hitbox, enemyVel :: DirectionalVelocity, enemyExist :: ShouldExist, enemyAnim :: EnemyAnimation }

data EnemyAnimation = EnemyMoving FrameNr Float

data Platform = Ground Hitbox | Brick Hitbox | BreakBrick Hitbox ShouldExist | ItemBox Hitbox PowerUp | EmptyItemBox Hitbox

data PowerUpState = Big | Small
data InvincibleState = Invincible Float | Vulnerable
data PowerUp = Mushroom {hitbox :: Hitbox, powerUpvel :: DirectionalVelocity, powerShouldExist :: ShouldExist} | Star {hitbox :: Hitbox, powerUpvel :: DirectionalVelocity, powerShouldExist :: ShouldExist}

data Coin = Coin Hitbox CoinAnimation ShouldExist

data FlagPole = FlagPole Hitbox

data Hitbox = Hitbox {
    pos :: Position,
    width :: Width,
    height :: Height
    }  --origin in centre

type Overlap = Hitbox

data Airborne = Grounded | Airborne deriving Eq

data CoinAnimation = Bling | Collecting Float

gridSize :: Float
gridSize = 30
type GameName = String

initialState :: Sprites -> [String] -> CoinScore -> GameState
initialState s l c = GameMenu (initialMenuState l c) s l 

initialMenuState :: [String] -> CoinScore -> MenuState
initialMenuState l c =
    let selectedLevelInt = if Prelude.null l then Nothing
            else Just 0 in
    let selectedLevelObject = if Prelude.null l then Nothing
            else Just generateSelector in
    MenuState{
    selectedLevel = selectedLevelInt,
    gameScreen = GameScreen $ Hitbox ((\(x,y) -> (0, 0)) screenSize) 0 0,
    levelButtons = generateLevelButtons l,
    selector = selectedLevelObject,
    coinMenuScore = c
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