module ImageLoading where

import Model
import Graphics.Gloss (loadBMP)

loadImages :: IO Sprites
loadImages = do kario <- loadBMP "assets\\Kario.bmp"
                karioWalking0 <- loadBMP "assets\\KarioWalking\\Kario0.bmp"
                karioWalking1 <- loadBMP "assets\\KarioWalking\\Kario1.bmp"
                karioWalking2 <- loadBMP "assets\\KarioWalking\\Kario2.bmp"
                karioJumping  <- loadBMP "assets\\KarioJumping.bmp"
                ground        <- loadBMP "assets\\Ground.bmp"
                brick         <- loadBMP "assets\\Brick.bmp"
                questionMark  <- loadBMP "assets\\QuestionMarkBlock.bmp"
                brokenQuestionMark <- loadBMP "assets\\BrokenQuestionMarkBlock.bmp"
                coin          <- loadBMP "assets\\Coin.bmp"
                coinBling     <- loadBMP "assets\\CoinBling.bmp"
                menu          <- loadBMP "assets\\StartScreen.bmp"
                levelBox      <- loadBMP "assets\\LevelBox.bmp"
                selectionRing <- loadBMP "assets\\SelectionRing.bmp"
                koomba        <- loadBMP "assets\\Koomba.bmp"
                koopa         <- loadBMP "assets\\Koopa.bmp"
                shell         <- loadBMP "assets\\Shell.bmp"
                flagPole      <- loadBMP "assets\\FlagPole.bmp"
                return Sprites {
                    karioImage = kario,
                    karioWalkingImages = [karioWalking0, karioWalking1, karioWalking2],
                    karioJumpingImage = karioJumping, 
                    groundImage = ground,
                    brickImage = brick,
                    questionMarkImage = questionMark,
                    brokenQuestionMarkImage = brokenQuestionMark,
                    coinPictures =  [coin, coinBling],
                    menuImage = menu,
                    levelBoxImage = levelBox,
                    selectionRingImage = selectionRing,
                    koombaImage = koomba,
                    koopaImage = koopa,
                    shellImage = shell,
                    flagPoleImage = flagPole
                    }