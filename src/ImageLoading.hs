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
                koopaMoving0  <- loadBMP "assets\\KoopaMoving\\Koopa0.bmp"
                koopaMoving1  <- loadBMP "assets\\KoopaMoving\\Koopa1.bmp"
                koopaMoving2  <- loadBMP "assets\\KoopaMoving\\Koopa2.bmp"
                koombaMoving0  <- loadBMP "assets\\KoombaMoving\\Koomba0.bmp"
                koombaMoving1  <- loadBMP "assets\\KoombaMoving\\Koomba1.bmp"
                koombaMoving2  <- loadBMP "assets\\KoombaMoving\\Koomba2.bmp"
                shell         <- loadBMP "assets\\Shell.bmp"
                flagPole      <- loadBMP "assets\\FlagPole.bmp"
                star <- loadBMP "assets\\Star.bmp"
                mushroom <- loadBMP "assets\\Mushroom.bmp"
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
                    koombaMovingImages = [koombaMoving0, koombaMoving1, koombaMoving2],
                    koopaMovingImages = [koopaMoving0, koopaMoving1, koopaMoving2],
                    shellImage = shell,
                    flagPoleImage = flagPole,
                    starImage = star,
                    mushroomImage = mushroom
                    }