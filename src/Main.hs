module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss (loadBMP)
import System.Directory (listDirectory)
main :: IO ()
main = do sprites <- loadImages
          levels <- loadLevels
          playIO (InWindow "Kario" screenSize (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              (initialState sprites levels)  -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

loadImages :: IO Sprites
loadImages = do kario <- loadBMP "assets\\Kario.bmp"
                ground <- loadBMP "assets\\Ground.bmp"
                brick <- loadBMP "assets\\Brick.bmp"
                questionMark <- loadBMP "assets\\QuestionMarkBlock.bmp"
                brokenQuestionMark <- loadBMP "assets\\BrokenQuestionMarkBlock.bmp"
                coin <- loadBMP "assets\\Coin.bmp"
                coinBling <- loadBMP "assets\\CoinBling.bmp"
                menu <- loadBMP "assets\\StartScreen.bmp"
                levelBox <- loadBMP "assets\\LevelBox.bmp"
                selectionRing <- loadBMP "assets\\SelectionRing.bmp"
                
                return Sprites {
                    karioImage = kario,
                    groundImage = ground,
                    brickImage = brick,
                    questionMarkImage = questionMark,
                    brokenQuestionMarkImage = brokenQuestionMark,
                    coinPictures =  [coin, coinBling],
                    menuImage = menu,
                    levelBoxImage = levelBox,
                    selectionRingImage = selectionRing
                    }

loadLevels :: IO [String]
loadLevels = do
    fileList <- listDirectory "Levels"
    list <- mapM readFile (map ((++) "Levels\\") fileList)
    return (reverse list)



