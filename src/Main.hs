module Main where

import Controller
import Model
import View
import ImageLoading

import Graphics.Gloss.Interface.IO.Game
import System.Directory (listDirectory)
import Utils.Containers.Internal.StrictPair (StrictPair)

main :: IO ()
main = do sprites <- loadImages
          levels <- loadLevels
          coinScore <- loadCoins
          playIO (InWindow "Kario" screenSize (0, 0)) -- Or FullScreen
              (makeColor (14/255) (120/255) (224/255) 1)            -- Background color
              60               -- Frames per second
              (initialState sprites levels coinScore)  -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function



loadLevels :: IO [String]
loadLevels = do
    fileList <- listDirectory "Levels"
    list <- mapM (readFile . ("Levels\\" ++)) fileList
    return (reverse list)

loadCoins :: IO CoinScore
loadCoins = do 
    coinString <- readFile "Coins\\Coins.txt" 
    return (stringToCoinScore coinString)

stringToCoinScore :: String -> CoinScore
stringToCoinScore [] = 0
stringToCoinScore s = read s
