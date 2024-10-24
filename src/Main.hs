module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss (loadBMP)
main :: IO ()
main = do sprites <- loadImages
          playIO (InWindow "Kario" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              30               -- Frames per second
              (initialState sprites)  -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function

loadImages :: IO Sprites
loadImages = do kario <- loadBMP "assets\\Kario.bmp"
                ground <- loadBMP "assets\\Ground.bmp"
                brick <- loadBMP "assets\\Brick.bmp"
                questionMark <- loadBMP "assets\\QuestionMarkBlock.bmp"
                brokenQuestionMark <- loadBMP "assets\\BrokenQuestionMarkBlock.bmp"
                return (Sprites kario ground brick questionMark brokenQuestionMark)

