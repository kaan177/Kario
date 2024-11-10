module Animation(updateAnimation, frameDuration) where

import Model 

--constant for animation in general
animationFPS :: Float
animationFPS = 10

frameDuration :: Float
frameDuration = 1 / animationFPS

--constants for kario animation

class Animatable a where
    getFrame         :: a -> FrameNr
    setFrame         :: FrameNr -> a -> a
    getNrOfFrames    :: a -> Int
    getTimeUntilNext :: a -> Float
    setTimeUntilNext :: Float -> a -> a 

instance Animatable Kario where
    getFrame Kario{karAnim = Idle}         = 0
    getFrame Kario{karAnim = Walking nr _} = nr
    getFrame Kario{karAnim = Jumping}      = 0
    getFrame Kario{karAnim = Dying nr _}   = nr
    setFrame _ k@Kario{karAnim = Idle}         = k
    setFrame nr k@Kario{karAnim = Walking _ t} = k{karAnim = Walking nr t}
    setFrame _ k@Kario{karAnim = Jumping}      = k
    setFrame nr k@Kario{karAnim = Dying _ t}   = k{karAnim = Walking nr t}
    getNrOfFrames Kario{karAnim = Idle}        = 0
    getNrOfFrames Kario{karAnim = Walking _ _} = 4
    getNrOfFrames Kario{karAnim = Jumping}     = 0
    getNrOfFrames Kario{karAnim = Dying _ _}   = 3
    getTimeUntilNext Kario{karAnim = Idle}        = 0
    getTimeUntilNext Kario{karAnim = Walking _ t} = t
    getTimeUntilNext Kario{karAnim = Jumping}     = 0
    getTimeUntilNext Kario{karAnim = Dying _ t}   = t
    setTimeUntilNext _ k@Kario{karAnim = Idle}         = k
    setTimeUntilNext t k@Kario{karAnim = Walking nr _} = k{karAnim = Walking nr t}
    setTimeUntilNext _ k@Kario{karAnim = Jumping}      = k
    setTimeUntilNext t k@Kario{karAnim = Dying nr _}   = k{karAnim = Walking nr t}
    

nextFrame ::(Animatable a) => Int -> a -> a
nextFrame nrOfFrames obj | curFrame < (nrOfFrames - 1) = setFrame (curFrame + 1) obj --go to next frame
                         | otherwise                   = setFrame 0 obj              --loop back around
    where curFrame = getFrame obj

updateTimeUntilNext :: (Animatable a) => Float -> a -> a
updateTimeUntilNext secs obj = setTimeUntilNext (curTime - secs) obj
    where curTime = getTimeUntilNext obj

updateAnimation :: (Animatable a) => Float -> a -> a
updateAnimation secs obj | curTime <= 0 = nextFrame nrOfFrames . setTimeUntilNext (frameDuration + curTime) $ obj
                         | otherwise    = updateTimeUntilNext secs obj
    where curTime = getTimeUntilNext obj
          nrOfFrames = getNrOfFrames obj
          

