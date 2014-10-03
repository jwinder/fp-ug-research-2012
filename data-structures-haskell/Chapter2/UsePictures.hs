{-
  Chapter 2 exercise
-}

module UsePictures where
import Pictures

-- A black horse
blackHorse :: Picture
blackHorse = invertColour horse

rotateHorse :: Picture -> Picture
rotateHorse = rotate
