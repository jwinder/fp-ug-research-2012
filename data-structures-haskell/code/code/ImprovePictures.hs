module ImprovePictures where

import Pictures

{- helium changes
Have to give a module declaration
replace toEnum (with range Char) by chr
-}

improve :: Picture -> Picture
improve p = map (map change) p

ws :: Char
ws = chr 219

bs :: Char 
bs = chr 177

change :: Char -> Char
change '.' = ws
change '#' = bs

main :: IO ()
main = printPicture (improve horse)

