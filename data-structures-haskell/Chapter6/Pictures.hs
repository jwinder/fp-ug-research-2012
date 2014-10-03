{-
  None of these are tested
-}

module Pictures where

type Picture = [[Char]]

flipH :: Picture -> Picture
flipH = reverse

above :: Picture -> Picture -> Picture
above = (++)

flipV :: Picture -> Picture
flipV pic = [reverse line | line <- pic]

sideBySide :: Picture -> Picture -> Picture
--sideBySide picL picR = [lineL ++ lineR | lineL <- PicL, lineR <- PicR]
sideBySide picL picR = [lineL ++ lineR | (lineL,lineR) <- zip picL picR]

invertChar :: Char -> Char
invertChar ch = if ch=='.' then '#' else '.'

invertLine :: [Char] -> [Char]
invertLine line = [invert ch | ch <- line]

invertColour :: Picture -> Picture
invertColour pic = [invertLine line | line <- pic]
--invertColour pic = [[invertChar ch | ch <- line] | line <- pic]

superimposeChar :: Char -> Char -> Char
superimposeChar a b = if a == '.' && b == '.' then '.' else '#'

superimposeLine :: [Char] -> [Char] -> [Char]
superimposeLine line1 line2 = [superimposeChar char1 char2 | char1 <- line1, char2 <- line2]

superimpose :: Picture -> Picture -> Picture
superimpose pic1 pic2 = [superimposeLine line1 line2 | line1 <- pic1, line2 <- pic2]

printPicture :: Picture -> IO ()
printPicture pic = [putStrLn line | line <- pic]

rotate90 :: Picture -> Picture
rotate90 pic = [concat (reverse (last (splitAt i line))) | line <- pic, i <- [1..length pic]]
