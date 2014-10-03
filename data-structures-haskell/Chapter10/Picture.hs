module Picture where

import Char

f .> g = g . f -- reverse composition function

type Picture = [[Char]]


flipH :: Picture -> Picture
flipH = reverse

flipV :: Picture -> Picture
flipV = map reverse

above :: Picture -> Picture -> Picture
above = (++)

sideBySide :: Picture -> Picture -> Picture
sideBySide = zipWith (++)

invertChar :: Char -> Char
invertChar x = if x=='#' then '.' else '#'

invertColour :: Picture -> Picture
invertColour = map (map invertChar)

combineChar :: Char -> Char -> Char
combineChar a b = if a=='#' || b=='#' then '#' else '.'

superimpose :: Picture -> Picture -> Picture
superimpose = zipWith (zipWith combineChar)

printPicture :: Picture -> IO ()
printPicture = map (++"\n") .> concat .> putStr

--chessBoard :: Int -> Picture
--chessBoard n = replicate n (replicate n '#')
