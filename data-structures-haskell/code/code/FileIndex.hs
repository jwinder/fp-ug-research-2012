
module FileIndex where

{- helium changes
readFile and writeFile not defined.
->

import Index

main = do
	inputText <- readFile "IndexInput.txt"
	writeFile "IndexOutput.txt" (show (makeIndex inputText))
