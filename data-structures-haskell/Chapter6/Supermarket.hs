module Supermarket where

type Name    = String
type Price   = Int
type BarCode = Int

type Database = [(BarCode,Name,Price)]

codeIndex :: Database
codeIndex = [(4719, "Fish Fingers", 121),
             (5643, "Nappies", 1010),
             (3814, "Orange Jelly", 56),
             (1111, "Hula Hoops", 21),
             (1112, "Hula Hoops (Giant)", 133),
             (1234, "Dry Sherry, 1lt", 540)]

-- The goal of the script will be to convert a list of BarCodes into a
-- list of pairs (Name,Price), then convert the list of pairs into a string
-- in order to be printed to the screen

type TillType = [BarCode]
type BillType = [(Name,Price)]

-- Functions that we would like to use

--makeBill    :: TillType -> BillType
--formatBill  :: BillType -> String
--produceBill :: TillType -> String

-- A constant

lineLength = 30 :: Int

-- Formatting the bill! 

-- This function isn't 100% correct. it will show 12.2 instead 12.02, so on. Too lazy right now...
formatPence :: Price -> String
formatPence price = show (div price 100) ++ "." ++ show (mod price 100) 

formatLine :: (Name,Price) -> String
formatLine (name,price) = name ++ replicate (lineLength - length name - length strPrice) ' ' ++ strPrice ++ "\n"
  where
    strPrice = formatPence price

formatLines :: [(Name,Price)] -> String
formatLines list = concat [formatLine line | line <- list]

makeTotal :: BillType -> Price
makeTotal bill = sum [p | (n,p) <- bill] 

formatTotal :: Price -> String
formatTotal price = "\nTotal" ++ replicate (lineLength - 5 - length strPrice) ' ' ++ strPrice ++ "\n"
  where
    strPrice = formatPence price

formatBill :: BillType -> String
formatBill bill = formatLines bill ++ formatTotal (makeTotal bill)

-- Bar codes into names and prices

look :: Database -> BarCode -> (Name,Price)
look db code = last ([("Unknown Item",0)] ++ [(n,p) | (c,n,p) <- db, c == code])

lookie :: BarCode -> (Name,Price)
lookie code = look codeIndex code

makeBill :: TillType -> BillType
makeBill till = [lookie code | code <- till]

--function to do both at once

produceBill :: TillType -> String
produceBill = formatBill . makeBill

printBill :: TillType -> IO ()
printBill = putStrLn . produceBill
