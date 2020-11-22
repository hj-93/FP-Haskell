module Sudoku where

import Test.QuickCheck
import Data.List
import Data.Maybe
import Data.Char
import Data.List.Split
------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku [ [Nothing | i <- [1..9]] | i <- [1..9] ]

-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = length s_rows == 9 &&
             and [ isRowOk row | row <- s_rows ]
             where s_rows = rows s
                   isRowOk r = length r == 9 && and [ fromMaybe 1 c `elem` [1..9 ] | c <- r]

-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s = isSudoku s &&
             and [Nothing `notElem` row | row <- s_rows]
               where s_rows = rows s 

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr $ concat [ trRow row | row <- s_rows ]
                     where s_rows = rows s
                           trCell Nothing  = "."
                           trCell (Just n) = show n
                           trRow []        = "\n"
                           trRow (x:xs)    = trCell x ++ trRow xs

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku path = do
                    content <- readFile path
                    let sudoku = Sudoku [lineToRow line | line <- lines content]
                    if isSudoku sudoku
                       then return sudoku
                       else error "Not a Sudoku!"

charToCell :: Char -> Cell
charToCell c | c == '.'  = Nothing
             | otherwise = Just (digitToInt c)

lineToRow :: String -> Row
lineToRow [] = []
lineToRow x  = map charToCell x

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency
         [(9, return Nothing),
          (1, do n <- choose (1,9)
                 return (Just n))]


-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do Sudoku <$> vectorOf 9 (vectorOf 9 cell)

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sudoku = isSudoku sudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

isOkayBlock :: Block -> Bool
isOkayBlock x = ( count Nothing x - 1 + (length $ nub x) ) == 9


-- * D2

blocks :: Sudoku -> [Block]
blocks s = (rows s) ++ (transpose (rows s)) ++ (squareBlock s)

squareBlock :: Sudoku -> [Block]
squareBlock s = map tupleToList (merge zippedChunk)
             where
               splittedChunks = chunksOf 3 [ chunksOf 3 row | row <- rows s]
               zippedChunk = [zip3 (chunk !! 0) (chunk !! 1) (chunk !! 2) | chunk <- splittedChunks]

merge :: [[a]] -> [a]
merge []     = []
merge (x:xs) = x ++ merge xs

tupleToList :: ([a],[a],[a]) -> [a]
tupleToList (a, b, c)  =  a ++ b ++ c


prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths s = length (blocks s) == 27 &&
                         and [length b == 9 | b <- blocks s]

-- * D3

isOkay :: Sudoku -> Bool
isOkay s = and [isOkayBlock b | b <- blocks s]


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
