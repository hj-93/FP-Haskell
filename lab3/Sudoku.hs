module Sudoku where

import Test.QuickCheck
import Test.QuickCheck.Modifiers
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
isOkayBlock x = ( removedNothing + (length $ nub x) ) == 9
                  where removedNothing | Nothing `elem` x = count Nothing x - 1
                                       | otherwise        = 0


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
blanks s = blanksHelper (rows s)

blanksHelper :: [[Cell]] -> [(Int, Int)]
blanksHelper []     = [] 
blanksHelper (x:xs) = zip (repeat (8 - length xs)) (elemIndices Nothing x) ++
                      blanksHelper xs

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length (blanks allBlankSudoku) == 81


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
[] !!= (i,y) = [] 
xs !!= (i,y) = part1 ++ [y] ++ part2
               where (part1, _:part2) = splitAt i xs

prop_bangBangEquals_correct :: [Cell]  -> (Int,Cell) -> Bool
prop_bangBangEquals_correct [] (i, a) = [] !!= (abs i, a) == []
prop_bangBangEquals_correct xs (i, a) = part1 == part1' &&
                                        part2 == part2' &&
                                        new   == a
                                          where (part1 ,   _:part2)  = splitAt pos xs
                                                (part1', new:part2') = splitAt pos (xs !!= (pos, a))
                                                pos                  = abs i `mod` (length xs) 


-- * E3
update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku allRows) pos cell = Sudoku (updateNthRow allRows pos cell)

updateNthRow :: [Block] -> Pos -> Cell -> [Block]
updateNthRow (x:xs) (r, c) cell | length xs == (8 - r) = x !!= (c, cell) : xs
                                | otherwise            = x : updateNthRow xs (r, c) cell

prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated s (r, c) cell = ((rows (update s (r', c') cell)) !! r') !! c' == cell
                                      where r' = r `mod` 8
                                            c' = c `mod` 8


------------------------------------------------------------------------------

-- * F1
solve :: Sudoku -> Maybe Sudoku
solve s = listToMaybe $ catMaybes $ solve' (blanks s) [s]

solve' :: [Pos] -> [Sudoku] -> [Maybe Sudoku]
solve' _ []                  = []
solve' [] (s:[])
  | isOkay s                 = [Just s]
  | otherwise                = [Nothing]
solve' (p:ps) (s:[])         = solve' ps s1' ++ solve' ps s2'
                                 where (s1', s2') = case res of
                                                      []      -> ([], [])
                                                      (s1:s2) -> ([s1], s2)
                                       res        = sFilter (map (update s p) (map Just [1..9]))
solve' ps (s:ss)             = solve' ps [s]  ++ solve' ps ss


sFilter :: [Sudoku] -> [Sudoku]
sFilter []                 = []
sFilter (s:ss)| isOkay s   = s : sFilter ss
              | otherwise  = sFilter ss

-- * F2
readAndSolve :: FilePath -> IO ()
readAndSolve  file = do
                       s <- readSudoku file
                       let maybeSudoku = solve s
                       if maybeSudoku == Nothing
                         then putStr "(no solution)\n"
                         else printSudoku (fromJust maybeSudoku)

-- * F3
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf slt s = isFilled slt &&
                     isOkay slt   &&
                     updateToNothing slt (blanks s) == s
                       where updateToNothing s []     = s
                             updateToNothing s (x:xs) = updateToNothing (update s x Nothing) xs

-- * F4
prop_SolveSound :: Sudoku -> Property
prop_SolveSound s = isOkay s && isJust (solve s) ==> fromJust (solve s) `isSolutionOf` s

fewerChecks prop =
  quickCheckWith stdArgs{maxSuccess=30 } prop
