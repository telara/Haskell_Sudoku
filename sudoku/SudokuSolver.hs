import System.Environment
import Data.List

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]]
type Sudoku = (Row,Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

centerOfBlocks :: [Int]
centerOfBlocks = [1, 4, 7]

-- Converts a sudoku into a grid.
sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

-- Converts a grid into a sudoku.
grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r, c) = (gr !! (r - 1)) !! (c - 1)

-- Function to extend a sudoku with a value at (row, column).
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if r == i && c == j then v else sud (i, j)

-- Function to get list of values in row of sudoku 
getRow:: Sudoku -> Row -> [Value]
getRow sud r = [sud (r,c) | c <- positions]

-- Function to get list of free values for row of sudoku 
freeInRow:: Sudoku -> Row -> [Value]
freeInRow sud r = values \\ getRow sud r

-- Function to get list of free values for column of sudoku 
getColumn:: Sudoku -> Column -> [Value]
getColumn sud c = [sud (r,c) | r <- positions]

freeInColumn:: Sudoku -> Column -> [Value]
freeInColumn sud c = values \\ getColumn sud c

-- Function to get 3 x 3 square of values from the sudoku (sud) at position (y,x)
getSubgrid :: Sudoku -> (Row,Column) -> [Value]
getSubgrid sud (y,x) = [ sud (r,c) | r <- concat $ filter(elem y)blocks, c <- concat $ filter(elem x)blocks ]

-- Function to get list of free values for block of sudoku 
freeInSubgrid :: Sudoku -> (Row,Column) -> [Value]
freeInSubgrid sud (r,c) = values \\ getSubgrid sud (r,c)

-- combine three functions above to have select 
-- get elem present in all three lists
freeAtPos :: Sudoku -> (Row,Column) -> [Value]
freeAtPos sud (r,c) = intersect (intersect (freeInRow sud r) (freeInColumn sud c)) (freeInSubgrid sud (r,c))

-- return co-ordinates for where the zeroes are.
-- get all values, get all positions, filter only positions of value 0
openPositions :: Sudoku -> [(Row,Column)]
openPositions sud = [(r,c) | r <- positions, c <- positions, sud(r,c) == 0]

alreadyInRow:: Sudoku -> Row -> [Value]
alreadyInRow sud r = filter(/= 0)(getRow sud r)

-- check that there are no duplicates
rowValid :: Sudoku -> Row -> Bool
rowValid sud r = alreadyInRow sud r == nub (alreadyInRow sud r)

-- Read a file-sudoku into a Sudoku
readSudoku :: String -> IO Sudoku
readSudoku filename =
    do stringGrid <- readFile filename
       return $ (grid2sud . splitStringIntoGrid) stringGrid
       where splitStringIntoGrid = map (map readint . words) . lines
             readint x = read x :: Int

-- Prints a Sudoku to the terminal by transforming it to a grid first.
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid

-- Helper to parse command-line arguments.
getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as first argument."
getSudokuName (x:_) = x

-- Do not modify the way your sudoku is printed!
main =
    do args <- getArgs
       sud <- (readSudoku . getSudokuName) args
       -- TODO: Call your solver.
       printSudoku sud
       -- print $ freeInRow sud 9e

grid :: Grid
grid = [
    [5,3,0,0,7,0,0,0,0],
    [6,0,0,1,9,5,0,0,0],
    [0,9,8,0,0,0,0,6,0],
    [8,0,0,0,6,0,0,0,3],
    [4,0,0,8,0,3,0,0,1],
    [7,0,0,0,2,0,0,0,6],
    [0,6,0,0,0,0,2,8,0],
    [0,0,0,4,1,9,0,0,5],
    [0,0,0,0,8,0,0,7,9]]

invalidGrid :: Grid
invalidGrid = [
    [5,3,0,0,7,7,0,0,0],
    [6,0,0,1,9,5,0,0,7],
    [0,9,8,7,0,0,0,6,0],
    [8,0,0,0,6,6,0,0,3],
    [4,0,0,8,0,3,3,0,1],
    [7,0,0,8,2,0,0,0,6],
    [0,6,6,0,0,0,2,8,0],
    [0,0,0,4,1,9,9,0,5],
    [0,0,0,8,8,0,0,7,9]]

{-
runhaskell ./sudoku/SudokuSolver.hs ./sudoku/sudoku_boards/simple_1_open_spot.txt

ghci SudokuSolver.hs
freeInRow (grid2sud grid) 1
-- looks at first row     👆

-- better solution for testing:
sudoku <- readSudoku "sudoku_boards/complete_sudoku.txt"
-- Now sudoku contains the sudoko of the type Sudoku from that file. - didn't work

rowValid (grid2sud invalidGrid) 1
False
rowValid (grid2sud grid) 1
True 

:main sudoku_boards/hard_sudoku_1.txt
-}