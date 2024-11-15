module SudokuSolver(Board, Solutions(..), author, nickname, numSolutions) where

import Sudoku(Board, Solutions(..))

author :: String
author = "Cristina Zhang"

nickname :: String
nickname = "Sudoku"

{- numSolutions board
   Given a Sudoku board, this function returns whether the board has 0, 1 or multiple solutions.
   PRE: The board must be a valid n^2 x n^2 Sudoku board (with empty cells represented by 0).
   RETURNS: NoSolution if the board has no solutions, UniqueSolution if it has exactly one solution,
            or MultipleSolutions if it has more than one solution.
   SIDE EFFECTS: None.
   EXAMPLES:
     numSolutions [[5, 3, 0, 0, 7, 0, 0, 0, 0], ... ] == UniqueSolution
     numSolutions [[5, 5, 0, 0, 7, 0, 0, 0, 0], ... ] == NoSolution
-}
numSolutions :: Board -> Solutions
numSolutions board =
  let solutions = findSolutions board 0
  in case solutions of
       0 -> NoSolution
       1 -> UniqueSolution
       _ -> MultipleSolutions

-- Helper function to count solutions using backtracking
findSolutions :: Board -> Int -> Int
findSolutions board maxSolutions = 
  let emptyCell = nextEmpty board
  in case emptyCell of
       Nothing -> 1  -- Base case: board is fully solved
       Just (r, c) -> sum [testPlacement board r c n maxSolutions | n <- [1..9]]

-- Test placing a number in a cell and recurse
testPlacement :: Board -> Int -> Int -> Int -> Int -> Int
testPlacement board r c n maxSolutions
  | not (isValidMove board r c n) = 0
  | otherwise =
    let newBoard = placeNumber board r c n
        numFound = findSolutions newBoard maxSolutions
    in if numFound > maxSolutions then maxSolutions + 1 else numFound

-- Find the next empty cell in the board (represented by 0)
nextEmpty :: Board -> Maybe (Int, Int)
nextEmpty board = case [(r, c) | r <- [0..8], c <- [0..8], board !! r !! c == 0] of
                    [] -> Nothing
                    (x:_) -> Just x

-- Place a number in the board at position (r, c) and return the new board
placeNumber :: Board -> Int -> Int -> Int -> Board
placeNumber board r c n =
  take r board ++ [take c (board !! r) ++ [n] ++ drop (c + 1) (board !! r)] ++ drop (r + 1) board

-- Check if placing a number n at position (r, c) is valid
isValidMove :: Board -> Int -> Int -> Int -> Bool
isValidMove board r c n = not (n `elem` getRow board r || n `elem` getCol board c || n `elem` getBlock board r c)

-- Get the row from the board
getRow :: Board -> Int -> [Int]
getRow board r = board !! r

-- Get the column from the board
getCol :: Board -> Int -> [Int]
getCol board c = [board !! r !! c | r <- [0..8]]

-- Get the 3x3 block from the board
getBlock :: Board -> Int -> Int -> [Int]
getBlock board r c =
  let startRow = (r `div` 3) * 3
      startCol = (c `div` 3) * 3
  in [board !! (startRow + i) !! (startCol + j) | i <- [0..2], j <- [0..2]]
