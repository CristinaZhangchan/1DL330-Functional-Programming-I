-- Main.hs
import SudokuSolver (Board, Solutions(..), numSolutions)
import System.IO (readFile)
import Data.List.Split (splitOn)

-- The main function that reads a board and solves it
main :: IO ()
main = do
    putStrLn "Enter the path of the Sudoku file:"
    filePath <- getLine
    content <- readFile filePath
    let board = parseBoard content
    putStrLn "Parsed board:"
    mapM_ print board  -- Print the parsed board for debugging
    let result = numSolutions board
    putStrLn "Solution result is being calculated..."  -- Add a message to track the flow
    print result  -- Print the result of numSolutions
    putStrLn "Done!"

-- Function to parse the Sudoku board from a text file
parseBoard :: String -> Board
parseBoard content =
    let rows = lines content
        boardLines = tail rows  -- If first line is the size indicator like '3'
    in map (map read . splitOn ",") boardLines
