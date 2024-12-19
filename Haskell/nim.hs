-- | The nim game
-- |
-- | 1. The board comprises five rows of stars
-- | 2. Two players take it turn about to remove one
-- |    or more stars from the end of a single row
-- | 3. The winner is the player who removes the last star of
-- |    stars from the board.

import Control.Monad
import Data.Char

-- | Board utilities
type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished b = all (== 0) b

valid :: Board -> Int -> Int -> Bool
valid b row num = b !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move b row num = [adjust r n | (r, n) <- zip [1 .. 5] b]
  where
    adjust r n = if r == row then n - num else n

-- | I/O utilities

newline :: IO ()
newline = putChar '\n'

stars :: Int -> String
stars n = concat (replicate n "* ")

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (stars num)

putBoard :: Board -> IO ()
putBoard b = mapM_ (\(row, num) -> putRow row num) (zip [1..5] b)

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                       return (digitToInt x)
                     else
                       do newline
                          putStrLn "ERROR: Invalid Digit!"
                          getDigit prompt

-- | Nim game
nim :: IO ()
nim = play initial 1

next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board -> Int -> IO ()
play board player =
  do newline
     putBoard board
     if finished board then
       do newline
          putStr "Player "
          putStr (show (next player)) -- if player 1 got empty board, that's player 2 won.
          putStrLn " wins!"
     else
       do newline
          putStr "Player "
          putStrLn (show player)
          r <- getDigit "Enter a row number: "
          n <- getDigit "Enter stars to remove: "
          if valid board r n then
            play (move board r n) (next player)
          else
            do newline
               putStrLn "ERROR: Invalid Move!"
               play board player

main :: IO ()
main = nim
