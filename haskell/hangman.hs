-- | The hangman game
-- |
-- | 1. One player secretly types in a word.
-- | 2. The other player tries to deduce the word, by
-- |    entering a sequence of guesses.
-- | 3. For each guess, the computer indicates which
-- |    letters in the secret word occur in the guess.

import System.IO

-- Top-down approach
hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

-- | Read a line of text from the keyboard, echoing each character as a dash
sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

-- | Read a single character from the keyboard, without echoing it to the screen
getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-- | Request and process guesses until the game ends
play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                 putStrLn "You got it!"
               else
                 do putStrLn (match word guess)
                    play word

-- | Indicate which characters in one string occur in a second string
match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]

main :: IO ()
main = hangman
