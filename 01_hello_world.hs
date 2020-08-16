module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  putStrLn ("The sum of the two CLI arguments are: " ++ show (read (args !! 0) + read (args !! 1)))

  putStr "Enter name: "
  name <- getLine
  putStrLn ("Hello, " ++ name)
