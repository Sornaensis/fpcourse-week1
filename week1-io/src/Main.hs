module Main where

sayHello :: String -> IO ()
sayHello s = do
  putStrLn $ "Hello, " ++ s

getName :: IO String
getName = do
  putStrLn "plz enter your name"
  name <- getLine
  let theirName = "The " ++ name
  return theirName

main :: IO ()
main = do
  name <- getName
  sayHello name
