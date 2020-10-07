module Main where

-- Warm up Exercises
-- Write out the body of the functions below so they perform as indicated.
-- You may need to write multiple `func param = ...` lines to make it work,
-- especially if pattern matching is involved.
-- If the function takes a parameter then you will need to add it on the left hand side of the equals sign

-- Use stack ghci to test your definitions and check types with
-- :t func
-- To reload the Main.hs file when edited, run  
-- :r  
-- in ghci
{- list function reference

head    :: [a] -> a
!!      :: [a] -> Int -> a
filter  :: (a -> Bool) -> [a] -> [a]

-}
-- List of all Odd Integers >0
allOdds :: [Integer]
allOdds = [1,3..]
-- Return the nth odd integer using the list above
nthOdd :: Int -> Integer
nthOdd n = allOdds !!  n
-- Using the Color datatype defined here...
data Color = Red | Blue | Green deriving (Show,Eq)
-- Write a function that uses pattern matching to return the correct color triplet for the color
-- The ONLY colors that need to work are the ones defined in the datatype
-- A color triplet is a parentheses enclosed list, written like so: ( 1.0, 0.0, 0.0 ) :: ( Float, Float, Float ) 
-- where each corresponds to the amount of Red, Green, and Blue from 0-1
--  e.g. (1.0,0.0,0.0) means all red, no green no blue
colorTriplet :: Color -> (Float, Float, Float)
colorTriplet Red = (1.0, 0.0, 0.0)
colorTriplet Green = (0.0, 1.0, 0.0)
colorTriplet Blue = (0.0, 0.0, 1.0)

main :: IO ()
main = return () 
