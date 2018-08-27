-- {-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module DateTime (DateTime(..), DTime(Time, Date), toTime, format) where

import Data.Char

class DateTime a where 
    showDTime :: a -> String
    (<->) :: a -> a -> a 
    (<+>) :: a -> a -> a 

data DTime = Time Int Int 
          | Date Int Int Int
          deriving (Show, Eq, Ord)

instance DateTime DTime where 
    showDTime = showDTime' . format 
        where 
            showDTime' (Time a b) = show a ++ ":" ++ show b
            showDTime' (Date a b c) = undefined

    (Time a b) <+> (Time c d) = format $ uncurry Time added
        where 
            added = (60*(a + c)+ b + d) `divMod` 60

    (Date a b c) <+> (Date d e f) = undefined

    (Time a b) <-> (Time c d) = format $ uncurry Time subtracted
        where 
            subtracted = (60*(a - c) + b - d) `divMod` 60

    (Date a b c) <-> (Date d e f) = undefined

toTime :: String -> Maybe DTime
toTime str 
  | all isDigit (hours ++ minutes) && hours /= "" && minutes /= "" 
  = Just . format $ uncurry Time (read hours, read minutes)
  | otherwise = Nothing
    where 
        hours = takeWhile (/=':') str
        minutes = safeTail $ dropWhile (/=':') str

safeTail :: [a] -> [a]
safeTail xs = if null xs then [] else tail xs

format :: DTime -> DTime
format (Time a b) = Time ((60*a + b `div` 60) `mod` 24) (b `mod` 60)
format (Date a b c) = undefined
