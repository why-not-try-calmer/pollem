{-# LANGUAGE OverloadedStrings #-}

module Compute where
import           Data.Foldable (foldl')
import           Data.List     (transpose)
import qualified Data.Text     as T

addAll :: (Num a, Eq a) => [[a]] -> Either T.Text [a]
addAll [] = Left "Empty list. Borked data probably."
addAll [x] = Right x
addAll ls =
    let (x:xs) = ls
        go = map sum $ transpose ls
    in  if any (/= x) xs then Left "Empty list. Borked data probably."
        else Right go
