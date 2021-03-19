{-# LANGUAGE OverloadedStrings #-}

module Compute where
import           Data.Foldable (foldl')
import           Data.List     (transpose)
import qualified Data.Text     as T

addAll :: Num a => [[a]] -> Either T.Text [a]
addAll [] = Left "Empty list. Borked data probably."
addAll [x] = Right x
addAll ls =
    let lengths = map length ls
        z = length . head $ ls
        go = foldl' (zipWith (+)) (filler [] z) ls
    in  if null go || minimum lengths /= maximum lengths
        then Left "Empty result due to missing or corrupted data."
        else Right go
    where   filler l end = if length l < end then filler (0:l) end else l

addAll' :: Num a => [[a]] -> Either T.Text [a]
addAll' [] = Left "Empty list. Borked data probably."
addAll' [x] = Right x
addAll' ls =
    let lengths = map length ls
        go = map sum $ transpose ls
    in  if minimum lengths /= maximum lengths then Left "Empty list. Borked data probably."
        else Right go

main = do
    let x = [[0,1],[0,0],[1,0],[1,1]]
        y = [[0,1],[0,0],[0],[1,1]]
    print $ addAll x
    print $ addAll x
    print $ addAll' y
    print $ addAll' y
