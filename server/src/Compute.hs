{-# LANGUAGE OverloadedStrings #-}

module Compute where
import           Data.Foldable (foldl')
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
