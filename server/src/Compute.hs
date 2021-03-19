{-# LANGUAGE OverloadedStrings #-}

module Compute where

import           Control.Exception
import           Data.Foldable     (foldl')
import qualified Data.Text         as T

addAll :: Num a => [[a]] -> Either T.Text [a]
addAll ls =
    let filler l end = if length l < end then filler (0:l) end else l
        z = length . head $ ls
        go = foldl' (zipWith (+)) (filler [] z) ls
    in  if null go
        then Left "Empty list. Borked data probably."
        else Right go
