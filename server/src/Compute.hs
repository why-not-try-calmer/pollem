{-# LANGUAGE OverloadedStrings #-}

module Compute where
import           Data.List     (transpose)
import qualified Data.Text     as T
import           ErrorsReplies
import qualified ErrorsReplies          as ER

collect :: (Num a, Eq a) => [[a]] -> Either (ER.Err T.Text ) [a]
collect [] = Left . ER.Err BorkedData $ mempty
collect [x] = Right x
collect ls =
    let (x:xs) = ls
    in  if any (/= x) xs then Left . ER.Err BorkedData $ mempty
        else Right . map sum . transpose $ ls
