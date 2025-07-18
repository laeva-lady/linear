{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main
  ( main,
  )
where

import Matrices
import Rref

main :: IO ()
main = do
  let m = [[1, 3, 3, 8, 5], [0, 1, 3, 10, 8], [0, 0, 0, -1, -4], [0, 0, 0, 2, 8]]
  putStrLn $ prettyMatrix $ elements $ rref $ fromList2DDefault m
