{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main
  ( main,
  )
where

import Matrices
import Rref

main :: IO ()
main = do
  example01
  example02

example01 :: IO ()
example01 =
  let m = [[1, 3, 3, 8, 5], [0, 1, 3, 10, 8], [0, 0, 0, -1, -4], [0, 0, 0, 2, 8]]
   in print $ rref $ fromList2D m

example02 :: IO ()
example02 = do
  print $ mapm (\x -> 35 + 624 * x) $ multiply a b
  print $ power 2 a
  print $ power 5 $ mapm (\x -> 35 + 624 * x) $ multiply a b
  where
    a =
      matrix
        7
        7
        [ [-35, 0, -19, 99, -91, -35, 0],
          [0, 0, -50, 0, 0, 80, 12],
          [0, -88, 0, -3, 0, -82, 0],
          [0, 0, 0, 0, 0, 91, 22],
          [-46, 18, -97, 95, -26, 0, 9],
          [0, 86, 0, -61, 22, 0, 0],
          [0, 0, -15, -78, 0, 82, -25]
        ]
    b =
      matrix
        7
        3
        [ [0, 1, 9],
          [0, 0, 0],
          [-1, 2, 0],
          [27, 75, 2],
          [54, 0, -29],
          [0, -45, 0],
          [0, 1, 0]
        ]
