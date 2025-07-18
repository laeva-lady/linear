module Lib
  ( display,
    matNull,
    matId,
    matTranspose,
    matSomme,
    matMultK,
    matProduit,
    matPow,
    matSousMat,
    matMap,
  )
where

import Data.Matrix

-- Lab01
display :: (Show a) => Matrix a -> IO ()
display ma = do
  putStrLn $ prettyMatrix ma

matNull :: Int -> Int -> a -> Matrix a
matNull rows cols defValue = matrix rows cols $ \(_, _) -> defValue

matId :: (Num a) => Int -> Matrix a
matId n =
  matrix n n $ \(i, j) ->
    if i == j
      then 1
      else 0

matTranspose :: Matrix a -> Matrix a
matTranspose a = matrix (ncols a) (nrows a) $ \(i, j) -> a ! (j, i)

matSomme :: (Num a) => Matrix a -> Matrix a -> Matrix a
matSomme a b =
  if nrows a /= nrows a
    then error "not smae length"
    else matrix (nrows a) (ncols a) $ \(i, j) -> (a ! (i, j)) + (b ! (i, j))

matMultK :: (Num a) => a -> Matrix a -> Matrix a
matMultK k a = matrix (nrows a) (ncols a) $ \(i, j) -> k * (a ! (i, j))

matMap :: (a -> a) -> Matrix a -> Matrix a
matMap f a = matrix (nrows a) (ncols a) $ \(i, j) -> f $ a ! (i, j)

matProduit :: (Num a) => Matrix a -> Matrix a -> Matrix a
matProduit a b
  | ncols a /= nrows b = error "Invalid matrix dimensions for multiplication"
  | otherwise =
      matrix (nrows a) (ncols b) $ \(i, j) ->
        -- dotproduct
        sum [(a ! (i, k)) * (b ! (k, j)) | k <- [1 .. ncols a]]

matPow :: (Num a) => Int -> Matrix a -> Matrix a
matPow 0 a =
  if nrows a == ncols a
    then
      matId $ nrows a
    else
      matId 0
matPow 1 a = a
matPow k a = matProduit a (matPow (k - 1) a)

matSousMat :: Matrix a -> Int -> Int -> Matrix a
matSousMat a r c = minorMatrix r c a
