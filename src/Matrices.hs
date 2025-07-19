-- {-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Matrices
  ( add,
    dotproduct,
    fromList2D,
    identity,
    mapm,
    matrix,
    Matrix (..),
    multiply,
    power,
    prettyMatrix,
    scale,
    transpose,
  )
where

data Matrix a
  = Matrix
  { rowCount :: Int,
    colCount :: Int,
    elements :: [[a]]
  }

instance (Show a) => Show (Matrix a) where
  show = prettyMatrix

matrix :: Int -> Int -> [[a]] -> Matrix a
matrix r c es
  | length es /= r = error "Number of rows does not match given row count"
  | any (\row -> length row /= c) es = error "All rows must have the same number of columns as the given column count"
  | otherwise = Matrix {rowCount = r, colCount = c, elements = es}

issquare :: Matrix a -> Bool
issquare (Matrix r c _) = r == c

newelems :: Matrix a -> [[a]] -> Matrix a
newelems m [] = m
newelems m xs = matrix (rowCount m) (colCount m) xs

defaultMatrix :: Matrix a
defaultMatrix = matrix 0 0 []

fromList2D :: [[a]] -> Matrix a
fromList2D [] = defaultMatrix
fromList2D rows@(r : rs)
  | all (\row -> length row == length r) rs =
      Matrix (length rows) (length r) rows
  | otherwise = error "All rows must have the same length"

add :: (Num a) => Matrix a -> Matrix a -> Matrix a
add m n
  | mr == nr && mc == nc =
      newelems m $ zipWith (zipWith (+)) ms ns
  | otherwise = error "Matrices must have the same dimensions for addition"
  where
    mr = rowCount m
    nr = rowCount n
    mc = colCount m
    nc = colCount n
    ms = elements m
    ns = elements n

mapm :: (a -> a) -> Matrix a -> Matrix a
mapm f m = newelems m $ map (map f) $ elements m

scale :: (Num a) => a -> Matrix a -> Matrix a
scale k m = newelems m $ map (map (* k)) $ elements m

identity :: (Num a) => Int -> Matrix a
identity size =
  matrix
    size
    size
    [[if i == j then 1 else 0 | j <- [0 .. size - 1]] | i <- [0 .. size - 1]]

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose x = map head x : transpose (map tail x)

dotproduct :: (Num a) => [a] -> [a] -> a
dotproduct va vb
  | length va == length vb = sum $ zipWith (*) va vb
  | otherwise = error "Vectors must have the same length for dot product"

power :: (Num a) => Int -> Matrix a -> Matrix a
power k _ | k < 0 = error "Power must be a non-negative integer"
power _ m | not $ issquare m = error "Matrix must be square"
power 0 m = identity $ rowCount m
power 1 m = m
power k m = multiply m (power (k - 1) m)

multiply :: (Num a) => Matrix a -> Matrix a -> Matrix a
multiply m n
  | colCount m == rowCount n = matrix newrow newcol $ map (\row -> map (dotproduct row) (transpose ns)) ms
  | otherwise = error "The number of columns in the first matrix must equal the number of rows in the second matrix for multiplication"
  where
    newrow = rowCount m
    newcol = colCount n
    ms = elements m
    ns = elements n

prettyMatrix :: (Show a) => Matrix a -> String
prettyMatrix mat =
  let -- Convert all elements to strings
      stringMatrix :: [[String]]
      stringMatrix = map (map show) $ elements mat

      numCols :: Int
      numCols = maximum (map length stringMatrix ++ [0])

      colWidths :: [Int]
      colWidths =
        let paddedRows = map (\row -> row ++ replicate (numCols - length row) "") stringMatrix
            colMaxWidths = foldr (zipWith max . map length) (replicate numCols 0) paddedRows
         in colMaxWidths

      -- sum of colWidths + spaces between + 4 for borders
      totalWidth :: Int
      totalWidth = sum colWidths + (numCols - 1) + 4

      horizontalLine :: String
      horizontalLine = concat $ replicate (totalWidth - 2) " "

      padLeft :: Int -> String -> String
      padLeft width s = replicate (width - length s) ' ' ++ s

      formatRow :: [String] -> String
      formatRow row =
        let paddedCells = zipWith padLeft colWidths row
         in "│ " ++ unwords paddedCells ++ " │"

      topLine = "╭" ++ horizontalLine ++ "╮"
      bottomLine = "╰" ++ horizontalLine ++ "╯"
      formattedRows = map formatRow stringMatrix
   in unlines (topLine : formattedRows ++ [bottomLine])
