module Rref (rref) where

import Matrices

-- taken from https://github.com/labruzese/HaskellRREF

rref :: (Ord a, Fractional a) => Matrix a -> Matrix a
rref mat = cleanedRows $ foldl processColumn mat [0 .. minDimension]
  where
    minDimension = min (rowCount mat - 1) (colCount mat - 1)
    cleanedRows (Matrix m n rows) = Matrix m n $ map cleanupRow rows
    cleanupRow = map (\x -> if abs x < 1e-10 then 0 else x)

processColumn :: (Ord a, Fractional a) => Matrix a -> Int -> Matrix a
processColumn mat col =
  case findPivotRow mat col of
    Nothing -> mat -- No pivot found, move to next column
    Just pivotRow ->
      let step1 = swapRows mat col pivotRow -- put the pivot into the correct place
          step2 = normalizePivot step1 col -- make pivot 1
          step3 = makeColumnZero step2 col -- make the other items in that column zero
       in step3

-- | Finds the first occurance of a nonzero entry in the column and returns the index of the column
findPivotRow :: (Ord a, Fractional a) => Matrix a -> Int -> Maybe Int
findPivotRow mat col = findPivotRowHelper mat col col

findPivotRowHelper :: (Ord a, Fractional a) => Matrix a -> Int -> Int -> Maybe Int
findPivotRowHelper mat@(Matrix m _ rows) col currentRow
  | currentRow >= m = Nothing -- all 0s
  | otherwise =
      let val = rows !! currentRow !! col
       in if abs val > 1e-10
            then Just currentRow -- we found the first non-zero
            else findPivotRowHelper mat col (currentRow + 1) -- check the next row

-- | Swap two rows in the matrix
swapRows :: Matrix a -> Int -> Int -> Matrix a
swapRows mat@(Matrix m n rows) row1 row2
  | row1 == row2 = mat
  | otherwise = Matrix m n newElements
  where
    newElements = zipWith (curry swap) [0 ..] rows
    swap (i, row)
      | i == row1 = rows !! row2
      | i == row2 = rows !! row1
      | otherwise = row

-- | Make the pivot element 1 by dividing the entire row by the pivot value
normalizePivot :: (Fractional a) => Matrix a -> Int -> Matrix a
normalizePivot (Matrix m n rows) col =
  Matrix m n newElements
  where
    pivot = rows !! col !! col -- Get the pivot
    newElements = zipWith (curry normalizeRow) [0 ..] rows
    normalizeRow (i, row)
      | i == col = map (/ pivot) row -- Divide every element by the pivot
      | otherwise = row

-- | Make all other elements in the column zero by subtracting appropriate multiples of the pivot row
-- | *Only works if the pivot is 1
makeColumnZero :: (Num a) => Matrix a -> Int -> Matrix a
makeColumnZero (Matrix m n rows) col =
  Matrix m n newElements
  where
    pivotRow = rows !! col -- Get the row where the pivot element is located
    newElements = zipWith (curry eliminateRow) [0 ..] rows -- Process each row with its index
    eliminateRow (i, row)
      | i == col = row -- Don't modify the pivot row itself
      | otherwise =
          let val = row !! col -- Value to elim
           in zipWith (\p r -> r - val * p) pivotRow row -- Multiply pivot row by value
          -- Subtract row from new pivot row
          -- (Then technically divide the pivot row by value again)
