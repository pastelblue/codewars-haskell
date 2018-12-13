import Data.Array

balancedParens :: Int -> [String]
balancedParens n = table ! (n, n)
  where 
    a :: Array Int Int
    a = listArray (1, n) [1..n] 

    table :: Array (Int, Int) [String]
    table = listArray ((0, 0), (n, n)) [f i j | i <- [0..n], j <- [0..n]]

    f i j | i == 0    = [take j $ repeat ')']
          | i == j    = map ('(':) (f (i-1) j)
          | i < j     = map ('(':) (f (i-1) j) ++ map (')':) (f i (j-1))
          | otherwise = [""]