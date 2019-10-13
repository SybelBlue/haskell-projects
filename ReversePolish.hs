import Data.List

solveRPN :: String -> Float
-- get the head of an evolving stack from folding words of expression
solveRPN = head . (foldl foldingFunction []) . words
    where   foldingFunction (x:y:ys) "*" = (x * y):ys
            foldingFunction (x:y:ys) "+" = (x + y):ys
            foldingFunction (x:y:ys) "-" = (y - x):ys
            foldingFunction (x:y:ys) "/" = (y / x):ys
            foldingFunction (x:y:ys) "^" = (y ** x):ys
            foldingFunction xs "sum" = [sum xs]
            foldingFunction xs numberString = read numberString:xs
