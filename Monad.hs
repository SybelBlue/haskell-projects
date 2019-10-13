data Expr = Val Int | Div Expr Expr deriving (Show)

safeDiv :: Int -> Int -> Maybe Int
safeDiv n m = if m == 0 then Nothing else Just (quot n m)

{- -- verbose but correct
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of j
                        Nothing -> Nothing
                        Just n -> case eval y of
                                        Nothing -> Nothing
                                        Just m -> safeDiv n m
-}

{-
-- sequencing operator to make things better
m >>= f = case m of
                Nothing -> Nothing
                Just x -> f x

-- good but still awkward
eval :: Expr -> Maybe Int
eval (Val n) = return n
eval (Div x y) = eval x >>= (\n ->
                 eval y >>= (\m ->
                 safeDiv n m))
-}

-- do block =?= Maybe Monad
eval :: Expr -> Maybe Int
eval (Val n) = return n
eval (Div x y) = do {n <- eval x; m <- eval y; safeDiv n m}
