{-# LANGUAGE GADTs #-}
module GADTs where

-- some trivial examples
data GMaybe a where
    Nothing :: GMaybe a
    Just    :: a -> GMaybe a

data GList a where
    Nil  :: GList a
    Cons :: a -> GList a -> GList a


-- a more useful example

-- we want to define an interpreter
-- that has more than one data type, 
-- say int and bool.
--
-- we want to be able to write this eval:

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Eq  e1 e2) = eval e1 == eval e2

-- this requires that we know that e1 & e2
-- satisfy different constraints in the same
-- function. 
--
-- Ideally, the constructors would determine
-- the constraint of their fields, ie
-- Add only accepts ints, B only bools.
--
-- The best solution here is GADTs:

data Expr a where
    I   ::                      Int -> Expr Int
    B   ::                     Bool -> Expr Bool
    Add ::     Expr Int -> Expr Int -> Expr Int
    Mul ::     Expr Int -> Expr Int -> Expr Int
    Eq  :: Eq a => Expr a -> Expr a -> Expr Bool

-- From this definition, Haskell can extract
-- the proper types for each matched statement:
-- (Add e1 e2) generates :t e1 as Int,
-- (B b) generates :t b as Bool, etc.