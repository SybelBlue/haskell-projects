-- Method declared in following grammar
-- <function name> :: <Optional Type Modifier Declarations... '=>'> <parameter type> -> ... <parameter type> -> <return type>
-- Types are usually generic, should be as generic as possible
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (a0, a1) (b0, b1) = (a0 + b0, a1 + b1)

-- Note: [Char] is identical to String
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiAsshole :: (RealFloat a) => a -> a -> String
bmiAsshole weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- let and where are similar constructs, except let comes first duh
-- and let requires an 'in' expression where the constants are evaluated

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea


-- [let square x = x * x in (square 5, square 3, square 2)]
 -- >> [(25,9,4)]

--Knock offs of lib functions to follow

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
   | x > maxTail = x
   | otherwise = maxTail
   where maxTail = maximum' xs

head' :: [a] -> a
head' xs = case xs of
                [] -> error "No head for empty lists!"
                (x:_) -> x

-- using gaurds instead of case because we switch on boolean expressions
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
   | n <= 0    = []
   | otherwise = x:replicate' (n-1) x

-- can use multiple cases and gaurds to take care of edge conditions
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
   | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys


-- higher order functions
-- note the mandatory parens, which explicilty state (fn of a to a) and param a return a
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- applyTwice (++ " HAHA") "HEY"
-- >> "HEY HAHA HAHA"

-- another rip off function
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
-- pattern (a::list) takes head and tail from list
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- lambda syntax: (\p0 p1 ... -> <op over pn>)

-- lib fn foldl "folds list up from left side"

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- $ is a weak binder like // in Mathematica, or f(g (z x)) = f $ g $ z $ x
-- useful for composition and instant application ie
-- map ($ 3) [(4+), (10*), (^2), sqrt]
-- >> [7.0,30.0,9.0,1.7320508075688772]

-- True composition happens with . so f(g(x)) = f.g x

-- some random practice
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) = (f x) && (all' f xs)

betterAll' :: (a -> Bool) -> [a] -> Bool
betterAll' f xs = foldl (\acc x -> acc && f x) True xs

reduce :: (result -> a -> result) -> result -> [a] -> result
reduce _ x [] = x
reduce f x (y:ys) = reduce f (f x y) ys
-- end of practice

{-
-- Consider the following terrible data declaration (uncomment for full effect)
data Person = Person String String Int Float String String deriving (Show)

-- It is slightly more readable through these getter functions
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _ number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor

-- let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- >>
-- firstName guy
-- >>"Buddy"
-}

-- There exists shorthand for this type of common construction,
-- it's called Record Syntax
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)

-- Old code still works in ghci
-- let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
-- >>
-- firstName guy
-- >>"Buddy"

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

-- Another benefit of Record Syntax is this is now allowable in ghci
-- Car {company="Ford", model="Mustang", year=1967}
-- >> Car {company="Ford", model="Mustang", year=1967}

-- Note that types can have type parameters like in most other OOP
-- For example, consider the rip off of the built-in type Maybe
data Maybe' a = Nothing' | Just' a deriving (Eq, Show)

--another rip off a built in
data Either' a b = Left a | Right b deriving (Eq, Ord, Read, Show)

-- view ./MapExamples.hs for more map and type stuff

{-
Note on deriving classes:
Eq grants deep == and /= checking
Ord tells Haskell to generate > < >= <=

Read allows well formatted (usually input) strings to convert into the type
Show allows the type to be turned into a string
--Note that read (show t)  is t and show (read t) is t for any type deriving both
-- Read and Show
-}

-- a new alias for List constructor, note Cons is new : operator
-- Note that the typer parameters come after the name
-- data List a = Empty | Cons a (List a) deriving (Show, Eq, Ord, Read)

-- `f` turns function f x y to an infix operator or
-- f x y always equals x `f` y
-- However this can be explicilty forced with a 'fixity operator'
infixr 5 :-: -- the 5 is the precedence value (* is 7, + is 6, higher is stronger)
data List a = Empty | a :-: (List a) deriving (Show, Eq, Ord, Read)

-- ++ operator for our new List, see below for mockup of original ++
infixr 5 .++
(.++) :: List a -> List a -> List a
Empty      .++ ys = ys
-- note that pattern matching happens on any constructors including operators
(x :-: xs) .++ ys = x :-: (xs .++ ys)

{- -- compare to original ++ operator
infixr 5  ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
-}


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

-- so we define a helper method for leaves
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- insert into tree as a set
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- a method to check membership
within :: (Eq a) => a -> Tree a -> Bool
within _ EmptyTree = False
within x (Node v left right) = x == v || x `within` left || x `within` right

-- rip off of the standard Eq class. This is standard syntax
class Eq' a where
    (.==) :: a -> a -> Bool
    (./=) :: a -> a -> Bool
    x .== y = not (x ./= y)
    x ./= y = not (x .== y)


-- consider the following Type
data TrafficLight = Red | Yellow | Green

-- and the instance of Eq we wish to define specific to TrafficLight
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False
    -- note that the circularity of the definition of == and /= allows only
    -- overriding one or the other

-- similar process for show
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"


-- another rip off definition
instance (Eq m) => Eq' (Maybe m) where
    Just x .== Just y = x == y
    Nothing .== Nothing = True
    _ .== _ = False

-- lets implement JS truthiness
class Truthy a where
  boolify :: a -> Bool

instance Truthy Int where
  boolify 0 = False
  boolify _ = True

instance Truthy Double where
  boolify 0 = False
  boolify _ = True

instance Truthy Bool where
  boolify = id -- some special method on Bool?

instance Truthy (Maybe a) where
  boolify (Just _) = True
  boolify Nothing = False

instance Truthy TrafficLight where
  boolify Red = False
  boolify _ = True

instance Truthy [a] where
  boolify [] = False
  boolify _ = True

ifTruthy :: (Truthy b) => b -> a -> a -> a
ifTruthy cond ifTrue ifFalse = if boolify cond then ifTrue else ifFalse

-- knock off functor is mapping function class
-- Note f is a type that takes a type and returns another
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b
-- to clarify, f is a non-concrete type (like a Java Generic type x Java Abstract Class)

-- as an example consider the following knock off version of the map function
instance Functor' [] where
  fmap' = map
  -- note that this makes the concrete signature of fmap
  --fmap' :: (a -> b) -> [a] -> [b]
  -- which is indeed the type pattern of map

-- another k-o declaration from an existing part of Haskell
instance Functor' Maybe where
  fmap' f (Just x) = Just (f x)
  fmap' f Nothing = Nothing

-- in the real deal ghci this is allowable because of the real Functor defns
-- fmap (++ " HEY GUYS IM INSIDE THE JUST") (Just "Something serious.")
-- >> Just "Something serious. HEY GUYS IM INSIDE THE JUST"
-- fmap (++ " HEY GUYS IM INSIDE THE JUST") Nothing
-- >> Nothing
-- fmap (*2) (Just 200)
-- >> Just 400
-- fmap (*2) Nothing
-- >> Nothing

-- Complex Typing

-- can look up "kind" of object :k command in ghci, so
-- :k Maybe
-- >> Maybe :: * -> *

-- Note that * refers to any concrete type, so
-- :k Maybe Int
-- >> Maybe Int :: *

-- Furthermore, look at the type of Either
-- :k Either
-- >> Either :: * -> * -> *
-- Note that kind constructors are curried like functions, so can be applied/partially constructed

-- Therefore type of Functor f is * -> *

-- some type foo, consider the typing of t
class Tofoo t where
  tofu :: j a -> t a j

-- j a must be *, and a is inferred *, so j is (* -> *)
-- therefore t is * -> (* -> *) -> *

-- lets build a class with the kind of t
data Frank a b = Frank {frankField :: b a} deriving (Show)
-- as seen in the record syntax, frankField takes a b (* -> *) and a * and therefore
-- constructor for Frank frankField is parameterized * -> (* -> *) -> *

instance Tofoo Frank where
  tofu x = Frank x

-- tofu (Just 'a') :: Frank Char Maybe
-- >> Frank { frankField = Just 'a' }
-- tofu ["HELLO"] :: Frank [Char] []
-- >> Frank {frankField = ["HELLO"]}

-- And I quote "Not very useful, but we did flex our type muscles."
-- Here's another type bar
data Barry t k p = Barry { yabba :: p, dabba :: t k }
-- yabba is kind p or *
-- dabba is kind t on k so t is (* -> *) and k is *
-- Barry is kind (* -> *) -> * -> *

instance Functor (Barry a b) where
  fmap f (Barry { yabba = x, dabba = y }) = Barry { yabba = f x, dabba = y }

-- the key to Functor is it's a way to pass an f into a data type implicitly,
-- or a Functor is a type which can be mapped over

-- runhaskell <name>.hs is the command to run a script on the fly,
-- or you can use ghc --make <name> and then .\<name>
-- look at .\IOExamples\ for more examples

-- interesting rip off of the meaning of composition
instance Functor' ((->) r) where
  fmap' f g = f.g
