------ RECORD SYNTAX -----------------------------------------------------------


-- There exists shorthand for this type of common construction,
-- it's called Record Syntax
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)


------ ALGEBRAIC TYPES & INTERFACE IMPLEMENTATION ------------------------------

-- consider the following Type
data TrafficLight = Red | Yellow | Green | Broken deriving (Show, Read)

switchColor :: TrafficLight -> TrafficLight
switchColor Red = Green
switchColor Green = Yellow
switchColor Yellow = Red
switchColor Broken = Broken

-- and the instance of Eq we wish to define specific to TrafficLight
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    Broken == Broken = False
    _ == _ = False


------ LIST COMPREHENSIONS -----------------------------------------------------


-- https://www.codewars.com/kata/two-sum/train/haskell
-- /u/Vulwsztyn
twoSum :: [Int] -> Int -> (Int,Int)
twoSum xs n = head [(fst x, fst y) | x <- exs
                                   , y <- exs
                                   , snd x + snd y == n
                                   , fst x < fst y
                                   ]
  where exs = zip [0..] xs -- enumerated exs


------ PATTERN MATCHING & RECURSION --------------------------------------------


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

-- a helper method for leaves
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- only inserts unique things
-- case | operators, pattern matching
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- backtick notation
within :: (Eq a) => a -> Tree a -> Bool
within _ EmptyTree = False
within x (Node v left right) = x == v || x `within` left || x `within` right


------ INFINTE RECURSION -------------------------------------------------------


infiniteTree :: Int -> Tree Int
infiniteTree n = Node n (infiniteTree (n + 1)) (infiniteTree (n + 1))

-- 100 `within` (infiniteTree 1)
