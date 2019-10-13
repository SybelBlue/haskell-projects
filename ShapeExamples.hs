data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

-- constructors are functions, ie
-- map (Circle (Point 10 20)) [4,5]
-- >> [Circle (Point 10.0 20.0) 4.0,Circle (Point 10.0 20.0) 5.0]

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x0 y0) (Point x1 y1)) = (abs $ x1 - x0) * (abs $ y1 - y0)

nudge :: Shape -> (Float, Float) -> Shape
nudge (Circle (Point cx cy) r) (a, b) = Circle (Point (cx+a) (cy+b)) r
nudge (Rectangle (Point x0 y0) (Point x1 y1)) (a, b) =
  Rectangle (Point (x0+a) (y0+b)) (Point (x1+a) (y1+b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRectange :: Float -> Float -> Shape
baseRectange a b = Rectangle (Point 0 0) (Point a b)

{-
module Shapes
( Point(..) --means export all constructor types
, Shape(..)
, surface
, nudge
, baseCircle
, baseRectange
) -- can add where clause here to alias internal names
-}
