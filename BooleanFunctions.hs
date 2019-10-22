import Data.Tuple

data Boolean = Tru | Fls deriving (Show, Read)

conditional :: Boolean -> a -> b -> Either a b
conditional Tru x _ = Left x
conditional Fls _ y = Right y

--type Cond = (* -> * -> *)

id' :: a -> a
id' x = x

true x _ = x

false _ y = y

--funcify :: Bool -> Cond
funcify True = true
funcify False = false

not' b = b false true

and' x y = x y false
or' x y = x true y

--xor' x y = x (y false true) (y true false)
xor x y = ((x not id) y) true false

--while :: (a -> Cond) -> (a -> a) -> a -> a
while sentinel body v = (sentinel v) (while sentinel body (body v)) v
