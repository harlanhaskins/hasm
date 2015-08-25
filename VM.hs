module VM where

import Data.Bits as B

data Register a = Register { value :: a } deriving (Show, Eq)

instance Functor Register where
    fmap f (Register v) = Register (f v)

instance Applicative Register where
    pure = Register
    Register f <*> Register v = Register (f v)

instance Monad Register where
    return = pure
    Register v >>= f = f v

data CPU a = CPU { counter :: Int, registers :: [Register a] } deriving (Show, Eq)

instance Functor CPU where
    fmap f (CPU c rs) = CPU (c + 1) (map (fmap f) rs)

--instance Applicative CPU where
--    pure v = CPU 0 [Register v]
--    CPU c rfs <*> CPU c' rs = CPU (c + c') (zipWith (<*>) rfs rs)

fromValue i rs = (value . (!! i)) rs

combine f r1 r2 dst (CPU c rs) = CPU (c+1) $ replace dst final rs
    where final = Register $ f (fromValue r1 rs) (fromValue r2 rs)

modify f src dst cpu = combine f src dst dst cpu

mov = modify (\x _ -> x) 

add = combine (+)

sub = combine (-)

mul = combine (*)

div = combine (/)

{-

xor = combine B.xor

and = combine (.&.)

or = combine (.|.)

not = modify B.complement

-}

replace n item ls = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls
