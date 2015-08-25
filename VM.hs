module VM where

import Data.Bits as B

data Instruction = Mov Int Int
                 | Add Int Int Int
                 | Sub Int Int Int
                 | Mul Int Int Int

data Register a = Register { value :: a } deriving (Show, Eq)

instance Functor Register where
    fmap f (Register v) = Register (f v)

instance Applicative Register where
    pure = Register
    Register f <*> Register v = Register (f v)

instance Monad Register where
    return = pure
    Register v >>= f = f v

data CPU a = CPU { counter :: Int, registers :: [Register a], memory :: [Register a]} deriving (Show, Eq)

fromValue i rs = (value . (!! i)) rs

fromLists rs mems = CPU 0 (toRegisters rs) (toRegisters mems)
    where toRegisters = (map Register) 

combine f r1 r2 dst (CPU c rs mem) = CPU (c+1) (replace dst final rs) mem
    where final = pure $ f (fromValue r1 rs) (fromValue r2 rs)

modify f src dst cpu = combine f src dst dst cpu

load addr dst (CPU c rs mem) = CPU (c+1) (replace dst (mem !! addr) rs) mem

mov = modify (\x _ -> x) 

add :: (Num a) => Int -> Int -> Int -> CPU a -> CPU a
add = combine (+)

sub :: (Num a) => Int -> Int -> Int -> CPU a -> CPU a
sub = combine (-)

mul :: (Num a) => Int -> Int -> Int -> CPU a -> CPU a
mul = combine (*)

-- div :: (Fractional a, Num a) => Int -> Int -> Int -> CPU a -> CPU a
-- div = combine (/)

xor :: (Bits a) => Int -> Int -> Int -> CPU a -> CPU a
xor = combine B.xor

and :: (Bits a) => Int -> Int -> Int -> CPU a -> CPU a
and = combine (.&.)

or :: (Bits a) => Int -> Int -> Int -> CPU a -> CPU a
or = combine (.|.)

-- not = modify B.complement

run :: (Num a) => [Instruction] -> CPU a -> CPU a
run [] cpu = cpu
run (x:xs) cpu = run xs (runInstruction x cpu)

runInstruction :: (Num a) => Instruction -> CPU a -> CPU a
runInstruction (Mov src dst) cpu = mov src dst cpu
runInstruction (Add r1 r2 dst) cpu = add r1 r2 dst cpu
runInstruction (Sub r1 r2 dst) cpu = sub r1 r2 dst cpu
runInstruction (Mul r1 r2 dst) cpu = mul r1 r2 dst cpu

replace n item ls = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls
