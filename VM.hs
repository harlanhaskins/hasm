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

data CPU a = CPU { registers :: [Register a], memory :: [Register a]} deriving (Show, Eq)

fromValue i rs = (value . (!! i)) rs

fromLists rs mems = CPU (toRegisters rs) (toRegisters mems)
    where toRegisters = (map Register) 

combine f r1 r2 dst (CPU rs mem) = CPU (replace dst final rs) mem
    where final = pure $ f (fromValue r1 rs) (fromValue r2 rs)

modify f src dst cpu = combine (\x _ -> f x) src dst dst cpu

load addr dst (CPU rs mem) = CPU (replace dst (mem !! addr) rs) mem

mov = modify id

str a idx (CPU rs mem) = CPU (replace idx (Register a) rs) mem

add :: (Num a) => Int -> Int -> Int -> CPU a -> CPU a
add = combine (+)

sub :: (Num a) => Int -> Int -> Int -> CPU a -> CPU a
sub = combine (-)

mul :: (Num a) => Int -> Int -> Int -> CPU a -> CPU a
mul = combine (*)

div :: (Fractional a, Num a) => Int -> Int -> Int -> CPU a -> CPU a
div = combine (/)

xor :: (Bits a) => Int -> Int -> Int -> CPU a -> CPU a
xor = combine B.xor

and :: (Bits a) => Int -> Int -> Int -> CPU a -> CPU a
and = combine (.&.)

or :: (Bits a) => Int -> Int -> Int -> CPU a -> CPU a
or = combine (.|.)

data Instruction a = Mov Int Int
                   | Load Int Int
                   | Add Int Int Int
                   | Sub Int Int Int
                   | Mul Int Int Int
                   | Jmp Int
                   | Bne Int Int Int
                   | Beq Int Int Int
                   | Str a Int 
                   deriving (Show, Eq)

data Program a = Program { remaining :: [Instruction a]
                         , instructions :: [Instruction a]
                         } deriving (Show, Eq)

fromInstructions xs = Program xs xs

run :: (Num a, Eq a) => Program a -> CPU a -> CPU a
run (Program [] _) cpu             = cpu
run (Program ((Jmp idx):_) is) cpu = run (Program (drop idx is) is) cpu
run p@(Program ((Bne r1 r2 idx):_) _) cpu = branchIf (/=) p r1 r2 idx cpu
run p@(Program ((Beq r1 r2 idx):_) _) cpu = branchIf (==) p r1 r2 idx cpu
run (Program (i:is) is') cpu       = run (Program is is') (runInstruction i cpu)
    where runInstruction (Mov src dst)   cpu = mov src dst cpu
          runInstruction (Load src dst)  cpu = load src dst cpu
          runInstruction (Add r1 r2 dst) cpu = add r1 r2 dst cpu
          runInstruction (Sub r1 r2 dst) cpu = sub r1 r2 dst cpu
          runInstruction (Mul r1 r2 dst) cpu = mul r1 r2 dst cpu
          runInstruction (Str a idx) cpu = str a idx cpu

branchIf f (Program (i:is) is') r1 r2 idx cpu@(CPU rs mem)
    | f (fromValue r1 rs) (fromValue r2 rs) = run (Program ((Jmp idx):is) is') cpu
    | otherwise                             = run (Program is is') cpu

replace _ item [_] = [item]
replace n item ls = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls
