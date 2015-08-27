module VM where

import qualified Data.Bits as B

data CPU a = CPU [a] [a] deriving (Eq)
instance (Show a) => Show (CPU a) where
    show (CPU rs _) = "CPU " ++ (show rs)

data Arg a = Reg Int | Mem Int | Val a deriving (Show, Eq)

data Instruction a = Nop
                   | Mov (Arg a) (Arg a)
                   | Add (Arg a) (Arg a) (Arg a)
                   | Sub (Arg a) (Arg a) (Arg a)
                   | Mul (Arg a) (Arg a) (Arg a)
                   | And (Arg a) (Arg a) (Arg a)
                   | Or (Arg a) (Arg a) (Arg a)
                   | Xor (Arg a) (Arg a) (Arg a)
                   | Jmp Int
                   | Bne (Arg a) (Arg a) Int
                   | Beq (Arg a) (Arg a) Int
                   deriving (Show, Eq)

data Program a = Program { remaining :: [Instruction a]
                         , instructions :: [Instruction a]
                         } deriving (Show, Eq)

fromInstructions xs = Program xs xs

fromLists rs mems = CPU rs mems

replace _ item [_] = [item]
replace n item ls = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls

valOf (Reg r) (CPU rs _) = rs !! r
valOf (Mem m) (CPU _ mem) = mem !! m
valOf (Val a) _ = a

final f r1 r2 cpu = f (valOf r1 cpu) (valOf r2 cpu)

combine f (Reg dst) r1 r2 cpu@(CPU rs mem) = CPU (replace dst (final f r1 r2 cpu) rs) mem
combine f (Mem dst) r1 r2 cpu@(CPU rs mem) = CPU rs (replace dst (final f r1 r2 cpu) mem)

mov dst src cpu = combine (\x _ -> x) dst src dst cpu

add :: (Num a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
add = combine (+)

sub :: (Num a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
sub = combine (-)

mul :: (Num a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
mul = combine (*)

div :: (Fractional a, Num a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
div = combine (/)

xor :: (B.Bits a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
xor = combine B.xor

and :: (B.Bits a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
and = combine (B..&.)

or :: (B.Bits a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
or = combine (B..|.)

run :: (Num a, B.Bits a) => Program a -> CPU a -> CPU a
run (Program [] _) cpu             = cpu
run (Program ((Jmp idx):_) is) cpu = run (Program (drop idx is) is) cpu
run p@(Program ((Bne r1 r2 idx):_) _) cpu = branchIf (/=) p r1 r2 idx cpu
run p@(Program ((Beq r1 r2 idx):_) _) cpu = branchIf (==) p r1 r2 idx cpu
run (Program (i:is) is') cpu       = run (Program is is') (runInstruction i cpu)
    where runInstruction (Mov dst src)   cpu = mov dst src cpu
          runInstruction (Add dst r1 r2) cpu = add dst r1 r2 cpu
          runInstruction (Sub dst r1 r2) cpu = sub dst r1 r2 cpu
          runInstruction (Mul dst r1 r2) cpu = mul dst r1 r2 cpu
          runInstruction (And dst r1 r2) cpu = VM.and dst r1 r2 cpu
          runInstruction (Xor dst r1 r2) cpu = xor dst r1 r2 cpu
          runInstruction (Or dst r1 r2) cpu = VM.or dst r1 r2 cpu
          runInstruction Nop cpu = cpu

branchIf f (Program (i:is) is') r1 r2 idx cpu@(CPU rs mem)
    | f (valOf r1 cpu) (valOf r2 cpu) = run (Program ((Jmp idx):is) is') cpu
    | otherwise                             = run (Program is is') cpu

