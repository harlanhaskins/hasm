module VM where

import qualified Data.Bits as B

data CPU a = CPU [a] [a] deriving (Show, Eq)

fromLists rs mems = CPU rs mems

{-
combine f (Reg r1) (Reg r2) (Reg dst) (CPU rs mem) = CPU (replace dst final rs) mem
combine f r1 r2 dst (CPU rs mem) = CPU (replace dst final rs) mem
combine f r1 r2 dst (CPU rs mem) = CPU (replace dst final rs) mem
    where final = pure $ f (rs !! r1) (rs !! r2)

modify f src dst cpu = combine (\x _ -> f x) src dst dst cpu

mov = modify id

add :: (Num a) => Int -> Int -> Int -> CPU a -> CPU a
add = combine (+)

sub :: (Num a) => Int -> Int -> Int -> CPU a -> CPU a
sub = combine (-)

mul :: (Num a) => Int -> Int -> Int -> CPU a -> CPU a
mul = combine (*)

div :: (Fractional a, Num a) => Int -> Int -> Int -> CPU a -> CPU a
div = combine (/)

xor :: (B.Bits a) => Int -> Int -> Int -> CPU a -> CPU a
xor = combine B.xor

and :: (B.Bits a) => Int -> Int -> Int -> CPU a -> CPU a
and = combine (B..&.)

or :: (B.Bits a) => Int -> Int -> Int -> CPU a -> CPU a
or = combine (B..|.)
-}

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
                   | Bne (Arg a) (Arg a) (Arg a)
                   | Beq (Arg a) (Arg a) (Arg a)
                   deriving (Show, Eq)

data Program a = Program { remaining :: [Instruction a]
                         , instructions :: [Instruction a]
                         } deriving (Show, Eq)

fromInstructions xs = Program xs xs
{-
run :: (Num a, B.Bits a) => Program a -> CPU a -> CPU a
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
          runInstruction (And r1 r2 dst) cpu = VM.and r1 r2 dst cpu
          runInstruction (Xor r1 r2 dst) cpu = xor r1 r2 dst cpu
          runInstruction (Or r1 r2 dst) cpu = VM.or r1 r2 dst cpu
          runInstruction (Str a idx) cpu = str a idx cpu
          runInstruction (Set a idx) cpu = set a idx cpu
          runInstruction Nop cpu = cpu

branchIf f (Program (i:is) is') r1 r2 idx cpu@(CPU rs mem)
    | f (rs !! r1) (rs !! r2) = run (Program ((Jmp idx):is) is') cpu
    | otherwise                             = run (Program is is') cpu
-}

replace _ item [_] = [item]
replace n item ls = a ++ (item:b)
    where (a, (_:b)) = splitAt n ls
