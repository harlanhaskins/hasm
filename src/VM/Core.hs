module VM.Core where

import qualified Data.Bits as B
import qualified Data.Vector as V

data CPU a = CPU Int (V.Vector Int) (V.Vector Int) deriving (Eq)
instance (Show a) => Show (CPU a) where
    show (CPU c rs _) = "CPU " ++ (show c) ++ " " ++ (show rs)

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

valOf (Reg r) (CPU _ rs _) = rs V.! r
valOf (Mem m) (CPU _ _ mem) = mem V.! m
valOf (Val a) _ = a

final f r1 r2 cpu = f (valOf r1 cpu) (valOf r2 cpu)
updatePair dst f r1 r2 cpu = V.fromList [(dst, (final f r1 r2 cpu))]

combine f (Reg dst) r1 r2 cpu@(CPU c rs mem) = CPU (c+1) (V.update rs (updatePair dst f r1 r2 cpu)) mem
combine f (Mem dst) r1 r2 cpu@(CPU c rs mem) = CPU (c+1) rs (V.update mem (updatePair dst f r1 r2 cpu))

mov dst src cpu = combine (\x _ -> x) dst src dst cpu

-- add :: (Num a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
add = combine (+)

-- sub :: (Num a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
sub = combine (-)

-- mul :: (Num a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
mul = combine (*)

-- div :: (Fractional a, Num a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
-- div = combine (/)

-- xor :: (B.Bits a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
xor = combine B.xor

-- and :: (B.Bits a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
and = combine (B..&.)

-- or :: (B.Bits a) => Arg a -> Arg a -> Arg a -> CPU a -> CPU a
or = combine (B..|.)

recount (CPU _ rs mem) c = CPU c rs mem

-- run :: (Num a, B.Bits a) => V.Vector (Instruction a) -> CPU a -> CPU a
run is cpu@(CPU c _ _) = 
    case (is V.!? c) of         
        Nothing -> cpu
        (Just i) -> run is $ runInstruction i cpu

runPrint is cpu@(CPU c _ _) = do
    case (is V.!? c) of
        Nothing -> print cpu
        (Just i) -> do
            print i
            let final = runInstruction i cpu
            print final
            runPrint is final

runInstruction (Mov dst src) cpu        = mov dst src cpu
runInstruction (Add dst r1 r2) cpu      = add dst r1 r2 cpu
runInstruction (Sub dst r1 r2) cpu      = sub dst r1 r2 cpu
runInstruction (Mul dst r1 r2) cpu      = mul dst r1 r2 cpu
runInstruction (And dst r1 r2) cpu      = VM.Core.and dst r1 r2 cpu
runInstruction (Xor dst r1 r2) cpu      = xor dst r1 r2 cpu
runInstruction (Or dst r1 r2) cpu       = VM.Core.or dst r1 r2 cpu
runInstruction (Jmp idx) cpu            = recount cpu idx
runInstruction (Bne r1 r2 idx) cpu      = branchIf (/=) r1 r2 idx cpu
runInstruction (Beq r1 r2 idx) cpu      = branchIf (==) r1 r2 idx cpu
runInstruction Nop cpu                  = cpu

branchIf f r1 r2 idx cpu@(CPU c rs mem)
    | f (valOf r1 cpu) (valOf r2 cpu) = recount cpu idx
    | otherwise                       = CPU (c+1) rs mem

