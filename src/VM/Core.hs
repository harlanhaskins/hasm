module VM.Core where

import qualified Data.Bits as B
import qualified Data.Vector as V

data CPU a = CPU Int (V.Vector a) (V.Vector a) deriving (Eq)
instance (Show a) => Show (CPU a) where
    show (CPU c rs _) = "CPU " ++ (show c) ++ " " ++ (show rs)

data Arg a = Reg Int | Mem Int | Val a deriving (Show, Eq)
data Label = Lbl String | Addr Int deriving (Show, Eq)

data Instruction a = Nop
                   | Mov (Arg a) (Arg a)
                   | Add (Arg a) (Arg a) (Arg a)
                   | Sub (Arg a) (Arg a) (Arg a)
                   | Mul (Arg a) (Arg a) (Arg a)
                   | Div (Arg a) (Arg a) (Arg a)
                   | And (Arg a) (Arg a) (Arg a)
                   | Or (Arg a) (Arg a) (Arg a)
                   | Xor (Arg a) (Arg a) (Arg a)
                   | Sll (Arg a) (Arg a) (Arg a)
                   | Srl (Arg a) (Arg a) (Arg a)
                   | Jmp Label
                   | Bne (Arg a) (Arg a) Label
                   | Beq (Arg a) (Arg a) Label
                   | Blt (Arg a) (Arg a) Label
                   | Bgt (Arg a) (Arg a) Label
                   | Ble (Arg a) (Arg a) Label
                   | Bge (Arg a) (Arg a) Label
                   deriving (Show, Eq)

valOf (Reg r) (CPU _ rs _) = rs V.! r
valOf (Mem m) (CPU _ _ mem) = mem V.! m
valOf (Val a) _ = a

final f r1 r2 cpu = f (valOf r1 cpu) (valOf r2 cpu)
updatePair dst f r1 r2 cpu = V.fromList [(dst, (final f r1 r2 cpu))]

combine f (Reg dst) r1 r2 cpu@(CPU c rs mem) = CPU (c+1) (V.update rs (updatePair dst f r1 r2 cpu)) mem
combine f (Mem dst) r1 r2 cpu@(CPU c rs mem) = CPU (c+1) rs (V.update mem (updatePair dst f r1 r2 cpu))

mov dst src cpu = combine (\x _ -> x) dst src dst cpu
add = combine (+)
sub = combine (-)
mul = combine (*)
div = combine (Prelude.div)
xor = combine B.xor
and = combine (B..&.)
or  = combine (B..|.)
sll = combine B.shiftL
srl = combine B.shiftR

recount (CPU _ rs mem) c = CPU c rs mem

run :: V.Vector (Instruction Int) -> CPU Int -> CPU Int
run is cpu@(CPU c _ _) =
    case (is V.!? c) of
        Nothing -> cpu
        (Just i) -> run is $ runInstruction i cpu

runPrint :: V.Vector (Instruction Int) -> CPU Int -> IO ()
runPrint is cpu@(CPU c _ _) = do
    case (is V.!? c) of
        Nothing -> print cpu
        (Just i) -> do
            print i
            let final = runInstruction i cpu
            print final
            runPrint is final

runInstruction :: Instruction Int -> CPU Int -> CPU Int
runInstruction (Mov dst src) cpu   = mov dst src cpu
runInstruction (Add dst r1 r2) cpu = add dst r1 r2 cpu
runInstruction (Sub dst r1 r2) cpu = sub dst r1 r2 cpu
runInstruction (Mul dst r1 r2) cpu = mul dst r1 r2 cpu
runInstruction (Div dst r1 r2) cpu = VM.Core.div dst r1 r2 cpu
runInstruction (And dst r1 r2) cpu = VM.Core.and dst r1 r2 cpu
runInstruction (Xor dst r1 r2) cpu = xor dst r1 r2 cpu
runInstruction (Or dst r1 r2) cpu  = VM.Core.or dst r1 r2 cpu
runInstruction (Sll dst r1 r2) cpu = sll dst r1 r2 cpu
runInstruction (Srl dst r1 r2) cpu = srl dst r1 r2 cpu
runInstruction (Jmp (Addr idx)) cpu       = recount cpu idx
runInstruction (Bne r1 r2 idx) cpu = branchIf (/=) r1 r2 idx cpu
runInstruction (Beq r1 r2 idx) cpu = branchIf (==) r1 r2 idx cpu
runInstruction (Blt r1 r2 idx) cpu = branchIf (<) r1 r2 idx cpu
runInstruction (Bgt r1 r2 idx) cpu = branchIf (>) r1 r2 idx cpu
runInstruction (Ble r1 r2 idx) cpu = branchIf (<=) r1 r2 idx cpu
runInstruction (Bge r1 r2 idx) cpu = branchIf (>=) r1 r2 idx cpu
runInstruction Nop cpu@(CPU c _ _) = recount cpu (c+1)

branchIf f r1 r2 (Addr idx) cpu@(CPU c rs mem)
    | f (valOf r1 cpu) (valOf r2 cpu) = recount cpu idx
    | otherwise                       = CPU (c+1) rs mem

