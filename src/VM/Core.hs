module VM.Core where

import qualified Data.Bits as B
import qualified Data.Vector as V

data CPU = CPU Int (V.Vector Integer) (V.Vector Integer) deriving (Eq)
instance Show CPU where
    show (CPU c rs _) = "CPU " ++ (show c) ++ " " ++ (show rs)

data Arg = Reg Int | Mem Int | Val Integer deriving (Show, Eq)
data Label = Lbl String | Addr Int deriving (Show, Eq)

data Instruction = Nop
                   | Mov Arg Arg
                   | Add Arg Arg Arg
                   | Sub Arg Arg Arg
                   | Mul Arg Arg Arg
                   | Div Arg Arg Arg
                   | And Arg Arg Arg
                   | Or Arg Arg Arg
                   | Xor Arg Arg Arg
                   | Sll Arg Arg Arg
                   | Srl Arg Arg Arg
                   | Jmp Label
                   | Bne Arg Arg Label
                   | Beq Arg Arg Label
                   | Blt Arg Arg Label
                   | Bgt Arg Arg Label
                   | Ble Arg Arg Label
                   | Bge Arg Arg Label
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
sll = combine (\x y -> B.shiftL x (fromIntegral y))
srl = combine (\x y -> B.shiftR x (fromIntegral y))

recount (CPU _ rs mem) c = CPU c rs mem

run :: V.Vector Instruction -> CPU -> CPU
run is cpu@(CPU c _ _) =
    case (is V.!? c) of
        Nothing -> cpu
        (Just i) -> run is $ runInstruction i cpu

runPrint :: V.Vector Instruction -> CPU -> IO ()
runPrint is cpu@(CPU c _ _) = do
    case (is V.!? c) of
        Nothing -> print cpu
        (Just i) -> do
            print i
            let final = runInstruction i cpu
            print final
            runPrint is final

runInstruction :: Instruction -> CPU -> CPU
runInstruction (Mov dst src) cpu    = mov dst src cpu
runInstruction (Add dst r1 r2) cpu  = add dst r1 r2 cpu
runInstruction (Sub dst r1 r2) cpu  = sub dst r1 r2 cpu
runInstruction (Mul dst r1 r2) cpu  = mul dst r1 r2 cpu
runInstruction (Div dst r1 r2) cpu  = VM.Core.div dst r1 r2 cpu
runInstruction (And dst r1 r2) cpu  = VM.Core.and dst r1 r2 cpu
runInstruction (Xor dst r1 r2) cpu  = xor dst r1 r2 cpu
runInstruction (Or dst r1 r2) cpu   = VM.Core.or dst r1 r2 cpu
runInstruction (Sll dst r1 r2) cpu  = sll dst r1 r2 cpu
runInstruction (Srl dst r1 r2) cpu  = srl dst r1 r2 cpu
runInstruction (Jmp (Addr idx)) cpu = recount cpu idx
runInstruction (Bne r1 r2 idx) cpu  = branchIf (/=) r1 r2 idx cpu
runInstruction (Beq r1 r2 idx) cpu  = branchIf (==) r1 r2 idx cpu
runInstruction (Blt r1 r2 idx) cpu  = branchIf (<) r1 r2 idx cpu
runInstruction (Bgt r1 r2 idx) cpu  = branchIf (>) r1 r2 idx cpu
runInstruction (Ble r1 r2 idx) cpu  = branchIf (<=) r1 r2 idx cpu
runInstruction (Bge r1 r2 idx) cpu  = branchIf (>=) r1 r2 idx cpu
runInstruction Nop cpu@(CPU c _ _)  = recount cpu (c+1)

branchIf f r1 r2 (Addr idx) cpu@(CPU c rs mem)
    | f (valOf r1 cpu) (valOf r2 cpu) = recount cpu idx
    | otherwise                       = CPU (c+1) rs mem

