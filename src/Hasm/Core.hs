module Hasm.Core where

import Prelude hiding (map, null, head, splitAt, prependToAll, intercalate, intersperse, foldr1, tail)
import qualified Prelude as P
import qualified Data.List as L
import qualified Data.Bits as B
import Data.Vector.Unboxed hiding ((++))
import qualified Data.Vector as V

data CPU = CPU Int (Vector Int) (Vector Int) deriving (Eq)
instance Show CPU where
    show (CPU c rs _) = "\t" ++ (showRegs rs)
        where showRegs = L.intercalate "\n\t" . P.map (L.intercalate "\t") . chunks 8 .  P.map showReg . toList . indexed
              showReg (idx, v) = "r" ++ (show idx) ++ ":\t" ++ (show v)

chunks n v
    | P.null v    = []
    | otherwise = (fst pair):((chunks n . snd) pair)
    where pair = P.splitAt n v

data Arg = Reg Int | Val Int deriving (Show, Eq)
data Label = Lbl String | Addr Int deriving (Show, Eq)

data Instruction = Nop
                 | Mov Arg Arg
                 | Add Arg Arg Arg
                 | Sub Arg Arg Arg
                 | Mul Arg Arg Arg
                 | Div Arg Arg Arg
                 | Mod Arg Arg Arg
                 | And Arg Arg Arg
                 | Or Arg Arg Arg
                 | Xor Arg Arg Arg
                 | Sll Arg Arg Arg
                 | Srl Arg Arg Arg
                 | Ld Arg Arg
                 | Str Arg Arg
                 | Movl Arg Label
                 | Jmp Label
                 | Call Label
                 | Jr Arg
                 | Bne Arg Arg Label
                 | Beq Arg Arg Label
                 | Blt Arg Arg Label
                 | Bgt Arg Arg Label
                 | Ble Arg Arg Label
                 | Bge Arg Arg Label
                 deriving (Show, Eq)

valOf (Reg r) (CPU _ rs _) = rs ! r
valOf (Val a) _            = a

final f r1 r2 cpu = f (valOf r1 cpu) (valOf r2 cpu)

combine f (Reg dst) r1 r2 cpu@(CPU c rs mem) = CPU (c+1) (rs // [(dst, final f r1 r2 cpu)]) mem

mov dst src cpu = combine (\x _ -> x) dst src dst cpu
add = combine (+)
sub = combine (-)
mul = combine (*)
mod = combine Prelude.mod
div = combine Prelude.div
xor = combine B.xor
and = combine (B..&.)
or  = combine (B..|.)
sll = combine (\x y -> B.shiftL x y)
srl = combine (\x y -> B.shiftR x y)

ld (Reg dst) src cpu@(CPU c rs mem) = CPU (c+1) (rs // [(dst, mem ! (valOf src cpu))]) mem
str dst src cpu@(CPU c rs mem)      = CPU (c+1) rs (mem // [(valOf src cpu, valOf dst cpu)])

recount c (CPU _ rs mem) = CPU c rs mem
recountR r1 cpu@(CPU _ rs mem) = CPU (valOf r1 cpu) rs mem
increment cpu@(CPU c _ _) = recount (c+1) cpu

run :: V.Vector Instruction -> CPU -> CPU
run is cpu@(CPU c _ _) =
    case (is V.!? c) of
        Nothing -> cpu
        (Just i) -> run is $ runInstruction i cpu

runPrint :: V.Vector Instruction -> CPU -> IO ()
runPrint is cpu@(CPU c _ _) = do
    case (is V.!? c) of
        Nothing  -> return ()
        (Just i) -> do
            putStrLn $ (show i) ++ "\n"
            let final = runInstruction i cpu
            putStrLn $ (show final) ++ "\n"
            runPrint is final

runInstruction :: Instruction -> CPU -> CPU
runInstruction (Mov dst src)         = mov dst src
runInstruction (Ld dst src)          = ld dst src
runInstruction (Str dst src)         = str dst src
runInstruction (Add dst r1 r2)       = add dst r1 r2
runInstruction (Sub dst r1 r2)       = sub dst r1 r2
runInstruction (Mul dst r1 r2)       = mul dst r1 r2
runInstruction (Div dst r1 r2)       = Hasm.Core.div dst r1 r2
runInstruction (Mod dst r1 r2)       = Hasm.Core.mod dst r1 r2
runInstruction (And dst r1 r2)       = Hasm.Core.and dst r1 r2
runInstruction (Xor dst r1 r2)       = xor dst r1 r2
runInstruction (Or dst r1 r2)        = Hasm.Core.or dst r1 r2
runInstruction (Sll dst r1 r2)       = sll dst r1 r2
runInstruction (Srl dst r1 r2)       = srl dst r1 r2
runInstruction (Movl dst (Addr idx)) = mov dst (Val idx)
runInstruction (Jmp (Addr idx))      = recount  idx
runInstruction (Jr arg)              = recountR arg
runInstruction (Bne r1 r2 idx)       = branchIf (/=) r1 r2 idx
runInstruction (Beq r1 r2 idx)       = branchIf (==) r1 r2 idx
runInstruction (Blt r1 r2 idx)       = branchIf (<)  r1 r2 idx
runInstruction (Bgt r1 r2 idx)       = branchIf (>)  r1 r2 idx
runInstruction (Ble r1 r2 idx)       = branchIf (<=) r1 r2 idx
runInstruction (Bge r1 r2 idx)       = branchIf (>=) r1 r2 idx
runInstruction Nop                   = increment
runInstruction (Call (Addr idx))     = runCall idx
    where runCall idx cpu@(CPU c rs mem) = (recount idx . mov (Reg 31) (Val (fromIntegral (c+1)))) cpu

branchIf f r1 r2 (Addr idx) cpu
    | f (valOf r1 cpu) (valOf r2 cpu) = recount idx cpu
    | otherwise                       = increment cpu
