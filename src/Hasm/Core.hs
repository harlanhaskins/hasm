module Hasm.Core where

import Prelude hiding (map, null, head, splitAt, prependToAll, intercalate, intersperse, foldr1, tail)
import qualified Data.Bits as B
import Data.Vector hiding ((++))

data CPU = CPU Int (Vector Integer) (Vector Integer) deriving (Eq)
instance Show CPU where
    show (CPU c rs _) = "CPU " ++ (show c) ++ "  " ++ (showRegs rs)
        where showRegs = intercalate "\n\t" . map (intercalate "  ") . chunks 5 .  map showReg . indexed
              showReg (idx, v) = "[r" ++ (show idx) ++ ": " ++ (show v) ++ "]"

intersperse :: a -> Vector a -> Vector a
intersperse s v | null v    = v
                | otherwise = head v `cons` prependToAll s (tail v)

prependToAll :: a -> Vector a -> Vector a
prependToAll s v | null v    = v
                 | otherwise = s `cons` (head v `cons` prependToAll s (tail v))

intercalate :: [a] -> Vector [a] -> [a]
intercalate = (foldr1 (++) .) . intersperse

chunks n v
    | null v    = empty
    | otherwise = cons (fst pair) ((chunks n . snd) pair)
    where pair = splitAt n v

data Arg = Reg Int | Mem Int | Val Integer deriving (Show, Eq)
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
                 | Jmp Label
                 | Bne Arg Arg Label
                 | Beq Arg Arg Label
                 | Blt Arg Arg Label
                 | Bgt Arg Arg Label
                 | Ble Arg Arg Label
                 | Bge Arg Arg Label
                 deriving (Show, Eq)

valOf (Reg r) (CPU _ rs _) = rs ! r
valOf (Mem m) (CPU _ _ mem) = mem ! m
valOf (Val a) _ = a

final f r1 r2 cpu = f (valOf r1 cpu) (valOf r2 cpu)
updatePair dst f r1 r2 cpu = fromList [(dst, (final f r1 r2 cpu))]

combine f (Reg dst) r1 r2 cpu@(CPU c rs mem) = CPU (c+1) (update rs (updatePair dst f r1 r2 cpu)) mem
combine f (Mem dst) r1 r2 cpu@(CPU c rs mem) = CPU (c+1) rs (update mem (updatePair dst f r1 r2 cpu))

mov dst src cpu = combine (\x _ -> x) dst src dst cpu
add = combine (+)
sub = combine (-)
mul = combine (*)
mod = combine Prelude.mod
div = combine Prelude.div
xor = combine B.xor
and = combine (B..&.)
or  = combine (B..|.)
sll = combine (\x y -> B.shiftL x (fromIntegral y))
srl = combine (\x y -> B.shiftR x (fromIntegral y))

recount c (CPU _ rs mem) = CPU c rs mem
increment (CPU c rs mem) = CPU (c+1) rs mem

run :: Vector Instruction -> CPU -> CPU
run is cpu@(CPU c _ _) =
    case (is !? c) of
        Nothing -> cpu
        (Just i) -> run is $ runInstruction i cpu

runPrint :: Vector Instruction -> CPU -> IO ()
runPrint is cpu@(CPU c _ _) = do
    case (is !? c) of
        Nothing -> print cpu
        (Just i) -> do
            print i
            let final = runInstruction i cpu
            print final
            runPrint is final

runInstruction :: Instruction -> CPU -> CPU
runInstruction (Mov dst src)    = mov dst src
runInstruction (Add dst r1 r2)  = add dst r1 r2
runInstruction (Sub dst r1 r2)  = sub dst r1 r2
runInstruction (Mul dst r1 r2)  = mul dst r1 r2
runInstruction (Div dst r1 r2)  = Hasm.Core.div dst r1 r2
runInstruction (Mod dst r1 r2)  = Hasm.Core.mod dst r1 r2
runInstruction (And dst r1 r2)  = Hasm.Core.and dst r1 r2
runInstruction (Xor dst r1 r2)  = xor dst r1 r2
runInstruction (Or dst r1 r2)   = Hasm.Core.or dst r1 r2
runInstruction (Sll dst r1 r2)  = sll dst r1 r2
runInstruction (Srl dst r1 r2)  = srl dst r1 r2
runInstruction (Jmp (Addr idx)) = recount idx
runInstruction (Bne r1 r2 idx)  = branchIf (/=) r1 r2 idx
runInstruction (Beq r1 r2 idx)  = branchIf (==) r1 r2 idx
runInstruction (Blt r1 r2 idx)  = branchIf (<) r1 r2 idx
runInstruction (Bgt r1 r2 idx)  = branchIf (>) r1 r2 idx
runInstruction (Ble r1 r2 idx)  = branchIf (<=) r1 r2 idx
runInstruction (Bge r1 r2 idx)  = branchIf (>=) r1 r2 idx
runInstruction Nop              = increment

branchIf f r1 r2 (Addr idx) cpu
    | f (valOf r1 cpu) (valOf r2 cpu) = recount idx cpu
    | otherwise                       = increment cpu
