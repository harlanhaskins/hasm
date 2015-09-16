module Hasm.Parser where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BSC
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import Data.Either
import qualified Data.Vector.Unboxed as V

import Hasm.Core

regs = M.fromList
     . map (\(n, idx) -> (n, Reg idx)) $
     [ ("ra", 31)
     ]

word = takeWhile1 . inClass $ "-a-zA-Z0-9_'"

caseChar c = char (toLower c) <|> char (toUpper c)
caseString s = try (mapM caseChar s) <?> "\"" ++ s ++ "\""

spaceSkip = skipMany $ satisfy (`elem` ['\t', ' '])

parseReg = Reg <$> (caseChar 'r' *> decimal)

parseVal = Val <$> decimal

parseLbl = Lbl . BSC.unpack <$> word

parseAddr = Addr <$> decimal

parseLabel = do
    spaceSkip
    text <- word
    char ':'
    return $ (Lbl . BSC.unpack) text

parseJmpLabel = parseLbl <|> parseAddr

parseArg = parseReg
       <|> parseVal

parseNoArgs inst = do
    spaceSkip
    caseString inst
    spaceSkip

parseUnary inst = do
    parseNoArgs inst
    arg <- parseArg
    return arg

parseBinary inst = do
    parseNoArgs inst
    arg1 <- parseArg
    spaceSkip
    arg2 <- parseArg
    return (arg1, arg2)

parseTernary inst = do
    parseNoArgs inst
    arg1 <- parseReg
    spaceSkip
    arg2 <- parseArg
    spaceSkip
    arg3 <- parseArg
    return (arg1, arg2, arg3)

fromTernaryTuple (inst, c) = do
    (dst, r1, r2) <- parseTernary inst
    return $ c dst r1 r2

parseBranch inst c = do
    parseNoArgs inst
    spaceSkip
    loc <- parseArg
    spaceSkip
    arg <- parseArg
    spaceSkip
    addr <- parseJmpLabel
    spaceSkip
    return (loc, arg, addr)

fromBranchTuple (inst, c) = do
    (r1, r2, addr) <- parseBranch inst c
    return $ c r1 r2 addr

parsePseudoBranchZero inst c = do
    parseNoArgs inst
    spaceSkip
    reg <- parseReg
    spaceSkip
    addr <- parseJmpLabel
    return $ c reg (Val 0) addr

fromPseudoBranchTuple (inst, c) = parsePseudoBranchZero inst c

ternaryParsers = map fromTernaryTuple           [("add", Add)
                                                ,("and", And)
                                                ,("or", Or)
                                                ,("xor", Xor)
                                                ,("sll", Sll)
                                                ,("srl", Srl)
                                                ,("sub", Sub)
                                                ,("div", Div)
                                                ,("mod", Mod)
                                                ,("mul", Mul)]

branchParsers = map fromBranchTuple             [("beq", Beq)
                                                ,("bne", Bne)
                                                ,("bge", Bge)
                                                ,("ble", Ble)
                                                ,("blt", Blt)
                                                ,("bgt", Bgt)]

pseudoBranchParsers = map fromPseudoBranchTuple [("bgtz", Bgt)
                                                ,("bgez", Bge)
                                                ,("bltz", Blt)
                                                ,("beqz", Beq)
                                                ,("bnez", Bne)
                                                ,("blez", Ble)]

parseNop = do
    parseNoArgs "nop"
    return Nop

parseRet = do
    parseNoArgs "ret"
    return $ Jr (Reg 31)

parseInc = do
    parseNoArgs "inc"
    reg <- parseReg
    return $ Add reg reg (Val 1)

parseDec = do
    parseNoArgs "dec"
    reg <- parseReg
    return $ Sub reg reg (Val 1)

parseMov = do
    parseNoArgs "mov"
    dst <- parseReg
    spaceSkip
    src <- parseArg
    return $ Mov dst src

parseMovl = do
    parseNoArgs "movl"
    dst <- parseReg
    spaceSkip
    src <- parseJmpLabel
    return $ Movl dst src

parseJmp = do
    parseNoArgs "jmp"
    dst <- parseJmpLabel
    return $ Jmp dst

parseCall = do
    parseNoArgs "call"
    dst <- parseJmpLabel
    return $ Call dst

parseJr = do
    parseNoArgs "jr"
    dst <- parseReg
    return $ Jr dst

parseMemInst inst c = do
    parseNoArgs inst
    dst <- parseReg
    spaceSkip
    src <- parseReg
    return $ c dst src

parseStr = parseMemInst "str" Str
parseLd = parseMemInst "ld" Ld

parseInst = parseMov
        <|> parseNop
        <|> parseJmp
        <|> parseCall
        <|> parseMovl
        <|> parseJr
        <|> parseRet
        <|> parseInc
        <|> parseDec
        <|> parseLd
        <|> parseStr
        <|> choice ternaryParsers
        <|> choice branchParsers
        <|> choice pseudoBranchParsers

skipComments = do
    spaceSkip
    char ';'
    manyTill' anyChar (try endOfLine)
    return ()

parseEndOfLine = skipComments <|> skipSpace

parseCmd = Right <$> parseInst
       <|> Left  <$> parseLabel

parseConfig = do
    spaceSkip
    memory <- decimal
    parseEndOfLine
    return $ CPU 0 (V.replicate 32 0) (V.replicate memory 0)

partitionLabels :: [Either Label Instruction] -> ([(String, Int)], [Instruction])
partitionLabels = partitionEithers . map fix . indexRights
    where fix ((Right i), _)       = Right i
          fix ((Left (Lbl l)), n)  = Left (l, n)

indexRights = indexRights' 0
    where indexRights' _ []     = []
          indexRights' n (x:xs) = (x, n):(indexRights' (offsetFor x) xs)
              where offsetFor (Left _) = n
                    offsetFor (Right _) = n + 1


replaceLabels lines = map (replacedLabel labels) instructions
    where labels = M.fromList $ fst partitioned
          instructions = snd partitioned
          partitioned = partitionLabels lines

replacedLabel ls (Jmp       (Lbl s)) = Jmp       (Addr (ls M.! s))
replacedLabel ls (Call      (Lbl s)) = Call      (Addr (ls M.! s))
replacedLabel ls (Movl dst  (Lbl s)) = Movl dst  (Addr (ls M.! s))
replacedLabel ls (Blt r1 r2 (Lbl s)) = Blt r1 r2 (Addr (ls M.! s))
replacedLabel ls (Bgt r1 r2 (Lbl s)) = Bgt r1 r2 (Addr (ls M.! s))
replacedLabel ls (Bge r1 r2 (Lbl s)) = Bge r1 r2 (Addr (ls M.! s))
replacedLabel ls (Ble r1 r2 (Lbl s)) = Ble r1 r2 (Addr (ls M.! s))
replacedLabel ls (Bne r1 r2 (Lbl s)) = Bne r1 r2 (Addr (ls M.! s))
replacedLabel ls (Beq r1 r2 (Lbl s)) = Beq r1 r2 (Addr (ls M.! s))
replacedLabel _  i                   = i

parseFile :: Parser (CPU, [Instruction])
parseFile = do
    cpu <- parseConfig
    skipSpace
    lines <- many $ parseCmd <* parseEndOfLine
    skipSpace
    endOfInput
    return (cpu, replaceLabels lines)
