module VM.Parser where

import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BSC
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import Data.Either
import qualified Data.Vector as V

import VM.Core

word = takeWhile1 . inClass $ "-a-zA-Z_'"

caseChar c = char (toLower c) <|> char (toUpper c)
caseString s = try (mapM caseChar s) <?> "\"" ++ s ++ "\""

spaceSkip = skipMany $ satisfy (`elem` ['\t', ' '])

parseReg = Reg <$> (char 'r' *> decimal)
parseMem = Mem <$> (char 'm' *> decimal)

parseVal = Val <$> decimal

parseLbl = Lbl . BSC.unpack <$> word

parseAddr = Addr <$> decimal

parseLabel = Lbl <$> manyTill anyChar (char ':')

parseJmpLabel = parseLbl <|> parseAddr

parseLoc = parseReg
       <|> parseMem

parseArg = parseLoc
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
    arg1 <- parseLoc
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
    reg <- parseLoc
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
                                                ,("blez", Ble)]

parseNop = do
    parseNoArgs "nop"
    return Nop

parseInc = do
    parseNoArgs "inc"
    reg <- parseLoc
    return $ Add reg reg (Val 1)

parseDec = do
    parseNoArgs "dec"
    reg <- parseLoc
    return $ Sub reg reg (Val 1)

parseMov = do
    parseNoArgs "mov"
    dst <- parseLoc
    spaceSkip
    src <- parseArg
    return $ Mov dst src

parseJmp = do
    parseNoArgs "jmp"
    dst <- parseJmpLabel
    return $ Jmp dst

parseInst = parseMov
        <|> parseNop
        <|> parseJmp
        <|> parseInc
        <|> parseDec
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
    registers <- decimal
    spaceSkip
    memory <- decimal
    return $ CPU 0 (V.replicate registers 0) (V.replicate memory 0)

partitionLabels :: [Either Label Instruction] -> ([(String, Int)], [Instruction])
partitionLabels = partitionEithers . map fix . index
    where fix ((Right i), _)       = Right i
          fix ((Left (Lbl l)), n)  = Left (l, n)

index = zipWithLabels 0
    where zipWithLabels _ []     = []
          zipWithLabels n (x:xs) = (x, n):(zipWithLabels (offsetFor x) xs)
              where offsetFor (Left _) = n
                    offsetFor (Right _) = n + 1


replaceLabels lines = map (replacedLabel labels) instructions
    where labels = M.fromList $ fst partitioned
          instructions = snd partitioned
          partitioned = partitionLabels lines

replacedLabel ls (Jmp       (Lbl s)) = Jmp       (Addr (ls M.! s))
replacedLabel ls (Blt r1 r2 (Lbl s)) = Blt r1 r2 (Addr (ls M.! s))
replacedLabel ls (Bgt r1 r2 (Lbl s)) = Bgt r1 r2 (Addr (ls M.! s))
replacedLabel ls (Bge r1 r2 (Lbl s)) = Bge r1 r2 (Addr (ls M.! s))
replacedLabel ls (Ble r1 r2 (Lbl s)) = Ble r1 r2 (Addr (ls M.! s))
replacedLabel ls (Bne r1 r2 (Lbl s)) = Bne r1 r2 (Addr (ls M.! s))
replacedLabel ls (Beq r1 r2 (Lbl s)) = Beq r1 r2 (Addr (ls M.! s))
replacedLabel _  i                   = i

listOf n v = Prelude.take n . cycle $ [v]

parseFile :: Parser (CPU, [Instruction])
parseFile = do
    cpu <- parseConfig
    parseEndOfLine
    lines <- many $ parseCmd <* parseEndOfLine
    -- endOfInput
    return (cpu, replaceLabels lines)
