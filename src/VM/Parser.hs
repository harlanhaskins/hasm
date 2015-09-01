module VM.Parser where

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Char
import Data.Either
import qualified Data.Vector as V

import VM.Core

caseChar c = char (toLower c) <|> char (toUpper c)
caseString s = try (mapM caseChar s) <?> "\"" ++ s ++ "\""

spaceSkip = skipMany $ satisfy (`elem` ['\t', ' '])

parseReg = Reg <$> (char 'r' *> decimal)
parseMem = Mem <$> (char 'm' *> decimal)

parseVal = Val <$> decimal

parseLbl = do
    label <- manyTill anyChar (char ':')
    return $ Lbl label

parseAddr = do
    addr <- decimal
    return $ Addr addr

parseLabel = parseLbl
         <|> parseAddr

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
    dst <- parseLabel
    return $ Jmp dst

parseBranch inst c = do
    (r1, r2) <- parseBinary inst
    addr <- parseLabel
    return (r1, r2, addr)

fromTernaryTuple (inst, c) = do
    (dst, r1, r2) <- parseTernary inst
    return $ c dst r1 r2

ternaryParsers = map fromTernaryTuple [("add", Add)
                                      ,("and", And)
                                      ,("or", Or)
                                      ,("xor", Xor)
                                      ,("sll", Sll)
                                      ,("srl", Srl)
                                      ,("sub", Sub)
                                      ,("mul", Mul)]

branchParsers = map fromBranchTuple   [("beq", Beq)
                                      ,("bne", Bne)
                                      ,("blt", Blt)
                                      ,("bgt", Bgt)]

fromBranchTuple (inst, c) = do
    (r1, r2, addr) <- parseBranch inst c
    return $ c r1 r2 addr

parseInst = parseMov
        <|> parseNop
        <|> parseJmp
        <|> parseInc
        <|> parseDec
        <|> choice ternaryParsers
        <|> choice branchParsers

skipComments = do
    spaceSkip
    char ';'
    manyTill' anyChar (try endOfLine)
    return ()

parseEndOfLine = skipComments <|> (spaceSkip <* endOfLine)

parseLine = Left <$> parseLabel
        <|> Right <$> (parseInst <* parseEndOfLine)

parseConfig = do
    spaceSkip
    registers <- decimal
    spaceSkip
    memory <- decimal
    return $ CPU 0 (V.replicate registers 0) (V.replicate memory 0)

partitionLabels :: [Either Label (Instruction Int)] -> ([(Label, Int)], [Instruction Int])
partitionLabels lines = partitionLabels' 0 lines [] []
    where partitionLabels' _ [] ls is = (ls, is)
          partitionLabels' n ((Left l):ls) labels insts = partitionLabels' (n+1) ls ((l, n):labels) insts
          partitionLabels' n ((Right i):ls) labels insts = partitionLabels' (n+1) ls labels (i:insts)

replaceLabels lines = instructions
    where labels = fst partitioned
          instructions = snd partitioned
          partitioned = partitionLabels lines

listOf n v = Prelude.take n . cycle $ [v]

parseFile :: Parser (CPU Int, [Instruction Int])
parseFile = do
    cpu <- parseConfig
    parseEndOfLine
    lines <- many $ parseInst <* parseEndOfLine
    --endOfInput
    return (cpu, lines)
