module VM.Parser where 

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Char
import qualified Data.Vector as V

import VM.Core

caseChar c = char (toLower c) <|> char (toUpper c)
caseString s = try (mapM caseChar s) <?> "\"" ++ s ++ "\""

spaceSkip = skipMany $ satisfy (`elem` ['\t', ' '])

parseReg = Reg <$> (char 'r' *> decimal)
parseMem = Mem <$> (char 'm' *> decimal)

parseVal = Val <$> decimal

parseLoc = parseReg
       <|> parseMem

parseArg = parseLoc
       <|> parseVal

parseNoArgs inst = do
    skipSpace
    caseString inst
    skipSpace

parseUnary inst = do
    parseNoArgs inst
    arg <- parseArg
    skipSpace
    return arg

parseBinary inst = do
    parseNoArgs inst
    arg1 <- parseArg
    skipSpace
    arg2 <- parseArg
    skipSpace
    return (arg1, arg2)

parseTernary inst = do
    parseNoArgs inst
    arg1 <- parseLoc
    skipSpace
    arg2 <- parseArg
    skipSpace
    arg3 <- parseArg 
    skipSpace
    return (arg1, arg2, arg3)

fromTernaryTuple (inst, c) = do
    (dst, r1, r2) <- parseTernary inst
    return $ c dst r1 r2

ternaryParsers = map fromTernaryTuple [("add", Add)
                                      ,("and", And)
                                      ,("or", Or)
                                      ,("xor", Xor)
                                      ,("sub", Sub)
                                      ,("mul", Mul)]

skipComments = do
    char ';' 
    manyTill' anyChar (try endOfLine)
    return ()

parseEndOfLine = skipSpace *> endOfLine --(skipComments <|> endOfLine)

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
    skipSpace
    src <- parseArg
    return $ Mov dst src

parseJmp = do
    parseNoArgs "jmp"
    dst <- decimal
    return $ Jmp dst

parseBne = do
    (r1, r2) <- parseBinary "bne"
    addr <- decimal
    return $ Bne r1 r2 addr

parseBeq = do
    (r1, r2) <- parseBinary "beq"
    addr <- decimal
    return $ Beq r1 r2 addr

parseInst = parseMov
        <|> parseNop
        <|> parseJmp
        <|> parseInc
        <|> parseDec
        <|> choice ternaryParsers
        <|> parseBne
        <|> parseBeq

parseLine = do
    inst <- parseInst
    skipComments
    return inst

parseConfig = do
    skipSpace
    registers <- decimal
    skipSpace
    memory <- decimal
    return $ CPU 0 (V.replicate registers 0) (V.replicate memory 0)

listOf n v = Prelude.take n . cycle $ [v]

parseFile :: Parser (CPU Int, [Instruction Int])
parseFile = do
    cpu <- parseConfig
    skipSpace
    instructions <- many $ parseInst <* endOfLine
    -- endOfInput
    return (cpu, instructions)
