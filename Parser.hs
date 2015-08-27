module Parser where 

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Char
import VM

caseChar c = char (toLower c) <|> char (toUpper c)
caseString s = try (mapM caseChar s) <?> "\"" ++ s ++ "\""

parseReg = do
    char 'r'
    val <- decimal
    return (Reg val)

parseMem = do
    char 'm'
    val <- decimal
    return (Mem val)

parseVal = do
    val <- decimal
    return (Val val)

parseArg = parseReg 
       <|> parseMem
       <|> parseVal

parseNoArgs inst = do
    skipSpace
    caseString inst
    skipSpace

parseUnary inst = do
    parseNoArgs inst
    src <- parseArg
    return src

parseBinary inst = do
    src <- parseUnary inst
    skipSpace
    dst <- parseArg
    return (src, dst)

parseTernary inst = do
    (r1, r2) <- parseBinary inst
    skipSpace
    dst <- parseArg
    return (r1, r2, dst)

parseNop = do
    parseUnary "nop" 
    return Nop

parseMov = do
    (src, dst) <- parseBinary "mov"
    return $ Mov src dst

parseJmp = do
    skipSpace 
    caseString "jmp"
    dst <- decimal
    return $ Jmp dst

parseAdd = do
    (r1, r2, dst) <- parseTernary "add"
    return $ Add r1 r2 dst

parseOr = do
    (r1, r2, dst) <- parseTernary "or"
    return $ Or r1 r2 dst

parseXor = do
    (r1, r2, dst) <- parseTernary "add"
    return $ Xor r1 r2 dst

parseAnd = do
    (r1, r2, dst) <- parseTernary "add"
    return $ And r1 r2 dst

parseSub = do
    (r1, r2, dst) <- parseTernary "sub"
    return $ Sub r1 r2 dst

parseMul = do
    (r1, r2, dst) <- parseTernary "mul"
    return $ Mul r1 r2 dst

parseBne = do
    (r1, r2) <- parseBinary "bne"
    skipSpace
    addr <- decimal
    return $ Bne r1 r2 addr

parseBeq = do
    (r1, r2) <- parseBinary "beq"
    skipSpace
    addr <- decimal
    return $ Beq r1 r2 addr

parseInst = parseMov
        <|> parseNop
        <|> parseJmp
        <|> parseAdd
        <|> parseAnd
        <|> parseOr
        <|> parseXor
        <|> parseSub
        <|> parseMul
        <|> parseBne
        <|> parseBeq

parseConfig = do
    skipSpace
    registers <- decimal
    skipSpace
    memory <- decimal
    return $ fromLists (listOf registers 0) (listOf memory 0)

listOf n v = Prelude.take n . cycle $ [v]

parseFile :: Parser (CPU Int, [Instruction Int])
parseFile = do
    cpu <- parseConfig
    skipSpace
    instructions <- many $ parseInst <* endOfLine
    return (cpu, instructions)
