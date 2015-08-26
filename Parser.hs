module Parser where 

import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Data.Char
import VM

caseChar c = char (toLower c) <|> char (toUpper c)
caseString s = try (mapM caseChar s) <?> "\"" ++ s ++ "\""

parseNoArgs inst = do
    skipSpace
    caseString inst
    skipSpace

parseUnary inst = do
    parseNoArgs inst
    src <- decimal
    return src

parseBinary inst = do
    src <- parseUnary inst
    skipSpace
    dst <- decimal
    return (src, dst)

parseTernary inst = do
    (r1, r2) <- parseBinary inst
    skipSpace
    dst <- decimal
    return (r1, r2, dst)

parseNop = do
    parseUnary "nop" 
    return Nop

parseMov = do
    (src, dst) <- parseBinary "mov"
    return $ Mov src dst

parseStr = do
    (val, dst) <- parseBinary "str"
    return $ Str val dst

parseSet = do
    (val, dst) <- parseBinary "set"
    return $ Set val dst

parseLoad = do
    (addr, dst) <- parseBinary "load"
    return $ Load addr dst

parseJmp = do
    dst <- parseUnary "jmp"
    return $ Jmp dst

parseAdd = do
    (r1, r2, dst) <- parseTernary "add"
    return $ Add r1 r2 dst

parseSub = do
    (r1, r2, dst) <- parseTernary "sub"
    return $ Sub r1 r2 dst

parseMul = do
    (r1, r2, dst) <- parseTernary "mul"
    return $ Mul r1 r2 dst

parseBne = do
    (r1, r2, addr) <- parseTernary "bne"
    return $ Bne r1 r2 addr

parseBeq = do
    (r1, r2, addr) <- parseTernary "beq"
    return $ Beq r1 r2 addr

parseInst = parseMov
        <|> parseNop
        <|> parseJmp
        <|> parseAdd
        <|> parseSub
        <|> parseMul
        <|> parseBne
        <|> parseBeq
        <|> parseLoad
        <|> parseStr
        <|> parseSet

parseConfig = do
    (registers, memory) <- parseBinary ""
    return $ fromLists (listOf registers 0) (listOf memory 0)

listOf n v = Prelude.take n . cycle $ [v]

parseFile :: Parser (CPU Int, [Instruction Int])
parseFile = do
    cpu <- parseConfig
    skipSpace
    instructions <- many $ parseInst <* endOfLine
    return (cpu, instructions)
