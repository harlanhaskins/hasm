module Hasm.Parser where
import qualified Data.ByteString.Char8 as BSC
import Control.Applicative
import Control.Monad (void)
import Data.Char
import qualified Data.Map as M
import Data.Either
import qualified Data.Vector.Unboxed as V
import Text.Megaparsec (try, (<?>))
import Text.Megaparsec.ByteString
import Text.Megaparsec.Combinator as MPC
import Text.Megaparsec.Prim
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Lexer as L

import Hasm.Core

infixl <||>
p <||> q = (try p) <|> (try q)

choice' = choice . map try

nonNewlineSpace = C.satisfy (\c -> isSpace c && (c /= '\n'))

comment = L.skipLineComment ";"
blockComment = L.skipBlockComment "/*" "*/"

space :: Parser ()
space = L.space (void nonNewlineSpace) comment blockComment

space' :: Parser ()
space' = L.space (void C.spaceChar) comment blockComment

symbol = L.symbol' space

lexeme        = L.lexeme space
integer       = lexeme L.integer
signedInteger = L.signed space integer

identifierChar = (C.alphaNumChar <||> (C.oneOf "*+-/_'=^?!<>"))
identifier = some identifierChar <?> "identifier"

parseNamedReg (name, r) = do
    symbol name
    return r

regSection c off end = map (\(n, off') -> (c:(show n), Reg off')) (zip [0..] [off..end])

regNames = [("ra", Reg 31), ("sp", Reg 26), ("_", Reg 30)]
         ++ (regSection 'v' 2 3)
         ++ (regSection 'a' 4 7)
         ++ (regSection 't' 8 17)
         ++ (regSection 's' 18 25)

parseNormalReg = Reg . fromIntegral <$> ((C.char 'r') *> integer <* space)
parseSpecialReg = map parseNamedReg regNames
parseReg = (choice' parseSpecialReg <||> parseNormalReg) <?> "register name"

parseVal = (Val . fromIntegral <$> signedInteger) <?> "value"

parseLbl = (Lbl <$> lexeme identifier) <?> "label"

parseAddr = (Addr . fromIntegral <$> integer) <?> "address"

parseLabel = do
    text <- lexeme identifier
    symbol ":"
    return $ Lbl text

parseJmpLabel = parseLbl
           <||> parseAddr

parseArg = (parseReg <?> "register name")
      <||> (parseVal <?> "value")

parseUnary inst = do
    symbol inst
    arg <- parseArg
    return arg

parseBinary inst = do
    symbol inst
    arg1 <- parseArg
    arg2 <- parseArg
    return (arg1, arg2)

parseTernary inst = do
    symbol inst
    arg1 <- parseReg
    arg2 <- parseArg
    arg3 <- parseArg
    return (arg1, arg2, arg3)

fromTernaryTuple (inst, c) = do
    (dst, r1, r2) <- parseTernary inst
    return $ c dst r1 r2

parseBranch inst c = do
    symbol inst
    loc <- parseArg
    arg <- parseArg
    addr <- parseJmpLabel
    return (loc, arg, addr)

fromBranchTuple (inst, c) = do
    (r1, r2, addr) <- parseBranch inst c
    return $ c r1 r2 addr

parsePseudoBranchZero inst c = do
    symbol inst
    reg <- parseReg
    addr <- parseJmpLabel
    return $ c reg (Val 0) addr

fromPseudoBranchTuple (inst, c) = parsePseudoBranchZero inst c

ternaryParsers = map fromTernaryTuple [("add", Add)
                                      ,("and", And)
                                      ,("or", Or)
                                      ,("xor", Xor)
                                      ,("sll", Sll)
                                      ,("srl", Srl)
                                      ,("sub", Sub)
                                      ,("div", Div)
                                      ,("mod", Mod)
                                      ,("mul", Mul)]

branchParsers = map fromBranchTuple [("beq", Beq)
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
    symbol "nop"
    return Nop

parseRet = do
    symbol "ret"
    return $ Jr (Reg 31)

parseInc = do
    symbol "inc"
    reg <- parseReg
    return $ Add reg reg (Val 1)

parseDec = do
    symbol "dec"
    reg <- parseReg
    return $ Sub reg reg (Val 1)

parseMov = do
    symbol "mov"
    dst <- parseReg
    src <- parseArg
    return $ Mov dst src

parseSyscall = do
    arg <- parseUnary "syscall"
    return $ Syscall arg

parsePush = do
    val <- parseUnary "push"
    return $ [ Str val (Reg 26)
             , Sub (Reg 26) (Reg 26) (Val 1)
             ]

parsePop = do
    dst <- parseUnary "pop"
    return $ [ Add (Reg 26) (Reg 26) (Val 1)
             , Ld dst (Reg 26)
             ]

parseMovl = do
    symbol "movl"
    dst <- parseReg
    src <- parseJmpLabel
    return $ Movl dst src

parseJmp = do
    symbol "jmp"
    dst <- parseJmpLabel
    return $ Jmp dst

parseCall = do
    symbol "call"
    dst <- parseJmpLabel
    return $ Call dst

parseJr = do
    symbol "jr"
    dst <- parseReg
    return $ Jr dst

parseMemInst inst c = do
    symbol inst
    dst <- parseReg
    src <- parseReg
    return $ c dst src

parseIntoList p = (:[]) <$> p

parseStr = parseMemInst "str" Str
parseLd = parseMemInst "ld" Ld

parseSingleInst = parseMov  <||> parseNop
             <||> parseJmp  <||> parseCall
             <||> parseMovl <||> parseJr
             <||> parseRet  <||> parseInc
             <||> parseDec  <||> parseLd
             <||> parseStr  <||> parseSyscall
             <||> (choice' ternaryParsers)
             <||> (choice' branchParsers)
             <||> (choice' pseudoBranchParsers)

parseInst = (parseIntoList parseSingleInst)
       <||> parsePush
       <||> parsePop

parseCmd = ((Right <$> parseInst)  <?> "instruction")
      <||> ((Left  <$> parseLabel) <?> "label")

flattenRights :: [Either a [b]] -> [Either a b]
flattenRights [] = []
flattenRights ((Left l):xs) = (Left l):(flattenRights xs)
flattenRights ((Right rs):xs) = map return rs ++ (flattenRights xs)

partitionLabels :: [Either Label [Instruction]] -> ([(String, Int)], [Instruction])
partitionLabels = partitionEithers . map fix . indexRights . flattenRights
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

parseFile :: Parser [Instruction]
parseFile = do
    lines <- many (space' *> parseCmd <* C.eol)
    eof
    return $ replaceLabels lines
