{-# LANGUAGE OverloadedStrings #-}

import Parser
import System.Environment
import Data.Attoparsec.Char8
import qualified Data.ByteString as B

main = do
    [filename] <- getArgs
    file <- B.readFile filename
    case parseOnly parseFile file of
        Left err -> putStrLn $ "Error parsing: " ++ err
        Right instructions -> print instructions
