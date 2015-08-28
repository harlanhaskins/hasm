{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString as B

import VM.Core
import VM.Parser

main = do
    [filename] <- getArgs
    file <- B.readFile filename
    case parseOnly parseFile file of
        Left err -> putStrLn $ "Error parsing: " ++ err
        Right (cpu, instructions) -> do
            putStrLn $ "Current CPU: \n" ++ (show cpu)
            putStrLn . unlines . (map show) $ instructions
            putStrLn $ "Final CPU: \n" ++ (show $ run (fromInstructions instructions) cpu)
