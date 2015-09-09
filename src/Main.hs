{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Read
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString as B
import qualified Data.Vector as V

import VM.Core
import VM.Parser

initialize (CPU c rs mem) xs = (CPU c (rs V.// (zip [0..] xs)) mem)

main = do
    (filename:xs) <- getArgs
    file <- B.readFile filename
    case parseOnly parseFile file of
        Left err -> putStrLn $ "Error parsing: " ++ err
        Right (cpu, instructions) -> do
            let initialized = initialize cpu (map read xs)
            putStrLn $ "Current CPU: \n" ++ (show initialized)
            putStrLn . unlines . (map show) $ instructions
            runPrint (V.fromList instructions) initialized
