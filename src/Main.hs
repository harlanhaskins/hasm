{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Read
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import Hasm.Core
import Hasm.Parser

data Config = Config
    { filename     :: String
    , debug        :: Bool
    , values       :: [Int]
    }

initialize (CPU c rs mem) xs = (CPU c (rs U.// (zip [0..] xs)) mem)

parseArguments :: [String] -> Config
parseArguments ("-d":fn:xs) = Config fn True  (map read xs)
parseArguments (     fn:xs) = Config fn False (map read xs)

main = do
    args <- getArgs
    let config = parseArguments args
    file <- (B.readFile . filename) config
    case parseOnly parseFile file of
        Left err -> putStrLn $ "Error parsing: " ++ err
        Right (cpu, instructions) -> do
            let initialized = initialize cpu (values config)
            putStrLn $ "Starting CPU: \n" ++ (show initialized)
            if (debug config) then do
                putStrLn . unlines . (map show) $ instructions
                runPrint (V.fromList instructions) initialized
            else do
                let final = run (V.fromList instructions) initialized
                putStrLn $ "Final CPU: \n" ++ (show final)
