{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Read
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Options.Applicative
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

configParse :: Parser Config
configParse = Config
          <$> strArgument (
                metavar "FILENAME"
             <> help "The hasm file to execute." )
          <*> switch (
                short 'd'
             <> long "debug"
             <> help "Show each CPU step and registers." )
          <*> some (argument auto (
                metavar "VALUES..."
             <> help "Initial values, in order, for the registers." ))

opts = info (helper <*> configParse)
     ( fullDesc
    <> progDesc "Runs a hasm file and prints the final state"
    <> header "hasm - an assembly-like programming language" )

main = do
    config <- execParser opts
    file <- (B.readFile . filename) config
    case parseOnly parseFile file of
        Left err -> putStrLn $ "Error parsing: " ++ err
        Right (cpu, instructions) -> do
            let initialized = initialize cpu (values config)
            putStrLn $ "Starting CPU: \n" ++ (show initialized)
            if (debug config) then do
                putStrLn . unlines . (map show) $ instructions
            else do
                return ()
            final <- run (debug config) (V.fromList instructions) initialized
            putStrLn $ "Final CPU: \n" ++ (show final)
