{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Text.Read
import Text.Megaparsec hiding (option)
import Text.Megaparsec.ByteString
import Options.Applicative
import qualified Data.ByteString as B
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import Hasm.Core
import Hasm.Parser

data Config = Config
    { filename     :: String
    , debug        :: Bool
    , memory       :: Int
    , values       :: [Int]
    }

initialized (Config _ _ memory values) = (CPU 0 (initializedRs U.// [(26, (memory - 1))]) (U.replicate memory 0))
    where rs            = U.replicate 32 0
          initializedRs = (rs U.// (zip [0..] values))

configParse :: Options.Applicative.Parser Config
configParse = Config
          <$> strArgument (
                metavar "FILENAME"
             <> help "The hasm file to execute." )
          <*> switch (
                short 'd'
             <> long "debug"
             <> help "Show each CPU step and registers." )
          <*> option auto (
                short 'm'
             <> long "memory"
             <> metavar "M"
             <> help "How much memory the CPU has during execution. Defaults to 0 banks."
             <> value 0 )
          <*> many (argument auto (
                metavar "VALUES..."
             <> help "Initial values, in order, for the registers." ) )

opts = info (helper <*> configParse)
     ( fullDesc
    <> progDesc "Runs a hasm file and prints the final state"
    <> header "hasm - an assembly-like programming language" )

main = do
    config <- execParser opts
    p <- parseFromFile parseFile (filename config)
    case p of
        Left err -> do
            error $ show err
        Right instructions -> do
            let cpu = initialized config
            if (debug config) then do
                putStrLn . unlines . (map show) $ instructions
            else do
                return ()
            _ <- run (debug config) (V.fromList instructions) cpu
            return ()
