{-# LANGUAGE OverloadedStrings #-}

import Parser
import System.Environment
import VM
import Data.Attoparsec.ByteString.Char8 (parseOnly)
import qualified Data.ByteString as B

listOf n v = take n . cycle $ [v]

cpu = fromLists (listOf 3 0) (listOf 10 0)

main = do
    [filename] <- getArgs
    putStrLn $ "Current CPU: \n" ++ (show cpu)
    file <- B.readFile filename
    case parseOnly parseFile file of
        Left err -> putStrLn $ "Error parsing: " ++ err
        Right instructions -> print $ run (fromInstructions instructions) cpu
