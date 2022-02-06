module Main where
    
import Parser
import Interpreter

import Data.Char(toUpper)
import Control.Monad

main :: IO()
main = do
    printImpLogo
    putStrLn "Chose one option to continue:"
    choice <- getLine;
    case choice of

        "1" -> do
            doWhileInsert
            putStrLn "Interpreter terminated with code <-1>"

        "2" -> do
            putStrLn "Insert the path to the file you want to use!"
            file_path <- getLine;
            input <- readFile file_path
            parserInput input


printImpLogo :: IO()
printImpLogo = do
            putStrLn " _   __ _ _                   "
            putStrLn "| | / /| (_)                  "
            putStrLn "| |/ / | |_ _ __ ___  _ __    "
            putStrLn "|    \\ | | | '_ ` _ \\| '_ \\   "
            putStrLn "| |\\  \\| | | | | | | | |_) |  "
            putStrLn "\\_| \\_/|_|_|_| |_| |_| .__/   "
            putStrLn "                     | |      "
            putStrLn "                     |_|F.R.      "
            putStrLn "                              "
            putStrLn "1) Use command Line parser "
            putStrLn "2) Load file "
            putStrLn "                              "

--
doWhileInsert :: IO()
doWhileInsert = do
            putStrLn "__________________________________"
            putStrLn "Insert the program or ESC to exit"
            input <- getLine;

            unless (map toUpper input == "ESC") $ do
            parserInput input
            doWhileInsert


--
parserInput :: String -> IO()
parserInput input = do
                let inputParsed = executeParser input
                if snd inputParsed == ""
                then do
                  let state = emptyState
                  let state' = executeCommands state (fst inputParsed)

                  --
                  putStrLn ""
                  putStrLn " -- INPUT PARSED -- "
                  putStrLn ""
                  let parsed = fst inputParsed
                  mapM_ print parsed

                  --
                  putStrLn ""
                  putStrLn " -- INPUT NOTPARSED -- "
                  putStrLn ""
                  let notParsed = snd inputParsed
                  mapM_ print notParsed
                  --
                  putStrLn ""
                  putStrLn " -- STATE -- "
                  putStrLn ""
                  print state'
                  putStrLn ""

                else do
                  putStrLn ""
                  print inputParsed
                  putStrLn ""
                  error "Parse failed"
                  putStrLn ""
