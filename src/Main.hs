module Main where
import System.IO
import System.Environment
import System.Process
import System.Exit
import Data.List (init, dropWhileEnd)
import Ast
import Assembly
import CParser
import Text.Show.Pretty
import Control.Monad

main = do
    args <- getArgs
    input <- readFile (head args) 
    let parseResult = runCparser input
    let justParse = elem "-t" args 
    let rawFileName = init $ dropWhileEnd ( /= '.') (head args)
    return ()
    case parseResult of
        Left error -> do
            putStrLn $ ppShow error
        Right tree -> 
                if justParse then 
                    dumpParseTree tree
                else do
                    writeFile (rawFileName ++ ".s") (runAssembly tree)
                    runAs rawFileName
                    runLinker rawFileName

dumpParseTree :: ParseTree -> IO ()
dumpParseTree tree = putStr $ ppShow tree

dumpAssembly :: ParseTree -> IO ()
dumpAssembly tree = putStr $ runAssembly tree

runAs :: String -> IO ()
runAs filename = do
    let asArgs = [filename ++ ".s", "--32", "-o", filename ++ ".o"]
    (asExit, asStdOut, asStdErr) <- readProcessWithExitCode "as" asArgs ""
    case asExit of 
        ExitSuccess -> return ()
        ExitFailure _ -> putStr asStdOut

runLinker :: String -> IO ()
runLinker filename = do
    let linkerArgs = ["-m32",filename ++ ".o", "-o", filename]
    (linkerExit, linkerStdOut, linkerStdErr) <- readProcessWithExitCode "gcc" linkerArgs ""
    case linkerExit of 
        ExitSuccess -> return ()
        ExitFailure _ -> putStr linkerStdOut
    

