module Main where
import System.IO
import System.Environment
import Ast
import Assembly
import CParser
import Text.Show.Pretty
import Control.Monad

main = do
    args <- getArgs
    input <- getContents
    let parseResult = runCparser input
    case parseResult of
        Left error -> do
            when (not $ elem "--no-error" args) $ putStr $ ppShow error
        Right tree -> do
            when (elem "-S" args) (dumpAssembly tree)
            when (elem "-t" args) (dumpParseTree tree)
    return ()

dumpParseTree :: ParseTree -> IO ()
dumpParseTree tree = putStr $ ppShow tree

dumpAssembly :: ParseTree -> IO ()
dumpAssembly tree = putStr $ runAssembly tree
    

