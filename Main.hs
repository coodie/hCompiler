module Main where
import System.IO
import Ast
import CParser

main = do
    input <- getContents
    print $ runCparser input

