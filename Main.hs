module Main where
import System.IO
import Ast
import CParser
import Text.Show.Pretty

main = do
    input <- getContents
    putStr $ ppShow $ runCparser input

