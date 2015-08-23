module CParser where
import Ast
import Text.ParserCombinators.Parsec

junk :: Parser ()
junk = skipMany space

word :: Parser String
word = many1 letter

keywords = ["int", "struct"]

valDecP :: Parser ValDec
valDecP = do
    tp <- junk >> word
    nm <- junk >> word
    junk >> char ';'
    return $ ValDec tp nm

cParser :: Parser CProg
cParser = do
    junk >> string "int"
    junk >> string "main"
    junk >> char '('
    junk >> char ')'
    junk >> char '{'
    x <- many valDecP
    junk >> char '}'
    return $ ValDecL x
    
runCparser = parse cParser "C parser"
