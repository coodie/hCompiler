module Lexical where
import Ast
import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding (many, (<|>))
import Data.Char

junk :: Parser ()
junk = spaces <?> "junk"

word :: Parser String
word = many1 $ satisfy (\x -> isAlpha x || x == '_')

identifier :: Parser String
identifier = junk >> word 

brackets1 :: Parser a -> Parser a
brackets1 p = do
    junk >> char '('
    res <- p
    junk >> char ')'
    return res

brackets2 :: Parser a -> Parser a
brackets2 p = do
    junk >> char '{'
    res <- p
    junk >> char '}'
    return res

assign :: Parser Char
assign = junk >> char '='

semicolon :: Parser ()
semicolon = junk >> char ';' >> return ()

coma :: Parser ()
coma = junk >> char ',' >> return ()

strConst :: Parser Expr
strConst = junk >> do 
    char '\"'
    x <- many $ noneOf "\""
    char '\"'
    return $ StrConst x

intConst :: Parser Expr
intConst = junk >> IntConst . read <$> many1 digit

boolConst :: Parser Expr
boolConst = junk >> 
    ((string "true" >> return (BoolConst True)) <|>
    (string "false" >> return (BoolConst False) ))

equalOperator :: Parser String
equalOperator = junk >> string "=="

notEqualOperator :: Parser String
notEqualOperator = junk >> string "!="

greaterOperator :: Parser Char
greaterOperator = junk >> char '<'
