module Lexical where
import Ast
import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding (many, (<|>))
import Data.Char
import qualified Text.Parsec.Token as Token

cLanguageDef :: Token.LanguageDef ()
cLanguageDef = Token.LanguageDef
        {
            Token.commentStart = "/*",
            Token.commentEnd = "*/",
            Token.commentLine = "//",
            Token.nestedComments = True,
            Token.identStart = letter <|> char '_',
            Token.identLetter = alphaNum <|> char '_',
            Token.opStart = oneOf "<+*-=!>;",
            Token.opLetter = oneOf "<+*-=!>",
            Token.reservedNames = [   
                                      "return",
                                      "if",
                                      "else",
                                      "while",
                                      "for"
                                  ],
            Token.reservedOpNames = [ "+",
                                      "-",
                                      "*",
                                      "=",
                                      "==",
                                      "<=",
                                      "<",
                                      "!=",
                                      ">=",
                                      ";"
                                    ],
            Token.caseSensitive = True
        }

lexer = Token.makeTokenParser cLanguageDef

identifier :: Parser String
identifier = Token.identifier lexer

integer :: Parser Integer
integer = Token.integer lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

braces :: Parser a -> Parser a
braces = Token.braces lexer

assign :: Parser ()
assign = Token.reservedOp lexer $ "="

semicolon :: Parser String
semicolon = Token.semi lexer

coma :: Parser String
coma = Token.comma lexer

equalToken :: Parser ()
equalToken = Token.reservedOp lexer $ "=="

notEqualToken :: Parser ()
notEqualToken = Token.reservedOp lexer $ "!="

greaterToken :: Parser ()
greaterToken = Token.reservedOp lexer $ "<"

greaterEqualToken :: Parser ()
greaterEqualToken = Token.reservedOp lexer $ "<="

lessToken :: Parser ()
lessToken = Token.reservedOp lexer $ ">"

lessEqualToken :: Parser ()
lessEqualToken = Token.reservedOp lexer $ ">="

mulToken :: Parser()
mulToken = Token.reservedOp lexer $ "*"

plusToken :: Parser()
plusToken = Token.reservedOp lexer $ "+"

subToken :: Parser()
subToken = Token.reservedOp lexer $ "-"

