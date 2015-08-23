module CParser where
import Ast
import Lexical
import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding (many, (<|>) )

valDec :: Parser Statement
valDec = do
    tp <- identifier
    nm <- identifier
    semicolon
    return $ ValDec tp nm

expr :: Parser Expr
expr = try strConst <|> try intConst <|> try boolConst <|> funCall

funCall :: Parser Expr
funCall = FunCall <$> identifier <*> (brackets1 (sepBy (try expr) coma))

assignment :: Parser Statement
assignment = do
    varname <- identifier
    junk >> char '='
    rest <- expr
    semicolon
    return $ Assignment varname rest

statement :: Parser Statement
statement = try valDec <|> assignment 

params :: Parser [Parameter]
params = sepBy (Parameter <$> identifier <*> identifier) coma

functionDecl = FunctionDecl <$> identifier <*> identifier <*> (brackets1 params)
function = Function <$> functionDecl <*> brackets2 (many $ try statement)

cParser :: Parser CProg
cParser = do 
    x <- Functions <$> (many1 $ try function)
    junk >> eof
    return x
    
runCparser = parse cParser "C parser"
