module CParser where
import Ast
import Lexical
import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding (many, (<|>))
import Debug.Trace (trace)

expr :: Parser Expr
expr = term1 `chainl1` (plus <|> minus)
    where
    term1 = term2 `chainl1` mul
    term2 = factor `chainl1` boolOp
    factor =    try strConst <|> 
                try intConst <|> 
                try funCall <|> 
                try varName <|> 
                brackets1 expr
    mul = junk >> char '*' >> return Mul
    plus = junk >> char '+' >> return Add
    minus = junk >> char '-' >> return Sub
    boolOp =        (try equalOperator >> return Equal) 
              <|>   (try notEqualOperator >> return NotEqual)
              <|>   (greaterOperator >> return Greater)

varName :: Parser Expr
varName = VarName <$> identifier

funCall :: Parser Expr
funCall = 
    FunCall <$> identifier <*> brackets1 (sepBy (try expr) coma)

valDec :: Parser Statement
valDec = do
    tp <- identifier
    nm <- identifier
    semicolon
    return $ ValDec tp nm

valDef :: Parser Statement
valDef = do
    tp <- identifier
    nm <- identifier
    assign
    rest <- expr
    semicolon
    return $ ValDef tp nm rest


assignment :: Parser Statement
assignment = do
    varname <- identifier
    assign
    rest <- expr
    semicolon
    return $ Assignment varname rest

bodyStatement = try manyStatements <|> oneStatement
    where
    manyStatements = brackets2 $ many (try statement)
    oneStatement = (: []) <$> statement

conditional :: Parser Statement
conditional = do
    junk >> string "if"
    ifExpr <- brackets1 expr
    ifStatements <- bodyStatement
    elseStatements <- junk >> option [] elseParser
    if null elseStatements then
            return $ ConditionalIf ifExpr ifStatements
        else
            return $ ConditionalIfElse ifExpr ifStatements elseStatements
    where
    elseParser = do
        string "else"
        bodyStatement

whileLoop :: Parser Statement
whileLoop = do
    junk >> string "while"
    whileExpr <- brackets1 expr
    statements <- bodyStatement
    return $ WhileLoop whileExpr statements

statExpr :: Parser Statement
statExpr = do 
    res <- expr
    semicolon
    return $ StatExpr res

functionReturn :: Parser Statement
functionReturn = do
    junk >> string "return"
    res <- expr
    semicolon
    return $ FunctionReturn res

statement :: Parser Statement
statement = try conditional 
        <|> try whileLoop 
        <|> try functionReturn 
        <|> try statExpr
        <|> try valDef
        <|> try assignment 
        <|> valDec 

params :: Parser [Parameter]
params = sepBy (Parameter <$> identifier <*> identifier) coma

functionDecl = FunctionDecl <$> identifier <*> identifier <*> brackets1 params
function = Function <$> functionDecl <*> brackets2 (many (try statement))

cParser :: Parser ParseTree
cParser = do 
    x <- (many1 $ try function)
    junk >> eof
    return x
    
runCparser = parse cParser "C parser"
