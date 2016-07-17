module CParser where
import Ast
import Lexical
import Text.ParserCombinators.Parsec
import Data.List
import Control.Applicative hiding (many, (<|>))
import Debug.Trace (trace)


funCall :: (Name -> [ArithExpr] -> a) -> Parser a
funCall fun = do
        funName <- identifier
        params <- parens (sepBy arithExpr coma)
        return $ fun funName params

-- ArithExpr
arithExpr :: Parser ArithExpr
arithExpr = (try term1) `chainl1` try (plus <|> minus)
    where
    term1 = factor `chainl1` try mul
    factor :: Parser ArithExpr
    factor =    try (parens arithExpr)
                <|> try intConst 
                <|> (try $ funCall ArithFunCall)
                <|> varName
    mul = mulToken >> return Mul
    plus = plusToken >> return Add
    minus = subToken >> return Sub
    intConst = IntConst <$> integer
    varName = VarName <$> identifier

-- BoolExpr
boolExpr :: Parser BoolExpr
boolExpr = do
    e1 <- arithExpr
    op <- operator
    e2 <- arithExpr
    return $ op e1 e2
    where
    operator = 
            try (equalToken >> (return Equal))
        <|> try (greaterEqualToken >> (return GreaterEqual))
        <|> try (lessEqualToken >> (return LessEqual))
        <|> try (notEqualToken >> (return NotEqual))
        <|> try (greaterToken >> (return Greater))
        <|> (lessToken >> (return Less))

-- Statement
valDec :: Parser Statement
valDec = do
    tp <- identifier
    nm <- identifier
    semicolon
    return $ ValDec tp nm

assignment :: Parser Statement
assignment = do
    varname <- identifier
    assign
    rest <- arithExpr
    semicolon
    return $ Assignment varname rest

valDef :: Parser Statement
valDef = do
    tp <- identifier
    nm <- identifier
    assign
    rest <- arithExpr
    semicolon
    return $ ValDef tp nm rest

bodyStatement = try manyStatements <|> oneStatement
    where
    manyStatements = braces $ many (try statement)
    oneStatement = (: []) <$> statement

conditional :: Parser Statement
conditional = do
    reserved "if"
    ifExpr <- parens boolExpr
    ifStatements <- bodyStatement
    elseStatements <- option [] elseParser
    return $ ConditionalIfElse ifExpr ifStatements elseStatements
    where
    elseParser = do
        reserved "else"
        bodyStatement

statFunCall :: Parser Statement
statFunCall = do
    x <- funCall StatFunCall
    semicolon
    return x

whileLoop :: Parser Statement
whileLoop = do
    reserved "while"
    whileArithExpr <- parens boolExpr
    statements <- bodyStatement
    return $ WhileLoop whileArithExpr statements

functionReturn :: Parser Statement
functionReturn = do
    reserved "return"
    res <- arithExpr
    semicolon
    return $ FunctionReturn res

statement :: Parser Statement
statement = try conditional 
        <|> try whileLoop 
        <|> try functionReturn 
        <|> try valDef
        <|> try assignment 
        <|> try statFunCall
        <|> valDec 

params :: Parser [Parameter]
params = sepBy (Parameter <$> identifier <*> identifier) coma

functionDecl = FunctionDecl <$> identifier <*> identifier <*> parens params
function = Function <$> functionDecl <*> braces (many (try statement))

cParser :: Parser ParseTree
cParser = do 
    x <- (many1 $ try function)
    eof
    return x
    
runCparser = parse cParser "C parser"
