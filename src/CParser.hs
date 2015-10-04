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
        params <- brackets1 (sepBy arithExpr coma)
        return $ fun funName params

-- ArithExpr
arithExpr :: Parser ArithExpr
arithExpr = (try term1) `chainl1` try (junk >> (plus <|> minus))
    where
    term1 = (junk >> factor) `chainl1` try (junk >> mul)
    factor :: Parser ArithExpr
    factor =    try (brackets1 arithExpr)
                <|> try intConst 
                <|> (try $ funCall ArithFunCall)
                <|> varName
    mul = char '*' >> return Mul
    plus = char '+' >> return Add
    minus = char '-' >> return Sub
    intConst = IntConst . read <$> many1 digit
    varName = VarName <$> word

-- BoolExpr
boolExpr :: Parser BoolExpr
boolExpr = do
    e1 <- arithExpr
    op <- operator
    e2 <- arithExpr
    return $ op e1 e2
    where
    operator = 
            try (equalOperator >> (return Equal))
        <|> try (greaterEqualOperator >> (return GreaterEqual))
        <|> try (lessEqualOperator >> (return LessEqual))
        <|> try (notEqualOperator >> (return NotEqual))
        <|> try (greaterOperator >> (return Greater))
        <|> (lessOperator >> (return Less))

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
    manyStatements = brackets2 $ many (try statement)
    oneStatement = (: []) <$> statement

conditional :: Parser Statement
conditional = do
    junk >> string "if"
    ifExpr <- brackets1 boolExpr
    ifStatements <- bodyStatement
    elseStatements <- junk >> option [] elseParser
    return $ ConditionalIfElse ifExpr ifStatements elseStatements
    where
    elseParser = do
        string "else"
        bodyStatement

statFunCall :: Parser Statement
statFunCall = do
    x <- funCall StatFunCall
    semicolon
    return x

whileLoop :: Parser Statement
whileLoop = do
    junk >> string "while"
    whileArithExpr <- brackets1 boolExpr
    statements <- bodyStatement
    return $ WhileLoop whileArithExpr statements

functionReturn :: Parser Statement
functionReturn = do
    junk >> string "return"
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

functionDecl = FunctionDecl <$> identifier <*> identifier <*> brackets1 params
function = Function <$> functionDecl <*> brackets2 (many (try statement))

cParser :: Parser ParseTree
cParser = do 
    x <- (many1 $ try function)
    junk >> eof
    return x
    
runCparser = parse cParser "C parser"
