module Ast where


data CProg = 
    Functions [Function]
    deriving (Show)

data FunctionDecl =
    FunctionDecl Type Name [Parameter]
    deriving (Show)

data Function =
    Function FunctionDecl [Statement]
    deriving (Show)

data Statement =
    ValDec Type Name | 
    Assignment Var Expr |
    ConditionalIf Expr [Statement] |
    ConditionalIfElse Expr [Statement] [Statement] |
    WhileLoop Expr [Statement] |
    StatExpr Expr
    deriving (Show)

data Expr = 
    StrConst String | 
    IntConst Integer |
    BoolConst Bool |
    VarName String |
    Add Expr Expr |
    Sub Expr Expr |
    Div Expr Expr |
    Mul Expr Expr |
    Equal Expr Expr |
    NotEqual Expr Expr | 
    FunCall Name [Expr]
    deriving (Show)

data Parameter =
    Parameter Type Name
    deriving (Show)



type Type = String
type Name = String
type Var = String
