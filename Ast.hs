module Ast where


type ParseTree = [Function]

data FunctionDecl =
    FunctionDecl Type Name [Parameter]
    deriving (Show)

data Function =
    Function FunctionDecl [Statement]
    deriving (Show)

data Statement =
      ValDec Type Name
    | Assignment Var ArithExpr
    | ValDef Type Name ArithExpr
    | ConditionalIfElse BoolExpr [Statement] [Statement]
    | WhileLoop BoolExpr [Statement]
    | StatFunCall Name [ArithExpr]
    | FunctionReturn ArithExpr
    deriving (Show)

data BoolExpr = 
      Equal ArithExpr ArithExpr
    | NotEqual ArithExpr ArithExpr 
    | Greater ArithExpr ArithExpr
    | GreaterEqual ArithExpr ArithExpr
    | Less ArithExpr ArithExpr
    | LessEqual ArithExpr ArithExpr
    deriving (Show)

data ArithExpr = 
      IntConst Integer
    | VarName String
    | ArithFunCall Name [ArithExpr]
    | Add ArithExpr ArithExpr
    | Sub ArithExpr ArithExpr
    | Mul ArithExpr ArithExpr
    deriving (Show)

data Parameter =
    Parameter Type Name
    deriving (Show)

type Type = String
type Name = String
type Var = String
