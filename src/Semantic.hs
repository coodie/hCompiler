module Semantic
    (validate
    ,ValidationResult)
    where

import Ast
import Control.Monad

type ScopeBody = [Statement]
type ScopeVariables = [String]
type ValidationResult = Either String ScopeVariables

validate :: ParseTree -> Maybe String
validate (x:xs) = 
    let Function (FunctionDecl _ funName params) body = x 
        names = map (\ (Parameter _ name) -> name) params in
        case scopeResolution body names of
            Left str -> Just str
            Right _ -> validate xs
validate [] = Nothing



scopeResolution :: ScopeBody -> ScopeVariables -> ValidationResult
scopeResolution (x:xs) vars = 
    case x of 
        ValDec _ name -> checkForRedeclaration name vars >>= scopeResolution xs
        ValDef _ name _ -> checkForRedeclaration name vars >>= scopeResolution xs
        Assignment name expr -> checkForExistance name vars >>= checkArithExpr expr >>= scopeResolution xs
        WhileLoop expr body -> checkBoolExpr expr vars >>= scopeResolution body >>= scopeResolution xs
        ConditionalIfElse expr ifBody elseBody -> checkBoolExpr expr vars >>= scopeResolution ifBody >>= scopeResolution elseBody >>= scopeResolution xs
        _ -> scopeResolution xs vars
    where
    checkForRedeclaration name vars = 
                if elem name vars then 
                    Left $ "Variable " ++ name ++ " redeclaration"
                else
                    Right (name:vars)
    checkForExistance name vars = 
                if not $ elem name vars then 
                    Left $ "Variable " ++ name ++ " undeclared"
                else
                    Right vars
    checkArithExpr x vars = case x of
        IntConst _ -> Right vars
        VarName name -> checkForExistance name vars 
        ArithFunCall name body -> foldM (flip $ checkArithExpr) vars body
        Add e1 e2 -> checkArithExpr e1 vars >>= checkArithExpr e2
        Sub e1 e2 -> checkArithExpr e1 vars >>= checkArithExpr e2
        Mul e1 e2 -> checkArithExpr e1 vars >>= checkArithExpr e2

    checkBoolExpr x vars = case x of
        Equal e1 e2 -> checkArithExpr e1 vars >>= checkArithExpr e2
        NotEqual e1 e2 -> checkArithExpr e1 vars >>= checkArithExpr e2
        Greater e1 e2 -> checkArithExpr e1 vars >>= checkArithExpr e2
        GreaterEqual e1 e2 -> checkArithExpr e1 vars >>= checkArithExpr e2
        Less e1 e2 -> checkArithExpr e1 vars >>= checkArithExpr e2
        LessEqual e1 e2 -> checkArithExpr e1 vars >>= checkArithExpr e2
        
scopeResolution [] _ = Right []
