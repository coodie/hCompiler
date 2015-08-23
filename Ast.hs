module Ast where

data CProg = 
    ValDecL [ValDec]
    deriving (Show)

data ValDec = 
    ValDec Type VarName
    deriving (Show)

type Type = String
type VarName = String
