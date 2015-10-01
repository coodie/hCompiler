module Assembly where
import Control.Monad.State
import Ast

charSize = 1 :: Int
intSize = 4 :: Int

data Asm = Asm 
    { variable :: [(String, Int)] -- (Stack offset, Name)
    ,  output :: String 
    } deriving Show

appendToOutput str (Asm vars output) = 
    Asm vars (output ++ str)

asmPut :: String -> State Asm ()
asmPut str = modify $ appendToOutput (str ++ "\n")

getVarStackOffset :: String -> State Asm Int
getVarStackOffset varName = do
    (Asm vars _) <- get 
    let Just x = lookup varName vars 
    return x

putNewVariable :: Name -> Int -> State Asm ()
putNewVariable name offset = do
    (Asm vars _) <- get 
    if null vars then
        do
        modify $ putVar (name, -offset)
    else
        do
        (Asm (hd:vars) _ ) <- get
        let lastOffset = snd hd
        modify $ putVar (name, lastOffset-offset)
    return ()
    where
    putVar x@(name, off) (Asm vars output) = Asm (x:vars) output
            

    

functionEnter :: State Asm ()
functionEnter = do 
    asmPut  "pushl %esp"  
    asmPut  "movl %esp, %ebp"
    return ()
    
functionLeave :: State Asm ()
functionLeave = do 
    asmPut "popl %ebp"
    asmPut "ret"
    return ()

evalExpr :: Expr -> State Asm ()
evalExpr (Add e1 e2) = do
    evalExpr e1
    asmPut "movl %eax, %ebx"
    evalExpr e2
    asmPut "add %ebx, %eax"

evalExpr (Mul e1 e2) = do
    evalExpr e1
    asmPut "movl %eax, %ebx"
    evalExpr e2
    asmPut "mul %eax, %ebx"

evalExpr (IntConst e) = do
    let val = show e
    asmPut $ "movl $" ++ val ++ ", %eax"

evalExpr (VarName e) = do
    offset <- getVarStackOffset e >>= return . show
    asmPut $ "movl " ++ offset ++ "(%ebp), %eax"
    return ()

evalExpr (Equal e1 e2) = do
    evalExpr e1
    asmPut "movl %eax, %ebx"
    evalExpr e2
    asmPut "subl %ebx, %eax"

evalExpr (FunCall e1 e2) = do
    evalArgs e2
    asmPut $ "call " ++ e1
    popArgs $ length e2
    return ()
    where
    evalArgs [] = return ()
    evalArgs (x:xs) = do
        evalExpr x
        asmPut "pushl %eax"
        evalArgs xs
    popArgs 0 = return ()
    popArgs n = asmPut "popl %ebx" >> (popArgs (n-1))

statement :: Statement -> State Asm ()
statement (ValDec typeName varName) = 
    case typeName of
        "int" -> putNewVariable varName 4
        "char" -> putNewVariable varName 1 

statement (Assignment varName expr) = do
    evalExpr expr
    offset <- getVarStackOffset varName >>= return . show
    asmPut $ "movl %eax, " ++ offset ++ "(%ebp)" 

statement (StatExpr expr) = evalExpr expr






--evalExpr (NotEqual e1 e2) = []
    


--evalExpr (BoolConst e) = []
--evalExpr (Sub e1 e2) = []
--
