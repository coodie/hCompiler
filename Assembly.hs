module Assembly where
import Control.Monad.State
import Data.List
import Ast

charSize = 1 :: Int
intSize = 4 :: Int

data Asm = Asm 
    { variable :: [(String, Int)] -- (Stack offset, Name)
    ,  output :: String 
    ,  label :: Int
    } deriving Show

appendToOutput str asm =
    asm {output = (output asm) ++ str}

asmPut :: String -> State Asm ()
asmPut str = modify $ appendToOutput (str ++ "\n")

getVarStackOffset :: String -> State Asm Int
getVarStackOffset varName = do
    vars <- gets variable 
    let Just x = lookup varName vars 
    return x

cleanVariables :: State Asm ()
cleanVariables =
    modify $ \(Asm vars output label) -> Asm [] output label

putVar :: (String, Int) -> Asm -> Asm
putVar x@(name, off) (Asm vars output label) = Asm (x:vars) output label

putNewVariable :: Name -> Int -> State Asm ()
putNewVariable name offset = do
    vars <- gets variable 
    if null vars 
        then 
            modify $ putVar (name, -offset)
        else do
            (hd:vars) <- gets variable
            let lastOffset = snd hd
            modify $ putVar (name, lastOffset-offset)

putOffsetBorder :: State Asm ()
putOffsetBorder = do
    vars <- gets variable
    modify $ putVar ("0_border",0)

newLabelNumber :: State Asm Int
newLabelNumber = do
    modify $ \(Asm vars output label) -> Asm vars output (label+1)
    newLabel <- gets label
    return newLabel


functionEnter :: State Asm ()
functionEnter = do 
    asmPut  "pushl %esp"  
    asmPut  "movl %esp, %ebp"
    
functionLeave :: State Asm ()
functionLeave = do 
    asmPut "popl %ebp"
    asmPut "ret"

-- Expression
expression :: Expr -> State Asm ()
expression (Add e1 e2) = do
    expression e1
    asmPut "movl %eax, %ecx"
    expression e2
    asmPut "add %ecx, %eax"

expression (Sub e1 e2) = do
    expression e1
    asmPut "movl %eax, %edx"
    expression e2
    asmPut "sub %eax, %edx"
    asmPut "movl %edx, %eax"

expression (Mul e1 e2) = do
    expression e1
    asmPut "movl %eax, %ebx"
    expression e2
    asmPut "imul %ebx, %eax"

expression (IntConst e) = do
    let val = show e
    asmPut $ "movl $" ++ val ++ ", %eax"

expression (VarName e) = do
    offset <- liftM show $ getVarStackOffset e
    asmPut $ "movl " ++ offset ++ "(%ebp), %eax"

expression (Equal e1 e2) = do
    expression e1
    asmPut "movl %eax, %ebx"
    expression e2
    asmPut "cmp %eax, %ebx"

expression (NotEqual e1 e2) = expression (Equal e1 e2)
expression (Greater e1 e2) = expression (Equal e1 e2)


expression (FunCall e1 e2) = do
    evalArgs e2
    asmPut $ "call " ++ e1
    asmPut $ "addl $" ++ (show (4 * length e2)) ++ ", %esp"
    where
    evalArgs [] = return ()
    evalArgs (x:xs) = do
        evalArgs xs
        expression x
        asmPut "pushl %eax"

-- Statement
getJumpInstr expr = 
    case expr of
        (Equal _ _) -> "jne "
        (NotEqual _ _) -> "je "
        (Greater _ _) -> "jg "
        _ -> error "wrong expression type"

statement :: Statement -> State Asm ()
statement (ValDec typeName varName) = 
    case typeName of
        "int" -> putNewVariable varName intSize
        "char" -> putNewVariable varName charSize

statement (Assignment varName expr) = do
    offset <- liftM show $ getVarStackOffset varName 
    expression expr
    asmPut $ "movl %eax, " ++ offset ++ "(%ebp)" 

statement (ValDef typeName varName expr) = do
    case typeName of
        "int" -> putNewVariable varName intSize
        "char" -> putNewVariable varName charSize
    offset <- liftM show $ getVarStackOffset varName 
    expression expr
    asmPut $ "movl %eax, " ++ offset ++ "(%ebp)" 

statement (StatExpr expr) = expression expr

statement (FunctionReturn expr) = do
    expression expr
    functionLeave

statement (WhileLoop expr whileBody) = do
    labelNumber <- liftM show newLabelNumber
    let enterLabel = "while" ++ labelNumber ++ "enter"
    let exitLabel = "while" ++ labelNumber ++ "exit"
    asmPut $ enterLabel ++ ":"
    let jumpInstr = getJumpInstr expr
    expression expr
    asmPut $ jumpInstr ++ exitLabel
    mapM_ statement whileBody
    asmPut $ "jmp " ++ enterLabel
    asmPut $ exitLabel ++ ":"
    

statement (ConditionalIf ifExpr ifBody) = do
    labelNumber <- liftM show newLabelNumber
    let exitLabel = "ifExit" ++ labelNumber
    let jumpInstr = getJumpInstr ifExpr
    expression ifExpr
    asmPut $ jumpInstr ++ exitLabel
    mapM_ statement ifBody
    asmPut $ exitLabel ++ ":"

statement (ConditionalIfElse ifExpr ifBody elseBody) = do
    labelNumber <- liftM show newLabelNumber
    let ifLabel = "if" ++ labelNumber
    let elseLabel = "else" ++ labelNumber
    let exitLabel = "ifElseExit" ++ labelNumber
    let jumpInstr = getJumpInstr ifExpr
    expression ifExpr
    asmPut $ jumpInstr ++ elseLabel
    mapM_ statement ifBody
    asmPut $ "jmp " ++ exitLabel

    asmPut $ elseLabel ++ ":"
    mapM_ statement elseBody
    asmPut $ exitLabel ++ ":"

-- Predefined functions
putPredefinedFunctions :: State Asm ()
putPredefinedFunctions =
    mapM_ putFunction predefinedFunctions
    where
    putFunction functionName = 
        case functionName of
            "print_int" -> do
                asmPut ".printf_int:"
                asmPut ".string \"%d\\n\"" 
                asmPut "print_int:"
                asmPut "pushl %ebp"
                asmPut "movl %esp, %ebp"
                asmPut "subl $24, %esp"
                asmPut "movl 8(%ebp), %eax"
                asmPut "movl %eax, 4(%esp)"
                asmPut "movl $.printf_int, (%esp)"
                asmPut "call printf"
                asmPut "leave"
                asmPut "ret"
                asmPut ""
                return ()
            _ -> return ()
    predefinedFunctions = ["print_int"]

-- Function
function :: Function -> State Asm ()
function (Function decl statements) = 
    let (FunctionDecl _ functionName params) = decl in do
    cleanVariables
    pushParameters params
    putFunctionName functionName
    functionEnter
    mapM_ statement statements
    asmPut ""
    where
    putFunctionName name =
        if name == "main" then 
            asmPut ".globl main" >> asmPut "main:"
        else
            asmPut $ name ++ ":"

    firstParameter (Parameter parType parName) = do
        case parType of
            "int" -> putNewVariable parName (-4-intSize)
            "char" -> putNewVariable parName (-4-charSize)

    pushParameters [] = return ()
    pushParameters (x:xs) = do
        firstParameter x
        pushParametersAux xs 
        putOffsetBorder

    pushParametersAux [] = return ()
    pushParametersAux ((Parameter parType parName):xs) = do
        case parType of
            "int" -> putNewVariable parName (-intSize)
            "char" -> putNewVariable parName (-charSize)
        pushParametersAux xs

cProg :: ParseTree -> State Asm ()
cProg parseTree = mapM_ function parseTree

runAssembly :: ParseTree -> String
runAssembly parseTree = 
    output $ execState (putPredefinedFunctions >> cProg parseTree) initialState
    where
    initialState = Asm [] [] 0
