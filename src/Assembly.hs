module Assembly where
import Control.Monad.State
import Data.List
import Ast

charSize = 1 :: Int
intSize = 4 :: Int

data Asm = Asm 
    { variable :: [(String, Int)] -- (Stack offset, Name)
    ,  output :: [String] 
    ,  label :: Int
    } deriving Show

appendToOutput str asm =
    asm {output = str:(output asm)}

asmPut :: String -> State Asm ()
asmPut str = modify $ appendToOutput (str ++ "\n")

-- asmComment :: String -> State Asm ()
-- asmComment str = modify $ ()

getVarStackOffset :: String -> State Asm Int
getVarStackOffset varName = do
    vars <- gets variable 
    let Just x = lookup varName vars 
    return x

calculateFunctionStackFrameSize :: [Statement] -> Int
calculateFunctionStackFrameSize [] = 0
calculateFunctionStackFrameSize (x:xs) =
    case x of
        (ValDec _ _) -> 4 + (calculateFunctionStackFrameSize xs)
        (ValDef _ _ _) -> 4 + (calculateFunctionStackFrameSize xs)
        _ -> (calculateFunctionStackFrameSize xs)
    

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


functionEnter :: Int -> State Asm ()
functionEnter 0 = do 
    asmPut "pushl %ebp"  
    asmPut "movl %esp, %ebp"

functionEnter space = do 
    asmPut "pushl %ebp"  
    asmPut "movl %esp, %ebp"
    asmPut $ "sub $" ++ (show space) ++ ", %esp"
    
functionLeave :: State Asm ()
functionLeave = do 
    asmPut "leave"
    asmPut "ret"

-- arithExpr
arithExpr :: ArithExpr -> State Asm ()
arithExpr (Add e1 e2) = do
    arithExpr e1
    asmPut "pushl %eax"
    arithExpr e2
    asmPut "popl %ebx"
    asmPut "add %ebx, %eax"

arithExpr (Sub e1 e2) = do
    arithExpr e1
    asmPut "pushl %eax"
    arithExpr e2
    asmPut "popl %ebx"
    asmPut "sub %eax, %ebx"
    asmPut "movl %ebx, %eax"

arithExpr (Mul e1 e2) = do
    arithExpr e1
    asmPut "pushl %eax"
    arithExpr e2
    asmPut "popl %ebx"
    asmPut "imull %ebx, %eax"

arithExpr (IntConst e) = do
    let val = show e
    asmPut $ "movl $" ++ val ++ ", %eax"

arithExpr (VarName e) = do
    offset <- liftM show $ getVarStackOffset e
    asmPut $ "movl " ++ offset ++ "(%ebp), %eax"

arithExpr (ArithFunCall e1 e2) = do
    evalArgs e2
    asmPut $ "call " ++ e1
    asmPut $ "add $" ++ (show $ 4*(length e2)) ++ ", %esp"
    where
    evalArgs [] = return ()
    evalArgs (x:xs) = do
        evalArgs xs
        arithExpr x
        asmPut "pushl %eax"

boolExpr :: BoolExpr -> State Asm String
boolExpr expr = do
    let (e1, e2) = getBoth expr
    arithExpr e1
    asmPut "movl %eax, %ebx"
    arithExpr e2
    asmPut "cmp %ebx, %eax"
    case expr of
        (Equal _ _) -> return "je"
        (NotEqual _ _) -> return "jne"
        (Greater _ _) -> return "jg"
        (GreaterEqual _ _) -> return "jge"
        (Less _ _) -> return "jl"
        (LessEqual _ _) -> return "jle"
    where
    getBoth (Equal e1 e2) = (e1, e2) 
    getBoth (NotEqual e1 e2) = (e1, e2) 
    getBoth (Greater e1 e2) = (e1, e2) 
    getBoth (GreaterEqual e1 e2) = (e1, e2) 
    getBoth (Less e1 e2) = (e1, e2) 
    getBoth (LessEqual e1 e2) = (e1, e2) 

reverseJumpInstr :: String -> String
reverseJumpInstr instr = 
    case instr of
        "je" -> "jne"
        "jne" -> "je"
        "jg" -> "jle"
        "jge" -> "jl"
        "jl" -> "jge"
        "jle" -> "jg"
        _ -> "not supported " ++ instr

statement :: Statement -> State Asm ()
statement (ValDec typeName varName) = 
    case typeName of
        "int" -> putNewVariable varName intSize
        "char" -> putNewVariable varName charSize

statement (Assignment varName expr) = do
    offset <- liftM show $ getVarStackOffset varName 
    arithExpr expr
    asmPut $ "movl %eax, " ++ offset ++ "(%ebp)" 

statement (ValDef typeName varName expr) = do
    case typeName of
        "int" -> putNewVariable varName intSize
        "char" -> putNewVariable varName charSize
    offset <- liftM show $ getVarStackOffset varName 
    arithExpr expr
    asmPut $ "movl %eax, " ++ offset ++ "(%ebp)" 

statement (StatFunCall a b) = arithExpr (ArithFunCall a b)

statement (FunctionReturn expr) = do
    arithExpr expr
    functionLeave

statement (WhileLoop expr whileBody) = do
    labelNumber <- liftM show newLabelNumber
    let enterLabel = "while" ++ labelNumber ++ "enter"
    let exitLabel = "while" ++ labelNumber ++ "exit"

    asmPut $ enterLabel ++ ":"
    jumpInstr <- boolExpr expr >>= return . reverseJumpInstr --if(expr == true) goto enterLabel
    asmPut $ jumpInstr ++ " " ++ exitLabel
    mapM_ statement whileBody
    asmPut $ "jmp " ++ enterLabel
    asmPut $ exitLabel ++ ":"
    
statement (ConditionalIfElse ifExpr ifBody elseBody) = do
    labelNumber <- liftM show newLabelNumber
    let ifLabel = "if" ++ labelNumber
    let elseLabel = "else" ++ labelNumber
    let exitLabel = "ifElseExit" ++ labelNumber

    jumpInstr <- boolExpr ifExpr
    asmPut $ jumpInstr ++ " " ++ ifLabel
    mapM_ statement elseBody
    asmPut $ "jmp " ++ exitLabel

    asmPut $ ifLabel ++ ":"
    mapM_ statement ifBody
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
    let space = calculateFunctionStackFrameSize statements
    functionEnter space
    mapM_ statement statements
    functionLeave
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
    concat . reverse . output $ execState (putPredefinedFunctions >> cProg parseTree) initialState
    where
    initialState = Asm [] [] 0
