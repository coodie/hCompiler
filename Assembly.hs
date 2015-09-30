import Control.Monad.State

charSize = 1 :: Int
intSize = 4 :: Int

data Asm = Asm 
    { variable :: [(String, Int)] -- (Stack offset, Name)
    ,  output :: String 
    }

appendToOutput str (Asm vars output) = 
    Asm vars (output ++ str)

getVariable (Asm vars output) varName =
    let Just x = lookup varName vars in x

functionEnter :: State Asm ()
functionEnter = do 
    modify $ appendToOutput $ "pushl %esp\n" ++ "movl %esp, %ebp\n"
    return ()
    
functionLeave :: State Asm ()
functionLeave = do 
    modify $ appendToOutput $ "leave\n" ++ "ret\n"
    return ()

main = return ()
