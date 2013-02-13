module Brainfuck.Generator.JVM (tokensToJVMCode, generateJVMTemplate) where
import Control.Monad.State
import Brainfuck.Tokens

type LabelStack = [Int] 

pop :: State LabelStack String 
pop = state $ \(x:xs) -> (show x, xs)

push :: State LabelStack String 
push = state $ inc
    where 
        inc (x:xs) = (show (x + 1), (x + 1):x:xs)
        inc []     = (show 0, [0])

evalStack :: State LabelStack [String] -> [String]
evalStack stack = (evalState stack) []

tokensToJVMCode :: [BFToken] -> String
tokensToJVMCode = unlines . evalStack . (mapM tokenToCode) 

tokenToCode :: BFToken -> State LabelStack String
tokenToCode (BFMove c) = return $ unlines
    [ "    ; " ++ tokenComment
    , "    iinc 1 " ++ (show c)
    ]
    where
        tokenComment = replicate (abs c) (if c > 0 then '>' else '<')

tokenToCode (BFInc c) = return $ unlines
    [ "    ; " ++ tokenComment
    , "    aload_2"
    , "    iload_1"
    , "    dup2"
    , "    iaload"
    , "    bipush " ++ (show c)
    , "    iadd"
    , "    iastore "
    ]
    where
        tokenComment = replicate (abs c) (if c > 0 then '+' else '-')

tokenToCode BFOut = return $ unlines
    [ "    ; ."
    , "    getstatic java/lang/System/out Ljava/io/PrintStream;"
    , "    aload_2"
    , "    iload_1"
    , "    iaload"
    , "    i2c"
    , "    invokevirtual java/io/PrintStream/print(C)V"
    ]
tokenToCode BFIn = return $ unlines
    [ "    ; ,"
    , "    aload_2"
    , "    iload_1"
    , "    getstatic java/lang/System/in Ljava/io/InputStream;"
    , "    invokevirtual java/io/InputStream/read()I"
    , "    iastore "
    ]
tokenToCode BFLoopStart = do
    current <- push
    return $ unlines
        [ "    ; ["
        , "loop" ++ current ++ "Start:"
        , "    aload_2"
        , "    iload_1"
        , "    iaload"
        , "    ifeq loop" ++ current ++ "End"
        ]
tokenToCode BFLoopEnd = do
    current <- pop
    return $ unlines
        [ "    ; ]"
        , "    goto loop" ++ current ++ "Start"
        , "loop" ++ current ++ "End:"
        ]

generateJVMTemplate :: String -> String -> String
generateJVMTemplate name body = unlines 
    [ ".class public " ++ name
    , ".super java/lang/Object"
    , ""
    , ".method public <init>()V"
    , "    aload_0"
    , "    invokenonvirtual java/lang/Object/<init>()V"
    , "    return"
    , ".end method" 
    , "" 
    , ".method public static main([Ljava/lang/String;)V"
    , "    .limit stack 10"
    , "    .limit locals 3"
    , ""
    , "    ; Pointer"
    , "    iconst_0"
    , "    istore_1"
    , "    "
    , "    ; Array"
    , "    bipush 100"
    , "    newarray int "
    , "    astore_2"
    , "" 
    , body
    , "    return "
    , ".end method"
    ]
