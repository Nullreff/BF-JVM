import Control.Monad.State
import System.Environment
import System.FilePath
import System.IO
import System.Cmd
import Data.List
import Data.Maybe

data BFToken = BFMove Int
             | BFInc Int
             | BFOut 
             | BFIn
             | BFLoopStart
             | BFLoopEnd
             deriving (Show)

data JVMType = JVM_int

data JVMToken = JVM_iconst Int 
              | JVM_istore Int
              | JVM_bipush Int
              | JVM_newarray JVMType

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

main = do
    args <- getArgs 
    let inputName  = head args
    inputData      <- readFile inputName
    let name       = dropExtension inputName
        outputName = name ++ ".j"
        outputData = generateTemplate name $ parseBF inputData
    writeFile outputName outputData
    rawSystem "jasmin" [ outputName ]

parseBF :: String -> String
parseBF input = 
    let rawTokens = catMaybes $ map getBFToken input
        optTokens = optimizeTokens rawTokens
        code      = evalStack $ mapM tokenToCode optTokens
    in unlines code 

optimizeTokens :: [BFToken] -> [BFToken]
optimizeTokens []                         = []
optimizeTokens ((BFMove x):(BFMove y):xs) = optimizeTokens $ BFMove (x + y) : xs
optimizeTokens ((BFInc x):(BFInc y):xs)   = optimizeTokens $ BFInc (x + y) : xs
optimizeTokens (x:xs)                     = x : (optimizeTokens xs)

tokenToCode :: BFToken -> State LabelStack String
tokenToCode (BFMove c) = return $ unlines
    [ "    iinc 1 " ++ (show c)
    ]
tokenToCode (BFInc c) = return $ unlines
    [ "    aload_2"
    , "    iload_1"
    , "    dup2"
    , "    iaload"
    , "    bipush " ++ (show c)
    , "    iadd"
    , "    iastore "
    ]
tokenToCode BFOut = return $ unlines
    [ "    getstatic java/lang/System/out Ljava/io/PrintStream;"
    , "    aload_2"
    , "    iload_1"
    , "    iaload"
    , "    i2c"
    , "    invokevirtual java/io/PrintStream/print(C)V"
    ]
tokenToCode BFIn = return $ unlines
    [ "    aload_2"
    , "    iload_1"
    , "    getstatic java/lang/System/in Ljava/io/InputStream;"
    , "    invokevirtual java/io/InputStream/read()I"
    , "    iastore "
    ]
tokenToCode BFLoopStart = do
    current <- push
    return $ unlines
        [ "loop" ++ current ++ "Start:"
        , "    aload_2"
        , "    iload_1"
        , "    iaload"
        , "    ifeq loop" ++ current ++ "End"
        ]
tokenToCode BFLoopEnd = do
    current <- pop
    return $ unlines
        [ "    goto loop" ++ current ++ "Start"
        , "loop" ++ current ++ "End:"
        ]

getBFToken :: Char -> Maybe BFToken
getBFToken '<' = Just $ BFMove (-1)
getBFToken '>' = Just $ BFMove 1
getBFToken '+' = Just $ BFInc 1
getBFToken '-' = Just $ BFInc (-1)
getBFToken '.' = Just BFOut
getBFToken ',' = Just BFIn
getBFToken '[' = Just BFLoopStart
getBFToken ']' = Just BFLoopEnd
getBFToken _   = Nothing 

generateTemplate :: String -> String -> String
generateTemplate name body = unlines 
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
