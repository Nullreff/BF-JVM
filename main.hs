import Control.Monad.State
import System.Environment
import System.IO
import Data.List
import Data.Maybe

data BFToken = BFLeft
             | BFRight
             | BFAdd
             | BFSub
             | BFOut
             | BFIn
             | BFLoopStart
             | BFLoopEnd
             deriving (Show)

type LabelStack = [Int] 

pop :: State LabelStack String 
pop = State $ \(x:xs) -> (show x, xs)

push :: State LabelStack String 
push = State $ inc
    where 
        inc (x:xs) = (show (x + 1), (x + 1):x:xs)
        inc [] = (show 0, [0])

evalStack :: State LabelStack [String] -> [String]
evalStack stack = (evalState stack) []

main = do
    args <- getArgs 
    let inputName = head args
        outputName = "TestOut.j"
    inputData <- readFile inputName
    let outputData = generateTemplate "TestOut" $ parseBF inputData
    writeFile outputName outputData

parseBF :: String -> String
parseBF input = 
    let tokens = catMaybes $ map getBFToken input
        code   = evalStack $ mapM tokenToCode tokens 
    in unlines code 

tokenToCode :: BFToken -> State LabelStack String
tokenToCode BFLeft      = return "iinc 1 -1\n"
tokenToCode BFRight     = return "iinc 1 1\n"
tokenToCode BFAdd       = return $ unlines
    [ "aload_2"
    , "iload_1"
    , "dup2"
    , "iaload"
    , "bipush 1"
    , "iadd"
    , "iastore "
    ]
tokenToCode BFSub       = return $ unlines
    [ "aload_2"
    , "iload_1"
    , "dup2"
    , "iaload"
    , "bipush -1"
    , "iadd"
    , "iastore "
    ]
tokenToCode BFOut       = return $ unlines
    [ "getstatic java/lang/System/out Ljava/io/PrintStream;"
    , "aload_2"
    , "iload_1"
    , "iaload"
    , "i2c"
    , "invokevirtual java/io/PrintStream/print(C)V"
    ]
tokenToCode BFIn        = return $ unlines
    [ "aload_2"
    , "iload_1"
    , "getstatic java/lang/System/in Ljava/io/InputStream;"
    , "invokevirtual java/io/InputStream/read()I"
    , "iastore "
    ]
tokenToCode BFLoopStart = do
    current <- push
    return $ unlines
        [ "loop" ++ current ++ "Start:"
        , "aload_2"
        , "iload_1"
        , "iaload"
        , "ifeq loop" ++ current ++ "End"
        ]
tokenToCode BFLoopEnd   = do
    current <- pop
    return $ unlines
        [ "goto loop" ++ current ++ "Start"
        , "loop" ++ current ++ "End:"
        ]

getBFToken :: Char -> Maybe BFToken
getBFToken '<' = Just BFLeft
getBFToken '>' = Just BFRight
getBFToken '+' = Just BFAdd
getBFToken '-' = Just BFSub
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
    , ""
    , "    return "
    , ".end method"
    ]
