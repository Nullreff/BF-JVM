import Control.Monad.State
import System.Environment
import System.IO
import Data.List

data BFToken = BFLeft
             | BFRight
             | BFAdd
             | BFSub
             | BFOut
             | BFIn
             | BFLoopStart
             | BFLoopEnd
             deriving (Show)

type LabelStack = [String] 

pop :: State LabelStack String 
pop = State $ \(x:xs) -> (x, xs)

push :: String -> State LabelStack ()
push a = State $ \xs -> ((), a:xs)

main = do
    args <- getArgs 
    let inputName = head args
        outputName = "TestOut.j"
    inputData <- readFile inputName
    let outputData = generateTemplate "TestOut" $ parseBF inputData
    writeFile outputName outputData

parseBF :: String -> String
parseBF input = ""

tokenToCode :: State LabelStack ()

getBFToken :: Char -> BFToken
getBFToken '<' = BFLeft
getBFToken '>' = BFRight
getBFToken '+' = BFAdd
getBFToken '-' = BFSub
getBFToken '.' = BFOut
getBFToken ',' = BFIn
getBFToken '[' = BFLoopStart
getBFToken ']' = BFLoopEnd

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

{-- 

    ; >
    ; Increment the pointer
    iinc 1 1

    ; <
    ; Decrement the pointer
    iinc 1 -1

    ; +
    ; Increment the byte at the pointer
    aload_2
    iload_1
    dup2
    iaload
    bipush 1
    iadd
    iastore 

    ; -
    ; Decrement the byte at the pointer
    aload_2
    iload_1
    dup2
    iaload
    bipush -1
    iadd
    iastore 

    ; ,
    ; Input a byte and store it in the byte at the pointer
    aload_2
    iload_1
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokevirtual java/io/InputStream/read()I
    iastore 

    ; .
    ; Output the byte at the pointer
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload_2
    iload_1
    iaload
    i2c
    invokevirtual java/io/PrintStream/print(C)V

    ; [
    ; Start loop: Execute delimited code until the byte at the pointer equals zero
loopStart:
    aload_2
    iload_1
    iaload
    ; Jump 2 + instruction count
    ifeq loopEnd

    ; Loop Code goes here

    ; ]
    ; End loop: Jump back to the matching [
    ; Jump -1 - instruction count
    goto loopStart
loopEnd:

    return 
.end method
--}
