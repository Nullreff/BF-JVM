import Control.Monad.State
import System.Environment
import System.FilePath
import System.IO
import System.Cmd
import Data.List
import Data.Maybe
import Brainfuck.Tokens
import Brainfuck.Optimizer
import Brainfuck.Parser
import Brainfuck.Generator.JVM

main = do
    args <- getArgs 
    let inputName  = head args
    inputData      <- readFile inputName
    let name       = dropExtension inputName
        outputName = name ++ ".j"
        outputData = generateJVMTemplate name $ compileBF inputData
    writeFile outputName outputData
    rawSystem "jasmin" [ outputName ]

compileBF :: String -> String
compileBF input = 
    let rawTokens = catMaybes $ map parseBFToken input
        optTokens = mergeTokens rawTokens
        code      = tokensToJVMCode optTokens
    in code
