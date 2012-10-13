module Brainfuck.Tokens (BFToken (BFMove, BFInc, BFOut, BFIn, BFLoopStart, BFLoopEnd)) where

data BFToken = BFMove Int
             | BFInc Int
             | BFOut 
             | BFIn
             | BFLoopStart
             | BFLoopEnd
             deriving (Show)
