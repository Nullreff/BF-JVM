module Brainfuck.Parser (parseBFToken) where
import Brainfuck.Tokens

parseBFToken :: Char -> Maybe BFToken
parseBFToken '<' = Just $ BFMove (-1)
parseBFToken '>' = Just $ BFMove 1
parseBFToken '+' = Just $ BFInc 1
parseBFToken '-' = Just $ BFInc (-1)
parseBFToken '.' = Just BFOut
parseBFToken ',' = Just BFIn
parseBFToken '[' = Just BFLoopStart
parseBFToken ']' = Just BFLoopEnd
parseBFToken _   = Nothing 

