module Brainfuck.Optimizer (mergeTokens) where
import Brainfuck.Tokens

mergeTokens :: [BFToken] -> [BFToken]
mergeTokens []                         = []
mergeTokens ((BFMove x):(BFMove y):xs) = mergeTokens $ BFMove (x + y) : xs
mergeTokens ((BFInc x):(BFInc y):xs)   = mergeTokens $ BFInc (x + y) : xs
mergeTokens (x:xs)                     = x : (mergeTokens xs)


