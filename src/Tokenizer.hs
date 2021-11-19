module Tokenizer
( getTokens
, tokensToString
, getTokenValue
, getTokenType
) where

import qualified Data.Char (isAlpha, isDigit)
import qualified Syntax (Token, TokenType)

tokensToString :: [Syntax.Token] -> String
tokensToString tokens = 
    concat $ map (++"\n") $ map show $ zip tValues tTypes
  where
    tValues = map getTokenValue tokens
    tTypes = map getTokenType tokens

getTokens :: String -> [Syntax.Token]
getTokens s = getTokensRecursive s []

getTokensRecursive :: String -> [Syntax.Token] -> [Syntax.Token]
getTokensRecursive "" tokens = tokens
getTokensRecursive (x:xs) tokens = 
    {-
    if the previous and current types match, and the types are mergeable
    we reconstruct the previous token by adding the current value to it 
    -}
    if getTokenType previousToken == currentTokenType && isMergeableTokenType currentTokenType
    then getTokensRecursive xs $ (init tokens) ++ [Token (getTokenValue previousToken++[x]) currentTokenType]
    else getTokensRecursive xs $ tokens ++ [Token [x] currentTokenType]
  where
    currentTokenType = charToToken x
    previousToken =
        if null tokens 
        then Token "" Bad
        else last tokens

charToToken :: Char -> Syntax.TokenType
charToToken c = 
    if Data.Char.isAlpha c
    then Identifier
    else if Data.Char.isDigit c || c == '.'
    then Number
    else
        case c of
            '(' -> OpenParenthesis
            ')' -> CloseParenthesis
            '+' -> Plus
            '-' -> Minus
            '*' -> Asterisk
            '/' -> ForwardSlash
            '^' -> Caret
            '=' -> Equals
            ' '  -> Whitespace
            _ -> Bad 

isMergeableTokenType :: Syntax.TokenType -> Bool
isMergeableTokenType t = t == Identifier || t == Number


