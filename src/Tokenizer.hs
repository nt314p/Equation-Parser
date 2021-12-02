module Tokenizer
  ( getTokens,
    tokensToString,
  )
where

import qualified Data.Char (isAlpha, isDigit)
import Syntax (Token (..), TokenType (..), getTokenType, getTokenValue)

tokensToString :: [Token] -> String
tokensToString = concatMap (\(Token tVal tType) -> show (tVal, tType) ++ "\n")

getTokens :: String -> [Token]
getTokens s = getTokensRecursive s []

getTokensRecursive :: String -> [Syntax.Token] -> [Syntax.Token]
getTokensRecursive "" tokens = tokens
getTokensRecursive (x : xs) tokens =
  {-
  if the previous and current types match, and the types are mergeable
  we reconstruct the previous token by adding the current value to it
  -}
  if getTokenType previousToken == currentTokenType && isMergeableTokenType currentTokenType
    then getTokensRecursive xs $ init tokens ++ [Token (getTokenValue previousToken ++ [x]) currentTokenType]
    else getTokensRecursive xs $ tokens ++ [Token [x] currentTokenType]
  where
    currentTokenType = charToToken x
    previousToken =
      if null tokens
        then Token "" BadToken
        else last tokens

charToToken :: Char -> Syntax.TokenType
charToToken c
  | Data.Char.isAlpha c = IdentifierToken
  | Data.Char.isDigit c || c == '.' = NumberToken
  | otherwise =
    case c of
      '(' -> OpenParenthesisToken
      ')' -> CloseParenthesisToken
      '+' -> PlusToken
      '-' -> MinusToken
      '*' -> AsteriskToken
      '/' -> ForwardSlashToken
      '^' -> CaretToken
      '=' -> EqualsToken
      ' ' -> WhitespaceToken
      _ -> BadToken

isMergeableTokenType :: Syntax.TokenType -> Bool
isMergeableTokenType t = t == IdentifierToken || t == NumberToken
