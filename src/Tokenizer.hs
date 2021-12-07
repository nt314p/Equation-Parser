module Tokenizer (getTokens) where

import qualified Data.Char (isAlpha, isDigit)
import Syntax (Token (..), TokenType (..))

getTokens :: String -> [Token]
getTokens s = getTokens' s []

getTokens' :: String -> [Syntax.Token] -> [Syntax.Token]
getTokens' "" tokens = tokens
getTokens' (x : xs) tokens =
  {-
  if the previous and current types match, and the types are mergeable
  we reconstruct the previous token by adding the current value to it
  -}
  if tokenType previousToken == currentTokenType && isMergeableTokenType currentTokenType
    then getTokens' xs $ init tokens ++ [Token (tokenValue previousToken ++ [x]) currentTokenType]
    else getTokens' xs $ tokens ++ [Token [x] currentTokenType]
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
