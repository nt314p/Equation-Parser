module Main where

import qualified Parser (tokensToEquationTokens, infixToPostfix)
import Syntax (EquationToken (..), Token (..), EquationTokenType (..), equationTokensToString, tokensToString)
import qualified Tokenizer (getTokens)
import Data.Either (isLeft)

tokens :: [Syntax.Token]
tokens = Tokenizer.getTokens "k=-9sin(x)^2+5.3b"

equationTokens :: Either String [Syntax.EquationToken]
equationTokens = Parser.tokensToEquationTokens tokens

a = EquationToken "a" VariableOperand 0.0
plus = EquationToken "+" AdditionOperator 0.0
b = EquationToken "b" VariableOperand 0.0


main :: IO ()
main = 
  case equationTokens of
    Left str -> putStrLn str
    Right eqTokens -> putStrLn $ Syntax.equationTokensToString $ Parser.infixToPostfix [a, plus, b]
