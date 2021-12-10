module Main where

import Data.Either (isLeft)
import qualified Parser (infixToPostfix, operatorPopCount, tokensToEquationTokens)
import Syntax (EquationToken (..), EquationTokenType (..), Token (..), equationTokensToString, tokensToString)
import qualified Tokenizer (getTokens)

tokens :: [Syntax.Token]
tokens = Tokenizer.getTokens "k=-9sin(x)^2+5.3b"

equationTokens :: Either String [Syntax.EquationToken]
equationTokens = Parser.tokensToEquationTokens tokens

a = EquationToken (Left "a") VariableOperand

plus = EquationToken (Left "+") AdditionOperator

times = EquationToken (Left "*") MultiplicationOperator

sub = EquationToken (Left "-") SubtractionOperator

b = EquationToken (Left "b") VariableOperand

s1 :: String
s1 = Syntax.equationTokensToString $ Parser.infixToPostfix [a, plus, b]

s2 = show $ Parser.operatorPopCount [plus, times, sub] ExponentiationOperator

main :: IO ()
main =
  case equationTokens of
    Left str -> putStrLn str
    Right eqTokens -> putStrLn $ equationTokensToString $ Parser.infixToPostfix eqTokens