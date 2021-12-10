module Syntax
  ( Token (..),
    TokenType (..),
    tokensToString,
    EquationToken (..),
    EquationTokenType (..),
    equationTokensToString,
    isOperand,
    isOperator,
    isUnaryOperator,
    isBinaryOperator,
    isParenthesis,
    operatorPriority,
  )
where

data Token = Token
  { tokenValue :: String,
    tokenType :: TokenType
  }
  deriving (Show)

data TokenType
  = TokenType
  | BadToken
  | WhitespaceToken
  | PlusToken
  | MinusToken
  | AsteriskToken
  | ForwardSlashToken
  | CaretToken
  | OpenParenthesisToken
  | CloseParenthesisToken
  | EqualsToken
  | IdentifierToken
  | NumberToken
  deriving (Show, Eq)

tokensToString :: [Token] -> String
tokensToString = concatMap (\(Token tVal tType) -> show (tVal, tType) ++ "\n")

data EquationToken = EquationToken
  { equationTokenValue :: Either String Double,
    equationTokenType :: EquationTokenType
  }
  deriving (Show)

getEquationTokenNumericalValue :: EquationToken -> Maybe Double -- maybe
getEquationTokenNumericalValue equationToken = Nothing

data EquationTokenType
  = EquationTokenType
  | Bad
  | VariableOperand
  | NumericalOperand
  | AdditionOperator
  | SubtractionOperator
  | MultiplicationOperator
  | DivisionOperator
  | ExponentiationOperator
  | NegationOperator
  | SineOperator
  | CosineOperator
  | TangentOperator
  | AbsoluteOperator
  | FloorOperator
  | CeilingOperator
  | SquareRootOperator
  | CubeRootOperator
  | EqualsOperator
  | OpenParenthesis
  | CloseParenthesis
  deriving (Show, Eq)

equationTokensToString :: [EquationToken] -> String
equationTokensToString = concatMap (\(EquationToken tVal tType) -> show (toStringValue tVal, tType) ++ "\n")

toStringValue :: Either String Double -> String
toStringValue val = case val of 
  Left str -> str 
  Right num -> show num

isOperand :: EquationTokenType -> Bool
isOperand t = t == NumericalOperand || t == VariableOperand

isUnaryOperator :: EquationTokenType -> Bool
isUnaryOperator t =
  t == NegationOperator
    || t == SineOperator
    || t == CosineOperator
    || t == TangentOperator
    || t == AbsoluteOperator
    || t == FloorOperator
    || t == CeilingOperator
    || t == SquareRootOperator
    || t == CubeRootOperator

isBinaryOperator :: EquationTokenType -> Bool
isBinaryOperator t =
  t == AdditionOperator
    || t == SubtractionOperator
    || t == MultiplicationOperator
    || t == DivisionOperator
    || t == ExponentiationOperator
    || t == EqualsOperator

isOperator :: EquationTokenType -> Bool
isOperator t = isUnaryOperator t || isBinaryOperator t

isParenthesis :: EquationTokenType -> Bool
isParenthesis t = t == OpenParenthesis || t == CloseParenthesis

operatorPriority :: EquationTokenType -> Int
operatorPriority t
  | t `elem` [AdditionOperator, SubtractionOperator] = 1
  | t `elem` [MultiplicationOperator, DivisionOperator] = 2
  | t == ExponentiationOperator = 3
  | t == EqualsOperator = 0
  | isUnaryOperator t = 4
  | otherwise = -1
