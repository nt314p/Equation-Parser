module Syntax
  ( Token (..),
    TokenType (..),
    getTokenType,
    getTokenValue,
    EquationToken (..),
    EquationTokenType (..),
    getEquationTokenType,
    getEquationTokenValue,
    isOperand,
    isOperator,
    isUnaryOperator,
    isBinaryOperator,
  )
where

data Token = Token
  { value :: String,
    tokenType :: TokenType
  }
  deriving (Show)

getTokenValue :: Token -> String
getTokenValue = value

getTokenType :: Token -> TokenType
getTokenType = tokenType

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

data EquationToken = EquationToken
  { equationTokenValue :: String,
    equationTokenType :: EquationTokenType,
    numericalValue :: Double
  }
  deriving (Show)

getEquationTokenValue :: EquationToken -> String
getEquationTokenValue = equationTokenValue

getEquationTokenType :: EquationToken -> EquationTokenType
getEquationTokenType = equationTokenType

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

getOperatorPriority :: EquationTokenType -> Int
getOperatorPriority t
  | t `elem` [AdditionOperator, SubtractionOperator] = 1
  | t `elem` [MultiplicationOperator, DivisionOperator] = 2
  | t == ExponentiationOperator = 3
  | isUnaryOperator t = 4
  | otherwise = error "getOperatorPriority: token is not an operator"
