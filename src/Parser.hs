module Parser (tokensToEquationTokens) where

import Syntax
  ( EquationToken (..),
    EquationTokenType (..),
    Token (..),
    TokenType (..),
    getEquationTokenType,
    getTokenType,
    getTokenValue,
    isOperand,
    isUnaryOperator,
  )

tokensToEquationTokens :: [Token] -> [EquationToken]
tokensToEquationTokens tokens = tokensToEquationTokensRecursive tokens []

tokensToEquationTokensRecursive ::
  [Token] -> [EquationToken] -> [EquationToken]
tokensToEquationTokensRecursive [] equationTokens = equationTokens
tokensToEquationTokensRecursive (t : ts) equationTokens =
  if currentTokenType == WhitespaceToken
    then tokensToEquationTokensRecursive ts equationTokens
    else tokensToEquationTokensRecursive ts $ equationTokens ++ [currentEquationToken]
  where
    currentTokenType = getTokenType t
    currentTokenValue = getTokenValue t

    currentEquationToken =
      if currentEquationTokenType == NumericalOperand
        then
          EquationToken
            currentTokenValue
            currentEquationTokenType
            (read currentTokenValue :: Double)
        else EquationToken currentTokenValue currentEquationTokenType 0.0

    previousEquationToken =
      if null equationTokens
        then EquationToken "" OpenParenthesis 0.0 -- just a filler value
        else last equationTokens

    currentEquationTokenType = case currentTokenType of
      BadToken -> error $ "Bad token: " ++ show t
      IdentifierToken -> identifierToEquationToken currentTokenValue
      OpenParenthesisToken -> OpenParenthesis
      CloseParenthesisToken -> CloseParenthesis
      PlusToken -> AdditionOperator
      MinusToken ->
        if isOperand $ getEquationTokenType previousEquationToken
          then SubtractionOperator
          else NegationOperator
      AsteriskToken -> MultiplicationOperator
      ForwardSlashToken -> DivisionOperator
      CaretToken -> ExponentiationOperator
      EqualsToken -> EqualsOperator
      NumberToken -> NumericalOperand
      _ -> error "There should be no unmatched tokens here"

infixToPostfix :: [EquationToken] -> [EquationToken]
infixToPostfix _ = [EquationToken "" Bad 0.0] -- filler

identifierToEquationToken :: String -> EquationTokenType
identifierToEquationToken str = case str of
  "sin" -> SineOperator
  "cos" -> CosineOperator
  "tan" -> TangentOperator
  "abs" -> AbsoluteOperator
  "floor" -> FloorOperator
  "ceil" -> CeilingOperator
  "sqrt" -> SquareRootOperator
  "cbrt" -> CubeRootOperator
  _ -> VariableOperand

{-
Implied multiply between
v: variable operand
u: unary operator (excluding negation)
n: number operand

)v = )*v
)u = )*u
)( = )*(
)n = )*n

nv = n*v
nu = n*u
n( = n*(

v( = v*(
-}
hasImpliedMultiply :: EquationTokenType -> EquationTokenType -> Bool
hasImpliedMultiply previousTokenType currentTokenType
  | -- previous type must be ) n v
    previousTokenType
      `notElem` [CloseParenthesis, NumericalOperand, VariableOperand] =
    False
  | -- current type must be v ( n u
    currentTokenType
      `notElem` [VariableOperand, OpenParenthesis, NumericalOperand]
      && not (isUnaryOperator currentTokenType) =
    False
  | -- current type cannot be negation
    currentTokenType == NegationOperator =
    False
  | otherwise = case previousTokenType of
    CloseParenthesis -> True -- ) * n v u (
    NumericalOperand -> currentTokenType /= NumericalOperand -- n * v u (
    VariableOperand -> currentTokenType == OpenParenthesis -- v * (
    _ -> False

canParseNumber :: Token -> Bool
canParseNumber token =
  2 > length (filter (== '.') tokenValue)
    && all (`elem` '.' : ['0' .. '9']) tokenValue
  where
    tokenValue = getTokenValue token
