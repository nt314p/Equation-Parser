module Parser (tokensToEquationTokens, infixToPostfix) where

import Data.Either (fromRight, isLeft, isRight)
import Syntax
  ( EquationToken (..),
    EquationTokenType (..),
    Token (..),
    TokenType (..),
    isBinaryOperator,
    isOperand,
    isOperator,
    isParenthesis,
    isUnaryOperator,
    operatorPriority,
  )

tokensToEquationTokens :: [Token] -> Either String [EquationToken]
tokensToEquationTokens tokens
  | isLeft equationTokens = equationTokens
  | parenthesisCount < 0 = Left $ "Unmatched close parenthesis: " ++ show parenthesisCount
  | parenthesisCount > 0 = Left $ "Unmatched open parenthesis: " ++ show parenthesisCount
  | isOperator $ equationTokenType $ last rightEquationTokens = Left "Expression cannot end with operator"
  | otherwise =
    case length $ filter (\(EquationToken _ t _) -> t == EqualsOperator) rightEquationTokens of
      0 -> Left "Expression must contain equals operator"
      1 ->
        if equationTokenType (head rightEquationTokens) /= VariableOperand
          && equationTokenType (rightEquationTokens !! 1) /= EqualsOperator
          then Left "Left hand side of assignment must be variable operand"
          else Right rightEquationTokens
      _ -> Left "Too many equals operators"
  where
    equationTokens = tokensToEquationTokens' tokens []
    parenthesisCount = countParenthesis rightEquationTokens
    rightEquationTokens = fromRight (error "Equation tokens is not right") equationTokens

tokensToEquationTokens' :: [Token] -> [EquationToken] -> Either String [EquationToken]
tokensToEquationTokens' [] equationTokens = Right equationTokens
tokensToEquationTokens' (t : ts) equationTokens
  | currentEquationTokenType == Bad = Left $ "Bad token '" ++ show currentEquationToken ++ "'"
  | previousEquationTokenType == OpenParenthesis && currentEquationTokenType == CloseParenthesis =
    Left "Cannot have empty parenthesis"
  | isOperator previousEquationTokenType && currentEquationTokenType == CloseParenthesis =
    Left $ "Operator '" ++ show previousEquationToken ++ "' is missing an operand"
  | isOperator previousEquationTokenType && isBinaryOperator currentEquationTokenType =
    Left $ "Expected operand but got operator '" ++ show currentEquationToken ++ "'"
  | otherwise = tokensToEquationTokens' ts $ equationTokens ++ addedEquationTokens
  where
    addedEquationTokens =
      if currentTokenType == WhitespaceToken
        then []
        else impliedMultiplyToken ++ [currentEquationToken]

    impliedMultiplyToken =
      [ EquationToken "*" MultiplicationOperator 0.0
        | hasImpliedMultiply previousEquationTokenType currentEquationTokenType
      ]

    currentTokenType = tokenType t
    currentTokenValue = tokenValue t

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
        then EquationToken "" Bad 0.0 -- just a filler value
        else last equationTokens

    currentEquationTokenType = case currentTokenType of
      BadToken -> Bad
      IdentifierToken -> identifierToEquationToken currentTokenValue
      OpenParenthesisToken -> OpenParenthesis
      CloseParenthesisToken -> CloseParenthesis
      PlusToken -> AdditionOperator
      MinusToken ->
        if isOperand $ equationTokenType previousEquationToken
          then SubtractionOperator
          else NegationOperator
      AsteriskToken -> MultiplicationOperator
      ForwardSlashToken -> DivisionOperator
      CaretToken -> ExponentiationOperator
      EqualsToken -> EqualsOperator
      NumberToken -> NumericalOperand
      _ -> error $ "There should be no unmatched tokens here, but got: " ++ show t

    previousEquationTokenType = equationTokenType previousEquationToken

infixToPostfix :: [EquationToken] -> [EquationToken]
infixToPostfix = infixToPostfix' [] []

infixToPostfix' :: [EquationToken] -> [EquationToken] -> [EquationToken] -> [EquationToken]
infixToPostfix' postfixTokens opStack [] =
  postfixTokens ++ filter (not . isParenthesis . equationTokenType) opStack
infixToPostfix' postfixTokens opStack (t : ts)
  | isOperand currentType = infixToPostfix' (postfixTokens ++ [t]) opStack ts
  | isOperator currentType = infixToPostfix' (postfixTokens ++ take opPopCount opStack) (drop opPopCount opStack) ts
  | isParenthesis currentType =
    if currentType == OpenParenthesis
      then infixToPostfix' postfixTokens (t : opStack) ts
      else infixToPostfix' (postfixTokens ++ take openParenIndex opStack) (tail $ drop openParenIndex opStack) ts
  | otherwise = error "Should not get here... but why? Add"
  where
    currentType = equationTokenType t
    opPopCount = operatorPopCount opStack currentType
    openParenIndex = openParenthesisIndex opStack

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

-- computes how many ops to pop off the stack
operatorPopCount :: [EquationToken] -> EquationTokenType -> Int
operatorPopCount [] _ = 0
operatorPopCount equationTokens op =
  operatorPopCount' equationTokens (operatorPriority op) 0

operatorPopCount' :: [EquationToken] -> Int -> Int -> Int
operatorPopCount' [] _ n = n
operatorPopCount' (op : ops) priority n
  | not (isParenthesis peekType) && operatorPriority peekType > priority =
    operatorPopCount' ops priority n + 1
  | otherwise = n
  where
    peekType = equationTokenType op

openParenthesisIndex :: [EquationToken] -> Int
openParenthesisIndex equationTokens = openParenthesisIndex' equationTokens 0

openParenthesisIndex' :: [EquationToken] -> Int -> Int
openParenthesisIndex' [] _ = error "Open parenthesis was not found - but should have"
openParenthesisIndex' (t : ts) n
  | equationTokenType t == OpenParenthesis = n
  | otherwise = openParenthesisIndex' ts n + 1

countParenthesis :: [EquationToken] -> Int
countParenthesis equationTokens = countParenthesis' equationTokens 0

countParenthesis' :: [EquationToken] -> Int -> Int
countParenthesis' [] n = n
countParenthesis' (t:ts) n = countParenthesis' ts n+dn
  where
    dn = case equationTokenType t of
      OpenParenthesis -> 1
      CloseParenthesis -> -1
      _ -> 0

canParseNumber :: Token -> Bool
canParseNumber token =
  2 > length (filter (== '.') value)
    && all (`elem` '.' : ['0' .. '9']) value
  where
    value = tokenValue token
