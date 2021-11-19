module Parser
(
tokensToEquationTokens
) where

import qualified Syntax (Token, EquationToken, isOperand, isUnaryOperator)
import qualified Tokenizer (getTokenType, getTokenValue)

tokensToEquationTokens :: [Token] -> [EquationToken]

tokensToEquationTokensRecursive :: [Token] -> [EquationToken] -> [EquationToken]
tokensToEquationTokensRecursive [] equationTokens = equationTokens
tokensToEquationTokensRecursive (t:ts) equationTokens = 

    tokensToEquationTokensRecursive ts $ equationTokens ++ [Equa]
  where
    currentTokenType = Tokenizer.getTokenType t
    currentTokenValue = Tokenizer.getTokenValue t
    currentEquationToken = 
        if currentEquationTokenType == NumericalOperand
        then EquationToken (readc currentTokenType) currentEquationTokenType
        else EquationToken 
    previousEquationToken = 
        if null equationTokens
        then EquationToken "" OpenParenthesis -- just a filler value
        else last equationTokens
    currentEquationTokenType = case currentTokenType of
        Bad -> error $ "Bad token: " ++ show t
        Identifier -> identifierToEquationToken currentTokenValue
        OpenParenthesis -> OpenParenthesis
        CloseParenthesis -> CloseParenthesis
        Plus -> AdditionOperator
        Minus -> 
            if isOperand getEquationTokenType previousEquationToken
            then SubtractionOperator
            else NegationOperator
        Asterisk -> MultiplicationOperator
        ForwardSlash -> DivisionOperator
        Caret -> ExponentiationOperator
        Equals -> EqualsOperator
        Number -> NumericalOperand
        

infixToPostfix :: [EquationToken] -> [EquationToken]

identifierToEquationToken :: String -> [EquationToken]
identifierToEquationToken str =
    case str of
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

hasImpliedMultiply :: EquationToken -> EquationToken -> Bool
hasImpliedMultiply previousTokenType currentTokenType
    -- previous type must be ) n v
    | not previousTokenType `elem` [CloseParenthesis, NumericalOperand, VariableOperand] -> False
    -- current type must be v ( n u
    | not currentTokenType `elem` [VariableOperand, OpenParenthesis, NumericalOperand]
        && not isUnaryOperator currentTokenType -> False
    -- current type cannot be negation
    | currentEquationToken == NegationOperator -> False
    | case previousTokenType of
        CloseParenthesis -> True -- ) * n v u (
        NumericalOperand -> currentTokenType /= NumericalOperand -- n * v u (
        VariableOperand -> currentTokenType == OpenParenthesis -- v * (
        _ -> False

canParseNumber :: Token -> Bool
canParseNumber token = 2 > length (filter (=='.') token) && all (`elem`'.':['0'..'9']) token

