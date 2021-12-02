import qualified Syntax (getTokenType, getTokenValue, Token)
import qualified Tokenizer (getTokens, tokensToString)

tokens :: [Syntax.Token]
tokens = Tokenizer.getTokens "k=sin(x)^2+5.3b"

main :: IO()
main = putStrLn $ Tokenizer.tokensToString tokens