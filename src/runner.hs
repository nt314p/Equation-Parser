import qualified Tokenizer (getTokens, getTokenValue, getTokenType, tokensToString)

tokens = Tokenizer.getTokens "k=sin(x)^2+5.3b"

main = putStrLn $ Tokenizer.tokensToString tokens