module Web.TwitterClient.Types (
      Tokens
    , CallbackURI
    , OAuthVerifier
    , APIKeys(..)
    , TokenPair(..)
    ) where

type Tokens = (String, String)
type CallbackURI = String
type OAuthVerifier = String

data APIKeys = APIKeys { consumerKey    :: String
                       , consumerSecret :: String } deriving (Show)

data TokenPair = TokenPair { token       :: String
                           , tokenSecret :: String
                           , screenName  :: Maybe String } deriving (Show)
