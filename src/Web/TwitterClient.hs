{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.TwitterClient (acquireTempTokens, acquireAccessTokens, displayTimeline) where

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (ByteString(..), toStrict)
import Control.Applicative
import Control.Lens
import Network.Wreq
import Network.HTTP.Types.URI
import Web.TwitterClient.Types


findTokensInResponse :: ByteString -> Maybe TokenPair
findTokensInResponse response = do
  token       <- lookup "oauth_token" body
  tokenSecret <- lookup "oauth_token_secret" body
  return $ TokenPair token tokenSecret
    where body = parseQuery (toStrict response)

acquireTempTokens :: APIKeys -> CallbackURI -> IO TokenPair
acquireTempTokens APIKeys{..} callback = do
  r <- postWith opts "https://api.twitter.com/oauth/authenticate" ("" :: ByteString)
  case (r ^? responseBody) of
    Just response -> case findTokensInResponse response of
                       Just tokens -> return tokens
                       Nothing     -> fail "Couldn't find tokens in response from Twitter"
    Nothing       -> fail "Twitter replied with an empty body"
  where opts = defaults & auth ?~ oauth1Temp (pack consumerKey) (pack consumerSecret) (pack callback)

acquireAccessTokens :: APIKeys -> TokenPair -> OAuthVerifier -> IO TokenPair
acquireAccessTokens = undefined

displayTimeline :: TokenPair -> IO ()
displayTimeline = undefined
