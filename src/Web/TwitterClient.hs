{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.TwitterClient (acquireTempTokens, acquireAccessTokens, displayTimeline) where

import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Lazy (ByteString(..), toStrict)
import Data.Maybe
import Control.Applicative
import Control.Lens
import Network.Wreq
import Network.HTTP.Types.URI
import Web.TwitterClient.Types


findTokensInResponse :: ByteString -> Maybe TokenPair
findTokensInResponse response = do
  token       <- lookup "oauth_token" body
  tokenSecret <- lookup "oauth_token_secret" body
  return $ TokenPair (unpack token) (unpack tokenSecret) Nothing
    where body = parseSimpleQuery (toStrict response)

findAccessTokensInResponse :: ByteString -> Maybe TokenPair
findAccessTokensInResponse response = do
  token       <- lookup "oauth_token" body
  tokenSecret <- lookup "oauth_token_secret" body
  screenName  <- lookup "screen_name" body
  return $ TokenPair (unpack token) (unpack tokenSecret) (Just (unpack screenName))
    where body = parseSimpleQuery (toStrict response)

acquireTempTokens :: APIKeys -> CallbackURI -> IO TokenPair
acquireTempTokens APIKeys{..} callback = do
  r <- postWith opts "https://api.twitter.com/oauth/request_token" ("" :: ByteString)
  case r ^? responseBody of
    Just response -> case findTokensInResponse response of
                       Just tokens -> return tokens
                       Nothing     -> fail "Couldn't find tokens in response from Twitter"
    Nothing       -> fail "Twitter replied with an empty body"
  where opts = defaults & auth ?~ oauth1Temp (pack consumerKey) (pack consumerSecret) (pack callback)

acquireAccessTokens :: APIKeys -> TokenPair -> OAuthVerifier -> IO TokenPair
acquireAccessTokens APIKeys{..} TokenPair{..} oauthVerifier = do
  r <- postWith opts "https://api.twitter.com/oauth/access_token" ("" :: ByteString)
  case r ^? responseBody of
    Just response -> case findAccessTokensInResponse response of
                       Just tokens -> return tokens
                       Nothing     -> fail "Couldn't find tokens in response from Twitter"
    Nothing       -> fail "Twitter replied with an empty body"
  where opts = defaults & auth ?~ oauth1ReqAccessToken (pack consumerKey) (pack consumerSecret)
                                                       (pack token) (pack tokenSecret) (pack oauthVerifier)

displayTimeline :: APIKeys -> TokenPair -> IO ()
displayTimeline APIKeys{..} TokenPair{..} = do
  putStrLn $ "User: " ++ fromJust screenName
  r <- getWith opts "https://api.twitter.com/1.1/statuses/home_timeline.json"
  putStrLn "Timeline:"
  case r ^? responseBody of
    Just body -> mapM_ print (parseTimeline body)
    Nothing   -> fail "Couldn't parse timeline"
  where opts = defaults & auth ?~ oauth1Auth (pack consumerKey) (pack consumerSecret)
                                             (pack token) (pack tokenSecret)

parseTimeline :: ByteString -> [String]
parseTimeline timeline =
  let tl = decode timeline :: Maybe Value in
  tl & catMaybes . toListOf (traverseArray . key "text")
