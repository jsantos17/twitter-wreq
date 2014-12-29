module Main (main) where

import Control.Monad (void)
import Web.TwitterClient (acquireTempTokens, acquireAccessTokens, displayTimeline)
import Web.TwitterClient.Types

main :: IO ()
main = do
  putStr "Input your consumer key: "
  consumerKey    <- getLine
  putStr "Input your consumer secret: "
  consumerSecret <- getLine
  putStr "Input your callback URI: "
  callback       <- getLine
  let apiKeys = APIKeys consumerKey consumerSecret
  tempTokens <- acquireTempTokens apiKeys callback
  putStr "Please go to https://api.twitter.com/oauth/authorize?oauth_token="
  putStrLn (token tempTokens)
  putStr "Hit enter when done"
  void getLine
  putStrLn "Input oauth_verifier"
  oauthVerifier <- getLine
  accessTokens  <- acquireAccessTokens apiKeys tempTokens oauthVerifier
  displayTimeline accessTokens
  return ()
