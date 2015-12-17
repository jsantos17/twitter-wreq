module Main (main) where

import Control.Monad (void)
import Web.TwitterClient (acquireTempTokens, acquireAccessTokens, displayTimeline)
import Web.TwitterClient.Types

main :: IO ()
main = do
  putStrLn "Input your consumer key: "
  consumerKey    <- getLine
  putStrLn "Input your consumer secret: "
  consumerSecret <- getLine
  putStrLn "Input your callback URI: "
  callback       <- getLined
  let apiKeys = APIKeys consumerKey consumerSecret
  tempTokens <- acquireTempTokens apiKeys callback
  putStrLn $ "Please go to https://api.twitter.com/oauth/authorize?oauth_token=" ++ token tempTokens
  putStrLn "Input oauth_verifier"
  oauthVerifier <- getLine
  accessTokens  <- acquireAccessTokens apiKeys tempTokens oauthVerifier
  displayTimeline apiKeys accessTokens
  return ()
