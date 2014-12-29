{-# LANGUAGE RecordWildCards #-}

module Web.TwitterClient (acquireTempTokens, acquireAccessTokens, displayTimeline) where

import Control.Lens
import Network.Wreq
import Web.TwitterClient.Types


acquireTempTokens :: APIKeys -> CallbackURI -> IO TokenPair
acquireTempTokens APIKeys{..} callback = undefined

acquireAccessTokens :: APIKeys -> TokenPair -> OAuthVerifier -> IO TokenPair
acquireAccessTokens = undefined

displayTimeline :: TokenPair -> IO ()
displayTimeline = undefined
