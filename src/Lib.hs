{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( listBucket
  ) where

import Control.Lens ((&), (.~), (<&>), (?~))
import Control.Monad.Trans.Resource (liftResourceT, runResourceT)

import Data.Text (Text)

import System.IO (stdout)

import qualified Data.Text as Text
import qualified Network.Google as Google
import Network.Google.Auth as Auth
import Network.Google.Resource.Storage.Objects.List (objectsList)
import qualified Network.Google.Storage as Storage
import Network.HTTP.Conduit (Manager, newManager, tlsManagerSettings)

listBucket :: IO ()
listBucket = do
  let serviceAccountKey = "path-to-service-account-key.json"
  credentials <- Auth.fromFilePath serviceAccountKey
  manager <- newManager tlsManagerSettings
  lgr <- Google.newLogger Google.Debug stdout
  env <-
    Google.newEnvWith credentials (\_ _ -> pure ()) manager <&> (Google.envLogger .~ lgr) .
    (Google.envScopes .~ Storage.storageReadWriteScope)
  objects <- runResourceT . Google.runGoogle env $ Google.send (objectsList "mybucket")
  print $ show objects