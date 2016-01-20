{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Applicative      ((<$>))
import           Control.Exception        (SomeException (..), catch)
import           Control.Logging
import           Data.Monoid
import qualified Data.Text                as T
import           Network.Wai.Handler.Warp
import           System.Environment

import           Site

main :: IO ()
main = withStdoutLogging $
       do port <- maybe 8000 read <$> lookupEnv "PORT"
          log' $ "Starting server on port " <> T.pack (show port) <> "..."
          (app', shutdown) <- app
          catch (run port app')
                (\(_ :: SomeException) ->
                   do log' "Shutting down..."
                      shutdown)
