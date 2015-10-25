{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception        (SomeException (..), catch)
import           Control.Lens             ((^.))
import           Control.Logging
import           Data.Pool                (destroyAllResources)
import           Network.Wai.Handler.Warp
import           Web.Fn

import           Site

main :: IO ()
main = withStdoutLogging $
       do log' "Starting server..."
          ctxt <- initializer
          log' "Listening port 8000..."
          catch (run 8000 $ toWAI ctxt app)
                (\(_ :: SomeException) ->
                   do log' "Shutting down..."
                      destroyAllResources (ctxt ^. db))
