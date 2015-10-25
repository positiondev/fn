{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception        (SomeException (..), catch)
import           Control.Logging
import           Network.Wai.Handler.Warp

import           Site

main :: IO ()
main = withStdoutLogging $
       do log' "Starting server on port 8000..."
          (app', shutdown) <- app
          catch (run 8000 app')
                (\(_ :: SomeException) ->
                   do log' "Shutting down..."
                      shutdown)
