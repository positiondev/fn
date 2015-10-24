{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Lens
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TL
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Web.Fn

data App = App { _req :: Request
               }

makeLenses ''App

instance RequestContext App where
  requestLens = req

initializer :: IO App
initializer = return (App defaultRequest)

main :: IO ()
main = do context <- initializer
          run 8000 $ toWAI context app

app :: App -> IO Response
app ctxt =
  route ctxt [text "foo" // fragment // text "baz" /? param "id" ==> handler]

handler :: App -> Text -> Text -> IO (Maybe Response)
handler ctxt fragment i =
  return $ Just $ responseLBS status200
                              []
                              (TL.encodeUtf8 $ TL.fromStrict $ fragment <> " - " <> i)
