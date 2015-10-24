{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Lens
import           Data.Monoid
import           Data.Text                (Text)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Util         as W
import           Web.Fn

data Ctxt = Ctxt { _req :: Request
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

initializer :: IO Ctxt
initializer = return (Ctxt defaultRequest)

main :: IO ()
main = do context <- initializer
          run 8000 $ toWAI context app

app :: Ctxt -> IO Response
app ctxt =
  route ctxt [path "foo" // segment // path "baz" /? param "id" ==> handler]

handler :: Ctxt -> Text -> Text -> IO (Maybe Response)
handler _ fragment i =
  Just <$> W.text status200
                  []
                  (fragment <> " - " <> i)
