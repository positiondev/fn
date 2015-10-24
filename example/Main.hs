{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Blaze.ByteString.Builder
import           Control.Arrow              (first)
import           Control.Lens
import           Control.Monad.Trans.Either
import           Data.List                  (intercalate)
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Heist
import           Heist.Interpreted
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Util           as W
import           Web.Fn

data Ctxt = Ctxt { _req   :: Request
                 , _heist :: HeistState IO
                 }

makeLenses ''Ctxt

instance RequestContext Ctxt where
  requestLens = req

initializer :: IO Ctxt
initializer =
  do let ts = loadTemplates "example/templates"
     hs <- runEitherT $
           initHeist (emptyHeistConfig & hcTemplateLocations .~ [ts])
     case hs of
       Left ers -> error ("Heist failed to load templates: \n" <>
                          intercalate "\n" ers)
       Right hs' -> return (Ctxt defaultRequest hs')

main :: IO ()
main = do context <- initializer
          run 8000 $ toWAI context app

app :: Ctxt -> IO Response
app ctxt =
  route ctxt [path "foo" // segment // path "baz" /? param "id" ==> handler
             ,path "template" ==> template]
    `fallthrough` return (responseLBS status404 [] "Page not found.")

handler :: Ctxt -> Text -> Int -> IO (Maybe Response)
handler _ fragment i =
  Just <$> W.text status200
                  []
                  (fragment <> " - " <> T.pack (show i))

template :: Ctxt -> IO (Maybe Response)
template ctxt =
  do r <- renderTemplate (ctxt ^. heist) "template"
     case first toLazyByteString <$> r of
       Nothing -> return Nothing
       Just (h,m) -> Just <$> W.bytestring status200 [(hContentType, m)] h
