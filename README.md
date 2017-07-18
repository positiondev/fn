## Fn (eff-enn) - a functional web framework.

> Or, how to do away with the monad transformers, and just use plain
> functions.

## Example

See the `example` directory for a full example, but a minimal
application is the following:


```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Lens
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
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
  route ctxt [ end ==> index
             , path "foo" // segment // path "baz" // param "id" ==> handler]
    `fallthrough` notFoundText "Page not found."

index :: IO (Maybe Response)
index = okText "This is the index page! Try /foo/bar/baz?id=10"

handler :: Text -> Int -> Ctxt -> IO (Maybe Response)
handler fragment i _ = okText (fragment <> " - " <> T.pack (show i))

```
