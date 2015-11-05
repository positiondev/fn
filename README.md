## Fn (eff-enn) - a functional web framework.

> Or, how to do away with the monad transformers, and just use plain
> functions.

## Example

See the `example` directory for a full example, but a minimal
application is the following:


```
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
             , path "foo" // segment // path "baz" /? param "id" ==> handler]
    `fallthrough` notFoundText "Page not found."

index :: IO (Maybe Response)
index = okText "This is the index page! Try /foo/bar/baz?id=10"

handler :: Text -> Int -> Ctxt -> IO (Maybe Response)
handler fragment i _ = okText (fragment <> " - " <> T.pack (show i))

```


## Recommended Pairings

Part of the design of `Fn` is that you won't have a suite of `fn-foo`
libraries that generally serve to adapt the functions from `foo` to
the monad transformer stack of the web framework of choice (we do have
an `fn-extra` package that has code to adapt libraries that have a
different style from `fn`, especially when they are libraries we use a
lot!). Still, it's helpful to know what are common tools that are well
designed and tested, so here are a list (those marked with `[*]` are
used in the example application included in the repository):

- [warp](http://hackage.haskell.org/package/warp)`[*]`: perhaps obvious,
  but you will need to choose an HTTP server to use with your `Fn`
  application, and `warp` is the defacto standard for applications that
  use the `WAI` interface that `Fn` does.
- [heist](http://hackage.haskell.org/package/heist)`[*]`: a wonderful
  templating system that is both really simple (the templates are just
  html) and powerful (any html tag can be bound to run haskell
  code). This is one library that has adaptors in `fn-extra`, as we
  wanted to have splices (those haskell-bound html tags) that were
  normal functions, rather than monadic.
- [postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)`[*]`:
  a well designed interface to PostgreSQL; ofter the lower level way
  to interact with the database (setting up connections, etc), if you
  use a higher level, safer abstraction like `opaleye` (below) for actual
  queries. Use it with
  [resource-pool](https://hackage.haskell.org/package/resource-pool)`[*]`
  to have it manage many connections.
- [opaleye](https://hackage.haskell.org/package/opaleye): a type-safe
  composable way to write database queries against PostgreSQL.
- [hedis](https://hackage.haskell.org/package/hedis)`[*]`: a full-featured
  client for the key-value store Redis.
- [logging](https://hackage.haskell.org/package/logging)`[*]`: a simple
  library for writing log messages, which allow you to change the
  logging level and suppress some subset of messages.
- [hspec](https://hackage.haskell.org/package/hspec)`[*]`: a
  full-featured testing framework. Use with
  [hspec-wai](https://hackage.haskell.org/package/hspec-wai)`[*]` -
  though the latter could use some work to make it do everything it
  needs to!
- [wai-session](https://hackage.haskell.org/package/wai-session)`[*]`:
  Combine with something like
  [wai-session-clientsession](https://hackage.haskell.org/package/wai-session-clientsession)`[*]`
  to store session data in encrypted cookies (like, who a user is
  logged in as).
- [dotenv](http://hackage.haskell.org/package/dotenv): Loads a file
  full of `KEY=value` lines into environment variables. Convention is
  to have a `.env` file, not checked into version control, with
  development configuration variables (like database server, username,
  password, api keys, etc), so that in production (where the `.env`
  file is missing) you can have the real environment variables set by
  whatever mechanism you have (Heroku uses this as it's primary
  mechanism, for example).
