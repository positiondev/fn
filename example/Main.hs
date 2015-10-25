{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Blaze.ByteString.Builder
import           Control.Arrow              (first)
import           Control.Exception          (SomeException, catch)
import           Control.Lens
import           Control.Logging
import           Control.Monad.Trans.Either
import           Data.List                  (intercalate)
import           Data.Monoid
import           Data.Pool
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.Redis             as R
import           Heist
import           Heist.Interpreted
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Util           as W
import           Web.Fn

data Ctxt = Ctxt { _req   :: Request
                 , _heist :: HeistState IO
                 , _db    :: Pool PG.Connection
                 , _redis :: R.Connection
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
       Right hs' -> do
         pgpool <- createPool (PG.connect (PG.ConnectInfo "localhost"
                                                          5432
                                                          "fn_user"
                                                          "111"
                                                          "fn_db"))
                              PG.close 1 60 20
         rconn <- R.connect R.defaultConnectInfo
         return (Ctxt defaultRequest hs' pgpool rconn)

main :: IO ()
main = withStdoutLogging $
       do log' "Starting server..."
          ctxt <- initializer
          log' "Listening port 8000..."
          catch (run 8000 $ toWAI ctxt app)
                (\(_ :: SomeException) ->
                   do log' "Shutting down..."
                      destroyAllResources (ctxt ^. db))


app :: Ctxt -> IO Response
app ctxt =
  route ctxt [end ==> indexHandler
             ,path "param" /? param "id" ==> paramHandler
             ,path "template" ==> templateHandler
             ,path "db" /? param "number" ==> dbHandler
             ,path "segment" // segment ==> segmentHandler
             ,path "redis" // segment /? paramOptional "set" ==> redisHandler
             ]
    `fallthrough` return (responseLBS status404 [] "Page not found.")

indexHandler :: Ctxt -> IO (Maybe Response)
indexHandler _ =
  Just <$>
    W.text status200 []
           ("Try /param?id=123, /template, /db?number=123, /segment/foo,"
           <> " or /redis/key or /redis/key?set=new")

paramHandler :: Ctxt -> Int -> IO (Maybe Response)
paramHandler _ i =
  Just <$> W.text status200
                  []
                  (T.pack (show i))

templateHandler :: Ctxt -> IO (Maybe Response)
templateHandler ctxt =
  do r <- renderTemplate (ctxt ^. heist) "template"
     case first toLazyByteString <$> r of
       Nothing -> return Nothing
       Just (h,m) -> Just <$> W.bytestring status200 [(hContentType, m)] h

dbHandler :: Ctxt -> Int -> IO (Maybe Response)
dbHandler ctxt n =
  do r <- withResource (ctxt ^. db) $ \c -> PG.query c "select ?" (PG.Only n)
     Just <$> W.text status200 []
                     (T.pack (show (r :: [[Int]])))

segmentHandler :: Ctxt -> Text -> IO (Maybe Response)
segmentHandler _ seg = Just <$> W.text status200 [] seg

redisHandler :: Ctxt -> Text -> Either Text Text -> IO (Maybe Response)
redisHandler ctxt key new =
  do res <- R.runRedis (ctxt ^. redis) $
              do let k = T.encodeUtf8 key
                 case new of
                   Left _ -> R.get k
                   Right new' -> R.getset k (T.encodeUtf8 new')
     Just <$> case res of
                Left err ->
                  W.text status500 []
                         (T.pack (show err))
                Right value ->
                  W.text status200 []
                         (T.pack (show value))
