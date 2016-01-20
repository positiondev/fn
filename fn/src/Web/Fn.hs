{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-|

This package provides a simple framework for routing and responses. The
two primary goals are:

1. All web handler functions are just plain IO. There is no Fn
monad, or monad transformer. This has a lot of nice properties,
foremost among them is that it is easier to call handlers from other
contexts (like GHCi, when testing, in other threads, etc). As a
result, these functions take a single extra parameter that
has the context that they need (like database connection pools, the
request, etc).

2. Web handlers are functions with typed parameters. When routing, we
specify many parameters (most commonly, numeric ids, but can be many
things), so the handlers should be functions that take those as
parameters.
-}


module Web.Fn ( -- * Application setup
                FnRequest
              , defaultFnRequest
              , RequestContext(..)
              , toWAI
                -- * Routing
              , Req
              , route
              , fallthrough
              , (==>)
              , (//)
              , (/?)
              , path
              , end
              , anything
              , segment
              , method
              , FromParam(..)
              , ParamError(..)
              , param
              , paramMany
              , paramOpt
              , File(..)
              , file
              , files
                -- * Responses
              , staticServe
              , okText
              , okHtml
              , errText
              , errHtml
              , notFoundText
              , notFoundHtml
              , redirect
  ) where

import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import           Control.Applicative                ((<$>))
import           Control.Arrow                      (second)
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as LB
import           Data.Either                        (rights)
import qualified Data.HashMap.Strict                as HM
import           Data.Maybe                         (fromJust)
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Text.Read                     (decimal, double)
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Parse                  (FileInfo (..), Param,
                                                     lbsBackEnd,
                                                     parseRequestBody)
import qualified Network.Wai.Parse                  as Parse
import           System.Directory                   (doesFileExist)
import           System.FilePath                    (takeExtension)

data Store b a = Store b (b -> a)
instance Functor (Store b) where
  fmap f (Store b h) = Store b (f . h)


-- | A normal WAI 'Request' and the parsed post body (if present). We can
-- only parse the body once, so we need to have our request (which we
-- pass around) to be able to have the parsed body.
type FnRequest = (Request, ([Param], [Parse.File LB.ByteString]))

-- | A default request, which is a WAI defaultRequest and no post info
defaultFnRequest :: FnRequest
defaultFnRequest = (defaultRequest, ([],[]))

-- | Specify the way that Fn can get the 'FnRequest' out of your context.
--
-- The easiest way to instantiate this is to use the lens, but if you
-- don't want to use lenses, define 'getRequest' and 'setRequest'.
--
-- Note that 'requestLens' is defined in terms of 'getRequest' and
-- 'setRequest' and vice-versa, so you need to define _one_ of these.
class RequestContext ctxt where
  requestLens :: Functor f => (FnRequest -> f FnRequest) -> ctxt -> f ctxt
  requestLens f c = setRequest c <$> f (getRequest c)
  getRequest :: ctxt -> FnRequest
  getRequest c =
    let (Store r _) = requestLens (`Store` id) c
    in r
  setRequest :: ctxt -> FnRequest -> ctxt
  setRequest c r =
    let (Store _ b) = requestLens (`Store` id) c
    in b r

instance RequestContext FnRequest where
  getRequest = id
  setRequest _ = id

-- | Convert an Fn application (provide a context, a context to response
-- function and we'll create a WAI application by updating the 'FnRequest'
-- value for each call).
toWAI :: RequestContext ctxt => ctxt -> (ctxt -> IO Response) -> Application
toWAI ctxt f req cont =
  do post <- parseRequestBody lbsBackEnd req
     let ctxt' = setRequest ctxt (req, post)
     f ctxt' >>= cont

-- | The main construct for Fn, 'route' takes a context (which it will pass
-- to all handlers) and a list of potential matches (which, once they
-- match, may still end up deciding not to handle the request - hence
-- the double 'Maybe'). It can be nested.
--
-- @
--  app c = route c [ end ==> index
--                  , path "foo" \/\/ path "bar" \/\/ segment \/? param "id ==> h]
--    where index :: Ctxt -> IO (Maybe Response)
--          index _ = okText "This is the index."
--          h :: Ctxt -> Text -> Text -> IO (Maybe Response)
--          h _ s i = okText ("got path \/foo\/" <> s <> ", with id=" <> i)
-- @
route :: RequestContext ctxt =>
         ctxt ->
         [ctxt -> Req -> Maybe (IO (Maybe Response))] ->
         IO (Maybe Response)
route ctxt pths =
  do let (r,post) = getRequest ctxt
         m = either (const GET) id (parseMethod (requestMethod r))
         req = (filter (/= "") (pathInfo r), queryString r, m, post)
     route' req pths
  where route' _ [] = return Nothing
        route' req (x:xs) =
          case x ctxt req of
            Nothing -> route' req xs
            Just action ->
              do resp <- action
                 case resp of
                   Nothing -> route' req xs
                   Just response -> return (Just response)

-- | The 'route' function (and all your handlers) return
-- 'IO (Maybe Response)', because each can elect to not respond (in
-- which case we will continue to match on routes). But to construct
-- an application, we need a response in the case that nothing matched
-- - this is what 'fallthrough' allows you to specify. In particular,
-- 'notFoundText' and 'notFoundHtml' may be useful.
fallthrough :: IO (Maybe Response) -> IO Response -> IO Response
fallthrough a ft =
  do response <- a
     case response of
       Nothing -> ft
       Just r -> return r

-- NOTE(dbp 2015-11-05): This list taken from snap-core, BSD3 licensed.
mimeMap :: HM.HashMap String ByteString
mimeMap =  HM.fromList [
  ( ".asc"     , "text/plain"                        ),
  ( ".asf"     , "video/x-ms-asf"                    ),
  ( ".asx"     , "video/x-ms-asf"                    ),
  ( ".avi"     , "video/x-msvideo"                   ),
  ( ".bz2"     , "application/x-bzip"                ),
  ( ".c"       , "text/plain"                        ),
  ( ".class"   , "application/octet-stream"          ),
  ( ".conf"    , "text/plain"                        ),
  ( ".cpp"     , "text/plain"                        ),
  ( ".css"     , "text/css"                          ),
  ( ".cxx"     , "text/plain"                        ),
  ( ".dtd"     , "text/xml"                          ),
  ( ".dvi"     , "application/x-dvi"                 ),
  ( ".gif"     , "image/gif"                         ),
  ( ".gz"      , "application/x-gzip"                ),
  ( ".hs"      , "text/plain"                        ),
  ( ".htm"     , "text/html"                         ),
  ( ".html"    , "text/html"                         ),
  ( ".ico"     , "image/x-icon"                      ),
  ( ".jar"     , "application/x-java-archive"        ),
  ( ".jpeg"    , "image/jpeg"                        ),
  ( ".jpg"     , "image/jpeg"                        ),
  ( ".js"      , "text/javascript"                   ),
  ( ".json"    , "application/json"                  ),
  ( ".log"     , "text/plain"                        ),
  ( ".m3u"     , "audio/x-mpegurl"                   ),
  ( ".mov"     , "video/quicktime"                   ),
  ( ".mp3"     , "audio/mpeg"                        ),
  ( ".mpeg"    , "video/mpeg"                        ),
  ( ".mpg"     , "video/mpeg"                        ),
  ( ".ogg"     , "application/ogg"                   ),
  ( ".pac"     , "application/x-ns-proxy-autoconfig" ),
  ( ".pdf"     , "application/pdf"                   ),
  ( ".png"     , "image/png"                         ),
  ( ".ps"      , "application/postscript"            ),
  ( ".qt"      , "video/quicktime"                   ),
  ( ".sig"     , "application/pgp-signature"         ),
  ( ".spl"     , "application/futuresplash"          ),
  ( ".svg"     , "image/svg+xml"                     ),
  ( ".swf"     , "application/x-shockwave-flash"     ),
  ( ".tar"     , "application/x-tar"                 ),
  ( ".tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( ".tar.gz"  , "application/x-tgz"                 ),
  ( ".tbz"     , "application/x-bzip-compressed-tar" ),
  ( ".text"    , "text/plain"                        ),
  ( ".tgz"     , "application/x-tgz"                 ),
  ( ".torrent" , "application/x-bittorrent"          ),
  ( ".ttf"     , "application/x-font-truetype"       ),
  ( ".txt"     , "text/plain"                        ),
  ( ".wav"     , "audio/x-wav"                       ),
  ( ".wax"     , "audio/x-ms-wax"                    ),
  ( ".wma"     , "audio/x-ms-wma"                    ),
  ( ".wmv"     , "video/x-ms-wmv"                    ),
  ( ".xbm"     , "image/x-xbitmap"                   ),
  ( ".xml"     , "text/xml"                          ),
  ( ".xpm"     , "image/x-xpixmap"                   ),
  ( ".xwd"     , "image/x-xwindowdump"               ),
  ( ".zip"     , "application/zip"                   ) ]


-- | Serves static files out of the specified path according to the
-- request path. Note that if you have matched parts of the path,
-- those will not be included in the path used to find the static
-- file. For example, if you have a file @static\/img\/a.png@, and do:
--
-- > path "img" ==> staticServe "static"
--
-- It will match @img\/img\/a.png@, not @img\/a.png@. If you wanted that,
-- you could:
--
-- > anything ==> staticServe "static"
--
-- If no file is found, this will continue routing.
staticServe :: RequestContext ctxt => Text -> ctxt -> IO (Maybe Response)
staticServe d ctxt = do
  let pth = T.unpack $ T.intercalate "/" $  d : pathInfo (fst . getRequest $ ctxt)
  exists <- doesFileExist pth
  if exists
     then do let ext = takeExtension pth
                 contentType = case HM.lookup ext mimeMap of
                                 Nothing -> []
                                 Just t -> [(hContentType, t)]
             return $ Just $ responseFile status200
                                          contentType
                                          pth
                                          Nothing
     else return Nothing

-- | The parts of the path, when split on /, and the query.
type Req = ([Text], Query, StdMethod, ([Param], [Parse.File LB.ByteString]))

-- | The connective between route patterns and the handler that will
-- be called if the pattern matches. The type is not particularly
-- illuminating, as it uses polymorphism to be able to match route
-- patterns with varying numbers (and types) of parts with functions
-- of the corresponding number of arguments and types.
(==>) :: RequestContext ctxt =>
         (Req -> Maybe (Req, k -> a)) ->
         (ctxt -> k) ->
         ctxt ->
         Req ->
         Maybe a
(match ==> handle) ctxt req =
   case match req of
     Nothing -> Nothing
     Just ((pathInfo',_,_,_), k) ->
       let (request, post) = getRequest ctxt in
       Just (k $ handle (setRequest ctxt (request { pathInfo = pathInfo' }, post)))

-- | Connects two path segments. Note that when normally used, the
-- type parameter r is 'Req'. It is more general here to facilitate
-- testing.
(//) :: (r -> Maybe (r, k -> k')) ->
        (r -> Maybe (r, k' -> a)) ->
        r -> Maybe (r, k -> a)
(match1 // match2) req =
   case match1 req of
     Nothing -> Nothing
     Just (req', k) -> case match2 req' of
                         Nothing -> Nothing
                         Just (req'', k') -> Just (req'', k' . k)

-- | Identical to '//', provided simply because it serves as a
-- nice visual difference when switching from 'path'/'segment' to
-- 'param' and friends.
(/?) :: (r -> Maybe (r, k -> k')) ->
        (r -> Maybe (r, k' -> a)) ->
        r -> Maybe (r, k -> a)
(/?) = (//)

-- | Matches a literal part of the path. If there is no path part
-- left, or the next part does not match, the whole match fails.
path :: Text -> Req -> Maybe (Req, a -> a)
path s req =
  case req of
    (y:ys,q,m,x) | y == s -> Just ((ys, q, m, x), id)
    _               -> Nothing

-- | Matches there being no parts of the path left. This is useful when
-- matching index routes.
end :: Req -> Maybe (Req, a -> a)
end req =
  case req of
    ([],_,_,_) -> Just (req, id)
    _ -> Nothing

-- | Matches anything.
anything :: Req -> Maybe (Req, a -> a)
anything req = Just (req, id)

-- | Captures a part of the path. It will parse the part into the type
-- specified by the handler it is matched to. If there is no segment, or
-- if the segment cannot be parsed as such, it won't match.
segment :: FromParam p => Req ->  Maybe (Req, (p -> a) -> a)
segment req =
  case req of
    (y:ys,q,m,x) -> case fromParam y of
                      Left _ -> Nothing
                      Right p -> Just ((ys, q, m, x), \k -> k p)
    _     -> Nothing

-- | Matches on a particular HTTP method.
method :: StdMethod -> Req -> Maybe (Req, a -> a)
method m r@(_,_,m',_) | m == m' = Just (r, id)
method _ _ = Nothing

data ParamError = ParamMissing | ParamUnparsable | ParamOtherError Text deriving (Eq, Show)

-- | A class that is used for parsing for 'param', 'paramOpt', 'paramMany'
-- and 'segment'.
class FromParam a where
  fromParam :: Text -> Either ParamError a

instance FromParam Text where
  fromParam = Right
instance FromParam Int where
  fromParam t = case decimal t of
                  Left _ -> Left ParamUnparsable
                  Right m | snd m /= "" ->
                            Left ParamUnparsable
                  Right (v, _) -> Right v
instance FromParam Double where
  fromParam t = case double t of
                  Left _ -> Left ParamUnparsable
                  Right m | snd m /= "" ->
                            Left ParamUnparsable
                  Right (v, _) -> Right v


findParamMatches :: FromParam p => Text -> [(ByteString, Maybe ByteString)] -> [Either ParamError p]
findParamMatches n ps = map (fromParam . maybe "" T.decodeUtf8 . snd) .
                        filter ((== T.encodeUtf8 n) . fst) $
                        ps

-- | Matches on a single query parameter of the given name. If there is no
-- parameters, or it cannot be parsed into the type needed by the
-- handler, it won't match.
param :: FromParam p => Text -> Req -> Maybe (Req, (p -> a) -> a)
param n req =
  let (_,q,_,(ps, _)) = req
  in case rights $ findParamMatches n q of
       [y] -> Just (req, \k -> k y)
       []  -> case rights $ findParamMatches n (map (second Just) ps) of
                [y] -> Just (req, \k -> k y)
                -- TODO(dbp 2015-11-05): It's too bad that the
                -- request body parsing will possibly be
                -- duplicated, in case nothing matches. Perhaps
                -- both branches should thread...
                _ -> Nothing
       _   -> Nothing

-- | Matches on query parameters of the given name. If there are no
-- parameters, or they cannot be parsed into the type needed by the
-- handler, it won't match.
paramMany :: FromParam p => Text -> Req -> Maybe (Req, ([p] -> a) -> a)
paramMany n req =
  let (_,q,_,(ps,_)) = req
  in case findParamMatches n (q ++ map (second Just) ps) of
       [] -> Nothing
       xs -> let ys = rights xs in
             if length ys == length xs
                then Just (req, \k -> k ys)
                else Nothing

-- | If the specified parameters are present, they will be parsed into the
-- type needed by the handler, but if they aren't present or cannot be
-- parsed, the handler will still be called.
paramOpt :: FromParam p =>
            Text ->
            Req ->
            Maybe (Req, (Either ParamError [p] -> a) -> a)
paramOpt n req =
  let (_,q,_,(ps, _)) = req
  in case findParamMatches n (q ++ map (second Just) ps) of
       [] -> Just (req, \k -> k (Left ParamMissing))
       ys -> Just (req, \k -> k (foldLefts [] ys))
  where foldLefts acc [] = Right (reverse acc)
        foldLefts _ (Left x : _) = Left x
        foldLefts acc (Right x : xs) = foldLefts (x : acc) xs


-- | An uploaded file.
data File = File { fileName        :: Text
                 , fileContentType :: Text
                 , fileContent     :: LB.ByteString
                 }

-- | Matches an uploaded file with the given parameter name.
file :: Text -> Req -> Maybe (Req, (File -> a) -> a)
file n req =
  let (_,_,_,(_, fs)) = req
  in case filter ((== T.encodeUtf8 n) . fst) fs of
       [(_, FileInfo nm ct c)] -> Just (req, \k -> k (File (T.decodeUtf8 nm)
                                                           (T.decodeUtf8 ct)
                                                           c))
       _ -> Nothing

-- | Matches all uploaded files, passing their parameter names and
-- contents.
files :: Req -> Maybe (Req, ([(Text, File)] -> a) -> a)
files req =
  let (_,_,_,(_, fs')) = req
      fs = map (\(n, FileInfo nm ct c) ->
                  (T.decodeUtf8 n, File (T.decodeUtf8 nm)
                                        (T.decodeUtf8 ct)
                                        c))
               fs'
  in Just (req, \k -> k fs)

returnText :: Text -> Status -> ByteString -> IO (Maybe Response)
returnText text status content =
  return $ Just $
    responseBuilder status
                    [(hContentType, content)]
                    (B.fromText text)

plainText :: ByteString
plainText = "text/plain; charset=utf-8"

html :: ByteString
html = "text/html; charset=utf-8"

-- | Returns 'Text' as a response.
okText :: Text -> IO (Maybe Response)
okText t = returnText t status200 plainText

-- | Returns Html (in 'Text') as a response.
okHtml :: Text -> IO (Maybe Response)
okHtml t = returnText t status200 html

-- | Returns 'Text' as a response with a 500 status code.
errText :: Text -> IO (Maybe Response)
errText t = returnText t status500 plainText

-- | Returns Html (in 'Text') as a response with a 500 status code.
errHtml :: Text -> IO (Maybe Response)
errHtml t = returnText t status500 html

-- | Returns a 404 with the given 'Text' as a body. Note that this
-- returns a 'IO Response' not an 'IO (Maybe Response)' because the
-- expectaiton is that you are calling this with 'fallthrough'.
notFoundText :: Text -> IO Response
notFoundText t = fromJust <$> returnText t status404 plainText

-- | Returns a 404 with the given html as a body. Note that this
-- returns a 'IO Response' not an 'IO (Maybe Response)' because the
-- expectaiton is that you are calling this with 'fallthrough'.
notFoundHtml :: Text -> IO Response
notFoundHtml t = fromJust <$> returnText t status404 html

-- | Redirects to the given url. Note that the target is not validated,
-- so it should be an absolute path/url.
redirect :: Text -> IO (Maybe Response)
redirect target =
  return $ Just $
    responseBuilder status303
                    [(hLocation, T.encodeUtf8 target)]
                    (B.fromText "")
