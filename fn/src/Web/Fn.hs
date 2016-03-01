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
              , (!=>)
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
              , sendFile
              , okText
              , okHtml
              , errText
              , errHtml
              , notFoundText
              , notFoundHtml
              , redirect
              , redirectReferer
  ) where

import qualified Blaze.ByteString.Builder.Char.Utf8 as B
import           Control.Applicative                ((<$>))
import           Control.Arrow                      (second)
import           Control.Concurrent.MVar
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Lazy               as LB
import           Data.Either                        (lefts, rights)
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


type PostMVar = Maybe (MVar (Maybe ([Param], [Parse.File LB.ByteString])))

-- | A normal WAI 'Request' and the parsed post body (if present). We can
-- only parse the body once, so we need to have our request (which we
-- pass around) to be able to have the parsed body.
type FnRequest = (Request, PostMVar)

-- | A default request, which is a WAI defaultRequest and no post info
defaultFnRequest :: FnRequest
defaultFnRequest = (defaultRequest, Nothing)

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
  do mv <- newMVar Nothing
     f (setRequest ctxt (req, Just mv)) >>= cont

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
         [ctxt -> Req -> IO (Maybe (IO (Maybe Response)))] ->
         IO (Maybe Response)
route ctxt pths =
  do let (r,post) = getRequest ctxt
         m = either (const GET) id (parseMethod (requestMethod r))
         req = (filter (/= "") (pathInfo r), queryString r, m, post)
     route' req pths
  where route' _ [] = return Nothing
        route' req (x:xs) =
          do mact <- x ctxt req
             case mact of
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
  sendFile pth

-- | Sends a specific file specified by path. It will specify the
-- content-type if it can figure it out by the file extension.
--
-- If no file exists at the given path, it will keep routing.
sendFile :: FilePath -> IO (Maybe Response)
sendFile pth =
  do exists <- doesFileExist pth
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
type Req = ([Text], Query, StdMethod, PostMVar)

-- | The non-body parsing connective between route patterns and the
-- handler that will be called if the pattern matches. The type is not
-- particularly illuminating, as it uses polymorphism to be able to
-- match route patterns with varying numbers (and types) of parts with
-- functions of the corresponding number of arguments and types.
(==>) :: RequestContext ctxt =>
         (Req -> IO (Maybe (Req, k -> a))) ->
         (ctxt -> k) ->
         ctxt ->
         Req ->
         IO (Maybe a)
(match ==> handle) ctxt req =
   do rsp <- match req
      case rsp of
        Nothing -> return Nothing
        Just ((pathInfo',_,_,_), k) ->
          let (request, mv) = getRequest ctxt in
          return $ Just (k $ handle (setRequest ctxt (request { pathInfo = pathInfo' }, mv)))

-- | The connective between route patterns and the handler that parses
-- the body, which allows post params to be extracted with 'param' and
-- allows 'file' to work (otherwise, it will trigger a runtime error).
(!=>) :: RequestContext ctxt =>
         (Req -> IO (Maybe (Req, k -> a))) ->
         (ctxt -> k) ->
         ctxt ->
         Req ->
         IO (Maybe a)
(match !=> handle) ctxt req =
   do let (request, Just mv) = getRequest ctxt
      modifyMVar_ mv (\r -> case r of
                              Nothing -> Just <$> parseRequestBody lbsBackEnd request
                              Just _ -> return r)
      rsp <- match req
      case rsp of
        Nothing -> return Nothing
        Just ((pathInfo',_,_,_), k) ->
          do return $ Just (k $ handle (setRequest ctxt (request { pathInfo = pathInfo' }, Just mv)))

-- | Connects two path segments. Note that when normally used, the
-- type parameter r is 'Req'. It is more general here to facilitate
-- testing.
(//) :: (r -> IO (Maybe (r, k -> k'))) ->
        (r -> IO (Maybe (r, k' -> a))) ->
        r -> IO (Maybe (r, k -> a))
(match1 // match2) req = do
  r1 <- match1 req
  case r1 of
    Nothing -> return Nothing
    Just (req', k) ->
      do r2 <- match2 req'
         return $ case r2 of
                    Nothing -> Nothing
                    Just (req'', k') -> Just (req'', k' . k)

{-# DEPRECATED (/?) "Use the identical '//' instead." #-}
-- | A synonym for '//'. To be removed
(/?) :: (r -> IO (Maybe (r, k -> k'))) ->
        (r -> IO (Maybe (r, k' -> a))) ->
        r -> IO (Maybe (r, k -> a))
(/?) = (//)

-- | Matches a literal part of the path. If there is no path part
-- left, or the next part does not match, the whole match fails.
path :: Text -> Req -> IO (Maybe (Req, a -> a))
path s req =
  return $ case req of
             (y:ys,q,m,x) | y == s -> Just ((ys, q, m, x), id)
             _               -> Nothing

-- | Matches there being no parts of the path left. This is useful when
-- matching index routes.
end :: Req -> IO (Maybe (Req, a -> a))
end req =
  return $ case req of
             ([],_,_,_) -> Just (req, id)
             _ -> Nothing

-- | Matches anything.
anything :: Req -> IO (Maybe (Req, a -> a))
anything req = return $ Just (req, id)

-- | Captures a part of the path. It will parse the part into the type
-- specified by the handler it is matched to. If there is no segment, or
-- if the segment cannot be parsed as such, it won't match.
segment :: FromParam p => Req -> IO (Maybe (Req, (p -> a) -> a))
segment req =
  return $ case req of
             (y:ys,q,m,x) -> case fromParam [y] of
                               Left _ -> Nothing
                               Right p -> Just ((ys, q, m, x), \k -> k p)
             _     -> Nothing

-- | Matches on a particular HTTP method.
method :: StdMethod -> Req -> IO (Maybe (Req, a -> a))
method m r@(_,_,m',_) | m == m' = return $ Just (r, id)
method _ _ = return Nothing

data ParamError = ParamMissing | ParamTooMany | ParamUnparsable | ParamOtherError Text deriving (Eq, Show)

-- | A class that is used for parsing for 'param' and 'paramOpt'.
-- and 'segment'.
class FromParam a where
  fromParam :: [Text] -> Either ParamError a

instance FromParam Text where
  fromParam [x] = Right x
  fromParam [] = Left ParamMissing
  fromParam _ = Left ParamTooMany
instance FromParam Int where
  fromParam [t] = case decimal t of
                    Left _ -> Left ParamUnparsable
                    Right m | snd m /= "" ->
                              Left ParamUnparsable
                    Right (v, _) -> Right v
  fromParam [] = Left ParamMissing
  fromParam _ = Left ParamTooMany
instance FromParam Double where
  fromParam [t] = case double t of
                    Left _ -> Left ParamUnparsable
                    Right m | snd m /= "" ->
                              Left ParamUnparsable
                    Right (v, _) -> Right v
  fromParam [] = Left ParamMissing
  fromParam _ = Left ParamTooMany
instance FromParam a => FromParam [a] where
  fromParam ps = let res = map (fromParam . (:[])) ps in
                 case lefts res of
                   [] -> Right $ rights res
                   _ -> Left $ ParamOtherError "Couldn't parse all parameters."
instance FromParam a => FromParam (Maybe a) where
  fromParam [x] = Just <$> fromParam [x]
  fromParam [] = Right Nothing
  fromParam _ = Left ParamTooMany

findParamMatches :: FromParam p => Text -> [(ByteString, Maybe ByteString)] -> Either ParamError p
findParamMatches n ps = fromParam .
                        map (maybe "" T.decodeUtf8 . snd) .
                        filter ((== T.encodeUtf8 n) . fst) $
                        ps

getMVarParams mv = case mv of
                     Just mv' -> do v <- readMVar mv'
                                    return $ case v of
                                               Nothing -> []
                                               Just (ps',_) -> ps'
                     Nothing -> return []

-- | Matches on a query parameter of the given name. It is parsed into
-- the type needed by the handler, which can be a 'Maybe' type if the
-- parameter is optional, or a list type if there can be many. If the
-- parameters cannot be parsed into the type needed by the handler, it
-- won't match.
--
-- Note: If you have used the '!=>' connective, so that the request
-- body has been parsed, this will also match post parameters (and
-- will combine the two together). If you haven't used that connective
-- (so the pattern is matched to handler with '==>'), it will only
-- match query parameters.
param :: FromParam p => Text -> Req -> IO (Maybe (Req, (p -> a) -> a))
param n req =
  do let (_,q,_,mv) = req
     ps <- getMVarParams mv
     return $ case findParamMatches n q of
                Right y -> Just (req, \k -> k y)
                Left _  -> case findParamMatches n (map (second Just) ps) of
                             Right y -> Just (req, \k -> k y)
                             Left _ -> Nothing

{-# DEPRECATED paramMany "Use 'param' with a list type, or define param parsing for non-empty list." #-}
-- | Matches on query parameters of the given name. If there are no
-- parameters, or they cannot be parsed into the type needed by the
-- handler, it won't match.
paramMany :: FromParam p => Text -> Req -> IO (Maybe (Req, ([p] -> a) -> a))
paramMany n req =
  do let (_,q,_,mv) = req
     ps <- getMVarParams mv
     return $ case findParamMatches n (q ++ map (second Just) ps) of
                Left _ -> Nothing
                Right ys -> Just (req, \k -> k ys)

-- | If the specified parameters are present, they will be parsed into the
-- type needed by the handler, but if they aren't present or cannot be
-- parsed, the handler will still be called.
--
-- Note: If you have used the '!=>' connective, so that the request
-- body has been parsed, this will also match post parameters (and
-- will combine the two together). If you haven't used that connective
-- (so the pattern is matched to handler with '==>'), it will only
-- match query parameters.
paramOpt :: FromParam p =>
            Text ->
            Req ->
            IO (Maybe (Req, (Either ParamError p -> a) -> a))
paramOpt n req =
  do let (_,q,_,mv) = req
     ps <- getMVarParams mv
     return $ Just (req, \k -> k (findParamMatches n (q ++ map (second Just) ps)))


-- | An uploaded file.
data File = File { fileName        :: Text
                 , fileContentType :: Text
                 , fileContent     :: LB.ByteString
                 }

getMVarFiles mv = case mv of
                    Nothing -> error $ "Fn: tried to read a 'file' or 'files', but FnRequest wasn't initialized with MVar."
                    Just mv' -> do
                      v <- readMVar mv'
                      case v of
                        Nothing -> error $ "Fn: tried to read a 'file' or 'files' from the request without parsing the body with '!=>'"
                        Just (_,fs') -> return fs'

-- | Matches an uploaded file with the given parameter name.
--
-- Note: You must use the '!=>' connective between the pattern and the
-- handler, or else the request body will not have been parsed and
-- this will fail.
file :: Text -> Req -> IO (Maybe (Req, (File -> a) -> a))
file n req =
  do let (_,_,_,mv) = req
     fs <- getMVarFiles mv
     return $ case filter ((== T.encodeUtf8 n) . fst) fs of
                [(_, FileInfo nm ct c)] -> Just (req, \k -> k (File (T.decodeUtf8 nm)
                                                                    (T.decodeUtf8 ct)
                                                                    c))
                _ -> Nothing

-- | Matches all uploaded files, passing their parameter names and
-- contents.
--
-- Note: You must use the '!=>' connective between the pattern and the
-- handler, or else the request body will not have been parsed and
-- this will fail.
files :: Req -> IO (Maybe (Req, ([(Text, File)] -> a) -> a))
files req =
  do let (_,_,_,Just mv) = req
     v <- readMVar mv
     let fs' = case v of
                 Nothing -> error $ "Fn: tried to read a 'file' from the request without parsing the body with '!=>'"
                 Just (_,fs) -> fs
     let fs = map (\(n, FileInfo nm ct c) ->
                 (T.decodeUtf8 n, File (T.decodeUtf8 nm)
                                       (T.decodeUtf8 ct)
                                       c))
              fs'
     return $ Just (req, \k -> k fs)

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

-- | Redirects to the referrer, if present in headers, else to "/".
redirectReferer :: RequestContext ctxt => ctxt -> IO (Maybe Response)
redirectReferer ctxt =
  let rs = requestHeaders $ fst $ getRequest ctxt in
  case lookup hReferer rs of
    Nothing -> redirect "/"
    Just r -> redirect (T.decodeUtf8 r)
