{-# LANGUAGE OverloadedStrings #-}

module Web.Fn where

import           Data.List          (find)
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types
import           Network.Wai

data Store b a = Store b (b -> a)
instance Functor (Store b) where
  fmap f (Store b h) = Store b (f . h)

class RequestContext c where
  requestLens :: Functor f => (Request -> f Request) -> c -> f c
  getRequest :: c -> Request
  getRequest c =
    let (Store r _) = requestLens (\piece -> Store piece id) c
    in r
  setRequest :: c -> Request -> c
  setRequest c r =
    let (Store _ b) = requestLens (\piece -> Store piece id) c
    in b r

toWAI :: RequestContext c => c -> (c -> IO Response) -> Application
toWAI ctxt f req cont = let ctxt' = setRequest ctxt req
                        in f ctxt' >>= cont

route :: RequestContext c => c -> [c -> IO (Maybe Response)] -> IO Response
route ctxt [] = return $ responseLBS status404 [] ""
route ctxt (x:xs) = do resp <- x ctxt
                       case resp of
                         Nothing -> route ctxt xs
                         Just response -> return response

type ReqInfo = ([Text], Query)

(==>) :: RequestContext c =>
         (ReqInfo -> t2 -> Maybe (ReqInfo, IO (Maybe t))) ->
         (c -> t2) ->
         c ->
         IO (Maybe t)
(==>) path handle = \ctxt ->
                      let r = getRequest ctxt
                          x = (pathInfo r, queryString r)
                      in case (path x (handle ctxt)) of
                           Nothing -> return Nothing
                           Just (_, action) -> action


(//) :: (c -> t -> Maybe (c, a')) ->
        (c -> a' -> Maybe (c, a)) ->
        c ->
        (t -> Maybe (c, a))
(//) path1 path2 = \ctxt k ->
                     case path1 ctxt k of
                       Nothing -> Nothing
                       Just (ctxt', k') -> path2 ctxt' k'


(/?) = (//)


fragment :: ReqInfo -> (Text -> a) -> Maybe (ReqInfo, a)
fragment = \ctxt k -> case fst ctxt of
                        (x:xs) -> Just ((xs, snd ctxt), k x)
                        _     -> Nothing

param :: Text -> ReqInfo -> (Text -> a) -> Maybe (ReqInfo, a)
param n = \ctxt k ->
            let match = find ((== T.encodeUtf8 n) . fst) (snd ctxt)
            in case (maybe "" T.decodeUtf8 . snd) <$> match of
                 Nothing -> Nothing
                 Just p -> Just (ctxt, k p)

text :: Text -> ReqInfo -> a -> Maybe (ReqInfo, a)
text t = \ctxt k -> case fst ctxt of
                      (x:xs) | x == t -> Just ((xs, snd ctxt), k)
                      _              -> Nothing
