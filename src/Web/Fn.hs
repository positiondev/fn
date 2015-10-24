{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Fn ( RequestContext(..)
              , toWAI
              , route
              , (==>)
              , (//)
              , (/?)
              , path
              , segment
              , param
  ) where

import           Data.List          (find)
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
    let (Store r _) = requestLens (`Store` id) c
    in r
  setRequest :: c -> Request -> c
  setRequest c r =
    let (Store _ b) = requestLens (`Store` id) c
    in b r

toWAI :: RequestContext c => c -> (c -> IO Response) -> Application
toWAI ctxt f req cont = let ctxt' = setRequest ctxt req
                        in f ctxt' >>= cont

route :: RequestContext c => c -> [c -> IO (Maybe Response)] -> IO Response
route _ [] = return $ responseLBS status404 [] ""
route ctxt (x:xs) = do resp <- x ctxt
                       case resp of
                         Nothing -> route ctxt xs
                         Just response -> return response

type Req = ([Text], Query)

(==>) :: RequestContext c =>
         (Req -> t2 -> Maybe (Req, IO (Maybe t))) ->
         (c -> t2) ->
         c ->
         IO (Maybe t)
(match ==> handle) ctxt =
   let r = getRequest ctxt
       x = (pathInfo r, queryString r)
   in case match x (handle ctxt) of
        Nothing -> return Nothing
        Just (_, action) -> action


(//) :: (c -> t -> Maybe (c, a')) ->
        (c -> a' -> Maybe (c, a)) ->
        c ->
        t -> Maybe (c, a)
(match1 // match2) req k =
   case match1 req k of
     Nothing -> Nothing
     Just (req', k') -> match2 req' k'

(/?) :: (c -> t -> Maybe (c, a')) ->
        (c -> a' -> Maybe (c, a)) ->
        c ->
        t -> Maybe (c, a)
(/?) = (//)

segment :: Req -> (Text -> a) -> Maybe (Req, a)
segment req k =
  case fst req of
    (x:xs) -> Just ((xs, snd req), k x)
    _     -> Nothing

param :: Text -> Req -> (Text -> a) -> Maybe (Req, a)
param n req k =
  let match = find ((== T.encodeUtf8 n) . fst) (snd req)
  in case (maybe "" T.decodeUtf8 . snd) <$> match of
       Nothing -> Nothing
       Just p -> Just (req, k p)

path :: Text -> Req -> a -> Maybe (Req, a)
path s req k =
  case fst req of
    (x:xs) | x == s -> Just ((xs, snd req), k)
    _               -> Nothing
