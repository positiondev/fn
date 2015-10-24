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
              , Param(..)
              , param
              , paramOptional
              , paramPresent
  ) where

import           Data.List          (find)
import           Data.Text          (Text)
import qualified Data.Text.Encoding as T
import           Data.Text.Read     (decimal, double)
import           Network.HTTP.Types
import           Network.Wai

data Store b a = Store b (b -> a)
instance Functor (Store b) where
  fmap f (Store b h) = Store b (f . h)

class RequestContext c where
  requestLens :: Functor f => (Request -> f Request) -> c -> f c
  requestLens f c = setRequest c <$> f (getRequest c)
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

route :: RequestContext c =>
         c ->
         [c -> Maybe (IO (Maybe Response))] ->
         IO Response
route _ [] = return $ responseLBS status404 [] ""
route ctxt (x:xs) =
  case x ctxt of
    Nothing -> route ctxt xs
    Just action ->
      do resp <- action
         case resp of
           Nothing -> route ctxt xs
           Just response -> return response

type Req = ([Text], Query)

(==>) :: RequestContext c =>
         (Req -> t2 -> Maybe (Req, a)) ->
         (c -> t2) ->
         c -> Maybe a
(match ==> handle) ctxt =
   let r = getRequest ctxt
       x = (pathInfo r, queryString r)
   in case match x (handle ctxt) of
        Nothing -> Nothing
        Just (_, action) -> Just action


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

segment :: Param p => Req -> (p -> a) -> Maybe (Req, a)
segment req k =
  case fst req of
    (x:xs) -> case fromParam x of
                Nothing -> Nothing
                Just p -> Just ((xs, snd req), k p)
    _     -> Nothing

class Param a where
  fromParam :: Text -> Maybe a

instance Param Text where
  fromParam = Just
instance Param Int where
  fromParam t = case decimal t of
                  Left _ -> Nothing
                  Right (_, x) | x /= "" -> Nothing
                  Right (v, _) -> Just v
instance Param Double where
  fromParam t = case double t of
                  Left _ -> Nothing
                  Right (_, x) | x /= "" -> Nothing
                  Right (v, _) -> Just v

param :: Param p => Text -> Req -> (p -> a) -> Maybe (Req, a)
param n req k =
  let match = find ((== T.encodeUtf8 n) . fst) (snd req)
  in case ((maybe "" T.decodeUtf8 . snd) <$> match) >>= fromParam of
       Nothing -> Nothing
       Just p -> Just (req, k p)

paramOptional :: Param p => Text -> Req -> (Maybe p -> a) -> Maybe (Req, a)
paramOptional n req k =
  let match = find ((== T.encodeUtf8 n) . fst) (snd req)
      p = ((maybe "" T.decodeUtf8 . snd) <$> match) >>= fromParam
  in Just (req, k p)

paramPresent :: Param p => Text -> Req -> (Maybe p -> a) -> Maybe (Req, a)
paramPresent n req k =
  let match = find ((== T.encodeUtf8 n) . fst) (snd req)
      p = ((maybe "" T.decodeUtf8 . snd) <$> match)
  in case p of
       Nothing -> Nothing
       Just p' -> Just (req, k (fromParam p'))

path :: Text -> Req -> a -> Maybe (Req, a)
path s req k =
  case fst req of
    (x:xs) | x == s -> Just ((xs, snd req), k)
    _               -> Nothing
