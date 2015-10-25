{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Fn ( RequestContext(..)
              , toWAI
              , route
              , fallthrough
              , (==>)
              , (//)
              , (/?)
              , path
              , end
              , segment
              , Param(..)
              , param
              , paramOptional
              , paramPresent
  ) where

import           Data.List          (find)
import           Data.Monoid        ((<>))
import           Data.Text          (Text)
import qualified Data.Text          as T
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

fallthrough :: IO (Maybe Response) -> IO Response -> IO Response
fallthrough a ft =
  do response <- a
     case response of
       Nothing -> ft
       Just r -> return r

route :: RequestContext c =>
         c ->
         [c -> Maybe (IO (Maybe Response))] ->
         IO (Maybe Response)
route _ [] = return Nothing
route ctxt (x:xs) =
  case x ctxt of
    Nothing -> route ctxt xs
    Just action ->
      do resp <- action
         case resp of
           Nothing -> route ctxt xs
           Just response -> return (Just response)

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
                Left _ -> Nothing
                Right p -> Just ((xs, snd req), k p)
    _     -> Nothing

class Param a where
  fromParam :: Text -> Either Text a

instance Param Text where
  fromParam = Right
instance Param Int where
  fromParam t = case decimal t of
                  Left msg -> Left (T.pack msg)
                  Right m | snd m /= "" ->
                            Left ("Incomplete match: " <> T.pack (show m))
                  Right (v, _) -> Right v
instance Param Double where
  fromParam t = case double t of
                  Left msg -> Left (T.pack msg)
                  Right m | snd m /= "" ->
                            Left ("Incomplete match: " <> T.pack (show m))
                  Right (v, _) -> Right v

param :: Param p => Text -> Req -> (p -> a) -> Maybe (Req, a)
param n req k =
  let match = find ((== T.encodeUtf8 n) . fst) (snd req)
  in case (maybe "" T.decodeUtf8 . snd) <$> match of
       Nothing -> Nothing
       Just p -> case fromParam p of
         Left _ -> Nothing
         Right p' -> Just (req, k p')

paramOptional :: Param p =>
                 Text ->
                 Req ->
                 (Either Text p -> a) ->
                 Maybe (Req, a)
paramOptional n req k =
  let match = find ((== T.encodeUtf8 n) . fst) (snd req)
      p = ((maybe "" T.decodeUtf8 . snd) <$> match)
  in case p of
       Nothing -> Just (req, k (Left "param missing"))
       Just p' -> Just (req, k (fromParam p'))


paramPresent :: Param p =>
                Text ->
                Req ->
                (Either Text p -> a) ->
                Maybe (Req, a)
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

end :: Req -> a -> Maybe (Req, a)
end req k =
  case fst req of
    [] -> Just (req, k)
    _ -> Nothing
