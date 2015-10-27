{-# LANGUAGE OverloadedStrings #-}

module Web.Fn.Extra.Heist ( HeistContext(..)
                          , FnHeistState
                          , FnSplice
                          , heistInit
                          , render
                          , renderWithSplices
                          , FromAttribute(..)
                          , tag
                          , attr
                          , attrOpt
                          , (&=)
                          ) where

import           Blaze.ByteString.Builder
import           Control.Arrow              (first)
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Text.Read             (decimal, double)
import           Heist
import           Heist.Interpreted
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Util           as W
import qualified Text.XmlHtml               as X

type FnHeistState ctxt = HeistState (ReaderT ctxt IO)
type FnSplice ctxt = Splice (ReaderT ctxt IO)

class HeistContext ctxt where
  heistLens :: Functor f => (FnHeistState ctxt -> f (FnHeistState ctxt)) ->
                            ctxt -> f ctxt
  heistLens f c = setHeist c <$> f (getHeist c)
  getHeist :: ctxt -> FnHeistState ctxt
  getHeist = view heistLens
  setHeist :: ctxt -> FnHeistState ctxt -> ctxt
  setHeist c r = set heistLens r c

heistInit :: HeistContext ctxt =>
             [Text] ->
             Splices (Splice (ReaderT ctxt IO)) ->
             IO (Either [String] (FnHeistState ctxt))
heistInit templateLocations splices =
  do let ts = map (loadTemplates . T.unpack) templateLocations
     runEitherT $ initHeist (emptyHeistConfig & hcTemplateLocations .~ ts
                                    & hcInterpretedSplices .~ splices
                                    & hcLoadTimeSplices .~ defaultLoadTimeSplices
                                    & hcNamespace .~ "")

render :: HeistContext ctxt =>
          ctxt ->
          Text ->
          IO (Maybe Response)
render ctxt name = renderWithSplices ctxt name mempty

renderWithSplices :: HeistContext ctxt =>
                     ctxt ->
                     Text ->
                     Splices (FnSplice ctxt) ->
                     IO (Maybe Response)
renderWithSplices ctxt name splices =
  do r <- runReaderT (renderTemplate (bindSplices splices (ctxt ^. heistLens)) (T.encodeUtf8 name)) ctxt
     case first toLazyByteString <$> r of
       Nothing -> return Nothing
       Just (h,m) -> Just <$> W.bytestring status200 [(hContentType, m)] h


class FromAttribute a where
  fromAttribute :: Text -> Maybe a

instance FromAttribute Text where
  fromAttribute = Just
instance FromAttribute Int where
  fromAttribute t = case decimal t of
                           Left _ -> Nothing
                           Right m | snd m /= "" ->
                                     Nothing
                           Right (v, _) -> Just v
instance FromAttribute Double where
  fromAttribute t = case double t of
                           Left _ -> Nothing
                           Right m | snd m /= "" ->
                                     Nothing
                           Right (v, _) -> Just v

tag :: Text ->
       (X.Node -> k -> Maybe (X.Node, FnSplice ctxt)) ->
       (ctxt -> k) ->
       Splices (FnSplice ctxt)
tag name match handle =
  name ## do ctxt <- lift ask
             node <- getParamNode
             case match node (handle ctxt) of
               Nothing -> do tellSpliceError $
                              "Invalid attributes for splice '" <>
                              name <> "'"
                             return []
               Just (_, a) -> a


(&=) :: (X.Node -> k -> Maybe (X.Node, k')) ->
        (X.Node -> k' -> Maybe (X.Node, a)) ->
        X.Node ->
        k -> Maybe (X.Node, a)
(&=) a1 a2 node k =
  case a1 node k of
    Nothing -> Nothing
    Just (_, k') -> a2 node k'

attr :: FromAttribute a =>
        Text ->
        X.Node ->
        (a -> t) ->
        Maybe (X.Node, t)
attr name node k = case X.getAttribute name node >>= fromAttribute of
                     Nothing -> Nothing
                     Just a -> Just (node, k a)

attrOpt :: FromAttribute a =>
           Text ->
           X.Node ->
           (Maybe a -> t) ->
           Maybe (X.Node, t)
attrOpt name node k =
  Just (node, k (X.getAttribute name node >>= fromAttribute))
