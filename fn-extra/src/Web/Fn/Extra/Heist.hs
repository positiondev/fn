{-# LANGUAGE OverloadedStrings #-}

module Web.Fn.Extra.Heist ( HeistContext(..)
                          , HeistState
                          , heistInit
                          , render
                          , renderWithSplices
                          ) where

import           Blaze.ByteString.Builder
import           Control.Arrow              (first)
import           Control.Lens
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Heist
import           Heist.Interpreted
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Util           as W

class HeistContext ctxt where
  heistLens :: Functor f => (HeistState (ReaderT ctxt IO) -> f (HeistState (ReaderT ctxt IO))) -> ctxt -> f ctxt
  heistLens f c = setHeist c <$> f (getHeist c)
  getHeist :: ctxt -> HeistState (ReaderT ctxt IO)
  getHeist = view heistLens
  setHeist :: ctxt -> HeistState (ReaderT ctxt IO) -> ctxt
  setHeist c r = set heistLens r c

heistInit :: HeistContext ctxt =>
               [Text] ->
               Splices (Splice (ReaderT ctxt IO)) ->
               IO (Either [String] (HeistState (ReaderT ctxt IO)))
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
                     Splices (Splice (ReaderT ctxt IO)) ->
                     IO (Maybe Response)
renderWithSplices ctxt name splices =
  do r <- runReaderT (renderTemplate (bindSplices splices (ctxt ^. heistLens)) (T.encodeUtf8 name)) ctxt
     case first toLazyByteString <$> r of
       Nothing -> return Nothing
       Just (h,m) -> Just <$> W.bytestring status200 [(hContentType, m)] h
