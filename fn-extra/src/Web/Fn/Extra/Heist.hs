{-# LANGUAGE OverloadedStrings #-}

{-|

This module contains helpers to make Heist fit in more closely within
`Fn`'s stance against monad transformers and for regular functions.

In particular, it instantiates the Monad for HeistState to be a
ReaderT that contains our context, so that in the splices we can get
the context out.

Further, we add splice builders that work similar to our url
routing - splices are declared to have certain attributes of specific
types, and the splice that correspond is a function that takes those
as arguments (and takes the context and the node as well).

-}

module Web.Fn.Extra.Heist ( -- * Types
                            HeistContext(..)
                          , FnHeistState
                          , FnSplice
                            -- * Initializer
                          , heistInit
                            -- * Rendering templates
                          , render
                          , renderWithSplices
                            -- * Building splices
                          , tag
                          , FromAttribute(..)
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

-- | The type of our state. We need a ReaderT to be able to pass the
-- runtime context (which includes the current request) into the
-- splices.
type FnHeistState ctxt = HeistState (ReaderT ctxt IO)

-- | The type of our splice. We need a ReaderT to be able to pass the
-- runtime context (which includes the current request) into the
-- splice.
type FnSplice ctxt = Splice (ReaderT ctxt IO)

-- | In order to have render be able to get the 'FnHeistState' out of
-- our context, we need this helper class. The easiest way to
-- instantiate it is with the 'heistLens', but if you prefer you can
-- use 'getHeist' and 'setHeist' instead (one of these must be
-- provided).
class HeistContext ctxt where
  heistLens :: Functor f => (FnHeistState ctxt -> f (FnHeistState ctxt)) ->
                            ctxt -> f ctxt
  heistLens f c = setHeist c <$> f (getHeist c)
  getHeist :: ctxt -> FnHeistState ctxt
  getHeist = view heistLens
  setHeist :: ctxt -> FnHeistState ctxt -> ctxt
  setHeist c r = set heistLens r c

-- | Initialize heist. This takes a list of paths to template
-- directories and a set of interpreted splices. Currently, we don't
-- have support for compiled splices yet (so you can drop down to just
-- plain Heist if you want them).
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

-- | Render a single template by name.
render :: HeistContext ctxt =>
          ctxt ->
          Text ->
          IO (Maybe Response)
render ctxt name = renderWithSplices ctxt name mempty

-- | Render a template, and add additional interpreted splices before
-- doing so.
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


-- | In order to make splice definitions more functional, we declare
-- them and the attributes they need, along with deserialization (if
-- needed). The deserialization is facilitated be this class.
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

-- | This declares a new splice. Given a name, an attribute matcher,
-- and a handler function (which takes the context, the node, and the
-- specified attributes), it will pass the handler function the
-- provided attributes or return nothing, if the attributes are
-- missing / not deserializable.
--
-- Note that due to the dynamism (the handler function can have any
-- number of arguments, and the number / type of them is based on the
-- matcher), the types of this may be a little confusing (in
-- particular, the `k` contains a lot). This continuation-based style
-- lets us achieve this style, but the types suffer. It may be easier
-- to see via an example:
--
-- @
--  tag "posts" (attr "num" & attr "sort") $ \\ctxt node num sort -> ...
-- @
tag :: Text ->
       (X.Node -> k -> Maybe (X.Node, FnSplice ctxt)) ->
       (ctxt -> X.Node -> k) ->
       Splices (FnSplice ctxt)
tag name match handle =
  name ## do ctxt <- lift ask
             node <- getParamNode
             case match node (handle ctxt node) of
               Nothing -> do tellSpliceError $
                              "Invalid attributes for splice '" <>
                              name <> "'"
                             return []
               Just (_, a) -> a


-- | This combines two matchers together.
(&=) :: (X.Node -> k -> Maybe (X.Node, k')) ->
        (X.Node -> k' -> Maybe (X.Node, a)) ->
        X.Node ->
        k -> Maybe (X.Node, a)
(&=) a1 a2 node k =
  case a1 node k of
    Nothing -> Nothing
    Just (_, k') -> a2 node k'

-- | This specifies that an attribute should be present and
-- convertable to the type indicated by it's type.
attr :: FromAttribute a =>
        Text ->
        X.Node ->
        (a -> t) ->
        Maybe (X.Node, t)
attr name node k = case X.getAttribute name node >>= fromAttribute of
                     Nothing -> Nothing
                     Just a -> Just (node, k a)

-- | This specifies that an attribute is optional - if absent or not
-- convertable, 'Nothing' will be passed.
attrOpt :: FromAttribute a =>
           Text ->
           X.Node ->
           (Maybe a -> t) ->
           Maybe (X.Node, t)
attrOpt name node k =
  Just (node, k (X.getAttribute name node >>= fromAttribute))
