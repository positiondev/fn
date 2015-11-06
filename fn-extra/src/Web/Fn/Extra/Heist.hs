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
                          , heistServe
                          , render
                          , renderWithSplices
                            -- * Building splices
                          , tag
                          , tag'
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
import           Web.Fn

-- | The type of our state. We need a ReaderT to be able to pass the
-- runtime context (which includes the current request) into the
-- splices.
type FnHeistState ctxt = HeistState (ReaderT ctxt IO)

-- | The type of our splice. We need a ReaderT to be able to pass the
-- runtime context (which includes the current request) into the
-- splice.
type FnSplice ctxt = Splice (ReaderT ctxt IO)

-- | In order to have render be able to get the 'FnHeistState' out of
-- our context, we need this helper class.
class HeistContext ctxt where
  getHeist :: ctxt -> FnHeistState ctxt

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

-- | Render templates according to the request path. Note that if you
-- have matched some parts of the path, those will not be included in
-- the path used to find the templates. For example, if you have
-- 'foo/bar.tpl' in the directory where you loaded templates from,
--
-- > path "foo" ==> heistServe
--
-- Will match @foo/foo/bar@, but not @foo/bar@. To match that, you could:
--
-- > anything ==> heistServe
--
-- If no template is found, this will continue routing.
heistServe :: (RequestContext ctxt, HeistContext ctxt) =>
              ctxt ->
              IO (Maybe Response)
heistServe ctxt =
  let p = pathInfo . fst $ getRequest ctxt in
  render ctxt (T.intercalate "/" p)

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
  do r <- runReaderT (renderTemplate (bindSplices splices (getHeist ctxt)) (T.encodeUtf8 name)) ctxt
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

-- | A tag with no attributes.
tag' :: Text ->
        (ctxt -> X.Node -> FnSplice ctxt) ->
        Splices (FnSplice ctxt)
tag' name handle =
  name ## do ctxt <- lift ask
             node <- getParamNode
             handle ctxt node


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
