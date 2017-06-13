{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{-|

This module contains helpers to make Heist fit in more closely within
`Fn`'s stance against monad transformers and for regular functions.

In particular, it instantiates the Monad for HeistState to be a
StateT that contains our context, so that in the splices we can get
the context out (and modify it if needed).

Further, we add splice builders that work similar to our url
routing - splices are declared to have certain attributes of specific
types, and the splice that correspond is a function that takes those
as arguments (and takes the context and the node as well).

-}

module Web.Fn.Extra.Heist ( -- * Types
                            HeistContext(..)
                          , FnHeistState
                          , FnSplice
                          , FnCSplice
                            -- * Initializer
                          , heistInit
                            -- * Rendering templates
                          , heistServe
                          , render
                          , renderWithSplices
                          , cHeistServe
                          , cRender
                            -- * Building splices
                          , tag
                          , tag'
                          , FromAttribute(..)
                          , attr
                          , attrOpt
                          , (&=)
                          ) where

import           Blaze.ByteString.Builder
import           Control.Applicative        ((<$>), (<*>))
import           Control.Arrow              (first)
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Either
#if MIN_VERSION_heist(1,0,0)
import           Data.Map.Syntax ((##))
#endif
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Text.Read             (decimal, double)
import           Heist
import qualified Heist.Compiled             as C
import qualified Heist.Interpreted          as I
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Util           as W
import qualified Text.XmlHtml               as X
import           Web.Fn

-- | The type of our state. We need a StateT to be able to pass the
-- runtime context (which includes the current request) into the
-- splices.
type FnHeistState ctxt = HeistState (StateT ctxt IO)

-- | The type of our splice (interpreted version). We need a StateT to
-- be able to pass the runtime context (which includes the current
-- request) into the splice (and sometimes modify it).
type FnSplice ctxt = I.Splice (StateT ctxt IO)

-- | The type of our splice (compiled version). We need a StateT to
-- be able to pass the runtime context (which includes the current
-- request) into the splice (and sometimes modify it).
type FnCSplice ctxt = C.Splice (StateT ctxt IO)


-- | In order to have render be able to get the 'FnHeistState' out of
-- our context, we need this helper class.
class HeistContext ctxt where
  getHeist :: ctxt -> FnHeistState ctxt

-- | Initialize heist. This takes a list of paths to template
-- directories, a set of interpreted splices, and a set of compiled
-- splices (you can pass @mempty@ as either)
heistInit :: HeistContext ctxt =>
             [Text] ->
             Splices (FnSplice ctxt) ->
             Splices (FnCSplice ctxt) ->
             IO (Either [String] (FnHeistState ctxt))
heistInit templateLocations isplices csplices =
  do let ts = map (loadTemplates . T.unpack) templateLocations
     let config = emptyHeistConfig & hcTemplateLocations .~ ts
                                   & hcInterpretedSplices .~ isplices
                                   & hcLoadTimeSplices .~ defaultLoadTimeSplices
                                   & hcCompiledSplices .~ csplices
                                   & hcNamespace .~ ""
     #if MIN_VERSION_heist(1,0,0)
     initHeist config
     #else
     runEitherT $ initHeist config
     #endif

-- | Render interpreted templates according to the request path. Note
-- that if you have matched some parts of the path, those will not be
-- included in the path used to find the templates. For example, if
-- you have @foo\/bar.tpl@ in the directory where you loaded templates
-- from,
--
-- > path "foo" ==> heistServe
--
-- Will match @foo\/foo\/bar@, but not @foo\/bar@. To match that, you could:
--
-- > anything ==> heistServe
--
-- This will also try the path followed by "index" if the first
-- doesn't match, so if you have @foo\/index.tpl@, the path @foo@ will
-- be matched to it.
--
-- If no template is found, this will continue routing.
heistServe :: (RequestContext ctxt, HeistContext ctxt) =>
              ctxt ->
              IO (Maybe Response)
heistServe ctxt =
  let p = pathInfo . fst $ getRequest ctxt in
  mplus <$> render ctxt (T.intercalate "/" p)
        <*> render ctxt (T.intercalate "/" (p ++ ["index"]))

-- | Render a single interpreted heist template by name.
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
  do (r,_) <- runStateT (I.renderTemplate (I.bindSplices splices (getHeist ctxt)) (T.encodeUtf8 name)) ctxt
     case first toLazyByteString <$> r of
       Nothing -> return Nothing
       Just (h,m) -> Just <$> W.bytestring status200 [(hContentType, m)] h

-- | Render a single compiled heist template by name.
cRender :: HeistContext ctxt => ctxt -> Text -> IO (Maybe Response)
cRender ctxt tmpl =
  let mr = C.renderTemplate (getHeist ctxt) (T.encodeUtf8 tmpl) in
  case mr of
    Nothing -> return Nothing
    Just (rc, ct) ->
      do (builder, _) <- runStateT rc ctxt
         return $ Just $ responseBuilder status200 [(hContentType, ct)] builder


-- | Like 'heistServe', but for compiled templates.
cHeistServe :: (RequestContext ctxt, HeistContext ctxt) =>
               ctxt ->
               IO (Maybe Response)
cHeistServe ctxt =
  do let p = pathInfo . fst $ getRequest ctxt
     mplus <$> cRender ctxt (T.intercalate "/" p)
           <*> cRender ctxt (T.intercalate "/" (p ++ ["index"]))


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
  name ## do ctxt <- lift get
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
  name ## do ctxt <- lift get
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
