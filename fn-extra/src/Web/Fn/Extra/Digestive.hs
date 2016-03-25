{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Web.Fn.Extra.Digestive (runForm) where

import           Control.Applicative          ((<$>))
import           Control.Arrow                (second)
import           Control.Concurrent.MVar      (readMVar)
import           Control.Monad.Trans          (liftIO)
import           Control.Monad.Trans.Resource
import           Data.ByteString              (ByteString)
import           Data.Maybe                   (fromMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Network.HTTP.Types.Method
import           Network.Wai                  (Request (..))
import           Network.Wai.Parse            (BackEnd, File, FileInfo (..),
                                               fileContent, parseRequestBody,
                                               tempFileBackEndOpts)
import           System.Directory             (getTemporaryDirectory)
import           Text.Digestive
import           Text.Digestive.Types
import           Text.Digestive.View
import           Web.Fn                       hiding (File, fileContent)

queryFormEnv :: [(ByteString, Maybe ByteString)] -> [File FilePath] -> Env IO
queryFormEnv qs fs = \pth ->
  let qs' = map TextInput $ map (T.decodeUtf8 . fromMaybe "" . snd) $ filter ((==) (fromPath pth) . T.decodeUtf8 . fst) qs
      fs' = map FileInput $ map (fileContent . snd) $ filter ((==) (fromPath pth) . T.decodeUtf8 . fst) fs
  in return $ qs' ++ fs'

requestFormEnv :: FnRequest -> ResourceT IO (Env IO)
requestFormEnv req = do
  st <- getInternalState
  v <- case snd req of
         Nothing -> return Nothing
         Just mv -> liftIO (readMVar mv)
  (query, files) <-
     case v of
       Nothing -> liftIO $ parseRequestBody (tempFileBackEnd' st)
                                            (fst req)
       Just (q,_) -> return q
  return $ queryFormEnv ((map (second Just) query) ++ queryString (fst req)) files

-- | This function runs a form and passes the function in it's last
-- argument the result, which is a 'View' and an optional result. If
-- the request is a get, or if the form failed to validate, the result
-- will be 'Nothing' and you should render the form (with the errors
-- from the 'View').
runForm :: RequestContext ctxt =>
       ctxt
    -> Text
    -> Form v IO a
    -> ((View v, Maybe a) -> IO a1)
    -> IO a1
runForm ctxt nm frm k =
  runResourceT $ let r = fst (getRequest ctxt) in
    if requestMethod r == methodPost
    then do env <- requestFormEnv (getRequest ctxt)
            r <- liftIO $ postForm nm frm (const (return env))
            liftIO $ k r
    else do r <- (,Nothing) <$> liftIO (getForm nm frm)
            liftIO $ k r
