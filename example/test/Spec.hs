{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.Hspec.Wai
import           Web.Fn

import           Site

main :: IO ()
main = do
  ctxt <- initializer
  hspec $ with (return $ toWAI ctxt app) $
    do describe "GET /" $
         it "responds with 200" $
           get "/" `shouldRespondWith` 200
       -- NOTE(dbp 2015-10-25): hspec-wai can't pass query params yet...
       --
       -- describe "GET /param?id=foo" $
       --   it "responds with foo" $
       --     get "/param?id=foo" `shouldRespondWith` "foo"
       describe "GET /segment/foo" $
         it "responds with foo" $
           get "/segment/foo" `shouldRespondWith` "foo"
       describe "GET /random/path" $
         it "should 404" $
           get "/random/path" `shouldRespondWith` 404
       describe "GET /template" $
         it "should return html" $
           get "/template" `shouldRespondWith` 200 { matchHeaders = ["Content-Type" <:> "text/html;charset=utf-8"]}
