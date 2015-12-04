{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           Test.Hspec.Wai

import           Site

main :: IO ()
main = do
  (a, shutdown) <- app
  hspec $ afterAll_ shutdown $ with (return a) $
    do describe "GET /" $
         it "responds with 200" $
           get "/" `shouldRespondWith` 200
       -- NOTE(dbp 2015-10-25): hspec-wai can't pass query params yet...
       --
       -- describe "GET /param?id=10" $
       --   it "responds with 10" $
       --     get "/param?id=10" `shouldRespondWith` "10"
       describe "GET /segment/foo" $
         it "responds with foo" $
           get "/segment/foo" `shouldRespondWith` "foo"
       describe "GET /segment/foo/" $
         it "also responds with foo (trailing slash doesn't matter)" $
           get "/segment/foo/" `shouldRespondWith` "foo"
       describe "GET /segment//foo" $
         it "STILL responds with foo (extra slash doesn't matter)" $
           get "/segment//foo" `shouldRespondWith` "foo"
       describe "GET /random/path" $
         it "should 404" $
           get "/random/path" `shouldRespondWith` 404
       describe "GET /template" $ do
         it "should return html" $
           get "/template" `shouldRespondWith` 200 { matchHeaders = ["Content-Type" <:> "text/html;charset=utf-8"]}
         it "should have /template in the response" $
           get "/template" `shouldRespondWith` "<html><body>\n\n  <p>x:/templatex:/template</p>\n\n  <p>hello</p>\n\n</body></html>\n"
       describe "GET /session" $
         it "should respond 0 for first visit" $
           get "/session" `shouldRespondWith` "0"
         -- NOTE(dbp 2015-10-25): hspec-wai doesn't have support for sessions
         -- it "should respond 1 for second visit" $
         --   get "/session" `shouldRespondWith` "1"
       describe "POST /param with id=100 in body" $
         it "should respond with 100" $
           postHtmlForm "/param" [("id", "100")] `shouldRespondWith` "100"
       describe "POST /param_many with id=100&id=200" $
         it "should respond with [100,200]" $
           postHtmlForm "/param_many" [("id", "100")
                                      ,("id", "200")] `shouldRespondWith` "[100,200]"
