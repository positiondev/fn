{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Maybe
import           Data.Text          (Text)
import           Network.HTTP.Types
import           Network.Wai
import           Test.Hspec
import           Web.Fn

newtype Req = Req ([Text], Query)
instance RequestContext Req where
  getRequest (Req (p,q)) = defaultRequest { pathInfo = p, queryString = q }
  setRequest (Req _) r = Req (pathInfo r, queryString r)

main :: IO ()
main = hspec $
  describe "routing" $ do
    it "should match first segment with path" $
      do path "foo" (["foo", "bar"], []) () `shouldSatisfy` isJust
         path "foo" ([], []) () `shouldSatisfy` isNothing
         path "foo" (["bar", "foo"], []) () `shouldSatisfy` isNothing
    it "should match two paths combined with //" $
      do (path "a" // path "b") (["a", "b"], []) () `shouldSatisfy` isJust
         (path "b" // path "a") (["a", "b"], []) () `shouldSatisfy` isNothing
         (path "b" // path "a") (["b"], []) () `shouldSatisfy` isNothing
    it "should pass url segment to segment" $
      do segment (["a"], []) (== "a") `shouldSatisfy` (snd . fromJust)
         segment ([], []) id `shouldSatisfy` isNothing
         segment (["a", "b"], []) (== "a") `shouldSatisfy` (snd . fromJust)
    it "should match two segments combined with //" $
      do (segment // segment) (["a", "b"],[]) (\a b -> a == "a" && b == "b")
                              `shouldSatisfy` (snd . fromJust)
         (segment // segment) ([], []) (\_ _ -> ()) `shouldSatisfy` isNothing
         (segment // segment) (["a", "b", "c"], [])
                              (\a b -> a == "a" && b == "b")
                              `shouldSatisfy` (snd . fromJust)
    it "should match path and segment combined with //" $
      do (path "a" // segment) (["a", "b"], []) (== "b")
                               `shouldSatisfy` (snd . fromJust)
         (path "a" // segment) (["b", "b"], []) (== "b")
                               `shouldSatisfy` isNothing
         (segment // path "b") (["a", "b"], []) (== "a")
                               `shouldSatisfy` (snd . fromJust)
    it "should match many segments and paths together" $
       do (path "a" // segment // path "c" // path "d")
            (["a","b","c", "d"], [])
            (== "b")
            `shouldSatisfy` (snd . fromJust)
          (segment // path "b" // segment // segment)
            (["a","b","c", "d", "e"], [])
            (\a c d -> a == "a" && c == "c" && d == "d")
            `shouldSatisfy` (snd . fromJust)
          (segment // path "b" // segment)
            (["a", "b"], []) (\_ _ -> True)
            `shouldSatisfy` isNothing
          (segment // path "a" // segment)
            (["a", "b"], []) (\_ _ -> True)
            `shouldSatisfy` isNothing
    it "should match query parameters with param" $
      do param "foo" ([], [("foo", Nothing)]) (== "")
                     `shouldSatisfy` (snd . fromJust)
         param "foo" ([], []) (const True) `shouldSatisfy` isNothing
    it "should match combined param and paths with /?" $
      do (path "a" /? param "id") (["a"], [("id", Just "x")])
                                  (== "x")
                                  `shouldSatisfy` (snd . fromJust)
         (path "a" /? param "id") (["b"], [("id", Just "x")])
                                  (== "x")
                                  `shouldSatisfy` isNothing
         (path "a" /? param "id") ([], [("id", Just "x")])
                         (== "x")
                         `shouldSatisfy` isNothing
         (path "a" /? param "id") (["a"], [("di", Just "x")])
                         (== "x")
                         `shouldSatisfy` isNothing
    it "should match combining param, path, segment" $
      do (path "a" // segment /? param "id")
           (["a", "b"], [("id", Just "x")])
           (\b x -> b == "b" && x == "x")
           `shouldSatisfy` (snd . fromJust)
         (path "a" // segment // segment /? param "id")
           (["a", "b"], [("id", Just "x")])
           (\_ _ _ -> True)
           `shouldSatisfy` isNothing
    it "should apply matchers with ==>" $
      do (path "a" ==> const ())
           (Req (["a"], []))
           `shouldSatisfy` isJust
         (segment ==> \_ _ -> ())
            (Req (["a"], []))
            `shouldSatisfy` isJust
         (segment // path "b" ==> \_ x -> x == "a")
           (Req (["a", "b"], []))
           `shouldSatisfy` fromJust
         (segment // path "b" ==> \_ x -> x == "a")
           (Req (["a", "a"], []))
           `shouldSatisfy` isNothing
         (segment // path "b" ==> \_ x -> x == "a")
           (Req (["a"], []))
           `shouldSatisfy` isNothing
