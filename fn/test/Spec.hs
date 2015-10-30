{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Either
import           Data.Maybe
import           Data.Text          (Text)
import           Network.HTTP.Types
import           Network.Wai
import           Test.Hspec
import           Web.Fn

newtype R = R ([Text], Query)
instance RequestContext R where
  getRequest (R (p,q)) = defaultRequest { pathInfo = p, queryString = q }
  setRequest (R _) r = R (pathInfo r, queryString r)

main :: IO ()
main = hspec $ do

  describe "matching" $ do
    it "should match first segment with path" $
      do path "foo" (["foo", "bar"], []) () `shouldSatisfy` isJust
         path "foo" ([], []) () `shouldSatisfy` isNothing
         path "foo" (["bar", "foo"], []) () `shouldSatisfy` isNothing
    it "should match two paths combined with //" $
      do (path "a" // path "b") (["a", "b"], []) () `shouldSatisfy` isJust
         (path "b" // path "a") (["a", "b"], []) () `shouldSatisfy` isNothing
         (path "b" // path "a") (["b"], []) () `shouldSatisfy` isNothing
    it "should pass url segment to segment" $
      do segment (["a"], []) (== ("a" :: Text))
                 `shouldSatisfy` (snd . fromJust)
         segment ([], []) (id :: Text -> Text) `shouldSatisfy` isNothing
         segment (["a", "b"], []) (== ("a" :: Text))
                 `shouldSatisfy` (snd . fromJust)
    it "should match two segments combined with //" $
      do (segment // segment) (["a", "b"],[]) (\a b -> a == ("a" :: Text) &&
                                                       b == ("b" :: Text))
                              `shouldSatisfy` (snd . fromJust)
         (segment // segment) ([], []) (\(_ :: Text) (_ :: Text) -> ())
                              `shouldSatisfy` isNothing
         (segment // segment) (["a", "b", "c"], [])
                              (\a b -> a == ("a" :: Text) &&
                                       b == ("b" :: Text))
                              `shouldSatisfy` (snd . fromJust)
    it "should match path and segment combined with //" $
      do (path "a" // segment) (["a", "b"], []) (== ("b" :: Text))
                               `shouldSatisfy` (snd . fromJust)
         (path "a" // segment) (["b", "b"], []) (== ("b" :: Text))
                               `shouldSatisfy` isNothing
         (segment // path "b") (["a", "b"], []) (== ("a" :: Text))
                               `shouldSatisfy` (snd . fromJust)
    it "should match many segments and paths together" $
       do (path "a" // segment // path "c" // path "d")
            (["a","b","c", "d"], [])
            (== ("b" :: Text))
            `shouldSatisfy` (snd . fromJust)
          (segment // path "b" // segment // segment)
            (["a","b","c", "d", "e"], [])
            (\a c d -> a == ("a" :: Text) &&
                       c == ("c" :: Text) &&
                       d == ("d" :: Text))
            `shouldSatisfy` (snd . fromJust)
          (segment // path "b" // segment)
            (["a", "b"], []) (\(_ :: Text) (_ :: Text) -> True)
            `shouldSatisfy` isNothing
          (segment // path "a" // segment)
            (["a", "b"], []) (\(_ :: Text) (_ :: Text) -> True)
            `shouldSatisfy` isNothing
    it "should match query parameters with param" $
      do param "foo" ([], [("foo", Nothing)]) (== ("" :: Text))
                     `shouldSatisfy` (snd . fromJust)
         param "foo" ([], []) (\(_ :: Text) -> True) `shouldSatisfy` isNothing
    it "should match combined param and paths with /?" $
      do (path "a" /? param "id") (["a"], [("id", Just "x")])
                                  (== ("x" :: Text))
                                  `shouldSatisfy` (snd . fromJust)
         (path "a" /? param "id") (["b"], [("id", Just "x")])
                                  (== ("x" :: Text))
                                  `shouldSatisfy` isNothing
         (path "a" /? param "id") ([], [("id", Just "x")])
                         (== ("x" :: Text))
                         `shouldSatisfy` isNothing
         (path "a" /? param "id") (["a"], [("di", Just "x")])
                         (== ("x" :: Text))
                         `shouldSatisfy` isNothing
    it "should match combining param, path, segment" $
      do (path "a" // segment /? param "id")
           (["a", "b"], [("id", Just "x")])
           (\b x -> b == ("b" :: Text) && x == ("x" :: Text))
           `shouldSatisfy` (snd . fromJust)
         (path "a" // segment // segment /? param "id")
           (["a", "b"], [("id", Just "x")])
           (\(_ :: Text) (_ :: Text) (_ :: Text) -> True)
           `shouldSatisfy` isNothing
    it "should apply matchers with ==>" $
      do (path "a" ==> const ())
           (R (["a"], []))
           `shouldSatisfy` isJust
         (segment ==> \(_ :: Text) _ -> ())
            (R (["a"], []))
            `shouldSatisfy` isJust
         (segment // path "b" ==> \x _ -> x == ("a" :: Text))
           (R (["a", "b"], []))
           `shouldSatisfy` fromJust
         (segment // path "b" ==> \x _ -> x == ("a" :: Text))
           (R (["a", "a"], []))
           `shouldSatisfy` isNothing
         (segment // path "b" ==> \x _ -> x == ("a" :: Text))
           (R (["a"], []))
           `shouldSatisfy` isNothing
    it "should always pass a value with paramOpt" $
      do paramOpt "id" ([], []) (isLeft :: Either ParamError [Text] -> Bool)
                  `shouldSatisfy` (snd . fromJust)
         paramOpt "id" ([], [("id", Just "foo")])
                       (== Right (["foo"] :: [Text]))
                       `shouldSatisfy` (snd . fromJust)
    it "should match end against no further path segments" $
      do end ([],[]) () `shouldSatisfy` isJust
         end ([],[("foo", Nothing)]) () `shouldSatisfy` isJust
         end (["a"],[]) () `shouldSatisfy` isNothing
    it "should match end after path and segments" $
      do (path "a" // end) (["a"],[]) () `shouldSatisfy` isJust
         (segment // end) (["a"],[]) (== ("a" :: Text))
                                     `shouldSatisfy` isJust
    it "should match anything" $
      anything ([],[]) () `shouldSatisfy` isJust

  describe "route" $ do
    it "should match route to parameter" $
      do r <- route (R (["a"], [])) [segment ==> (\a _ -> if a == ("a"::Text) then okText "" else return Nothing)]
         (responseStatus <$> r) `shouldSatisfy` isJust
    it "should match nested routes" $
      do r <- route (R (["a", "b"], [])) [path "a" ==> (\c -> route c [path "b" ==> const (okText "")])]
         (responseStatus <$> r) `shouldSatisfy` isJust

  describe "parameter parsing" $
    do it "should parse Text" $
         fromParam "hello" `shouldBe` Right ("hello" :: Text)
       it "should parse Int" $
         do fromParam "1" `shouldBe` Right (1 :: Int)
            fromParam "2011" `shouldBe` Right (2011 :: Int)
            fromParam "aaa" `shouldSatisfy`
              (isLeft :: Either ParamError Int -> Bool)
            fromParam "10a" `shouldSatisfy`
              (isLeft :: Either ParamError Int -> Bool)
       it "should be able to parse Double" $
         do fromParam "1" `shouldBe` Right (1 :: Double)
            fromParam "1.02" `shouldBe` Right (1.02 :: Double)
            fromParam "thr" `shouldSatisfy`
              (isLeft :: Either ParamError Double -> Bool)
            fromParam "100o" `shouldSatisfy`
              (isLeft :: Either ParamError Double -> Bool)
