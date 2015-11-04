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
  getRequest (R (p',q')) = defaultRequest { pathInfo = p', queryString = q' }
  setRequest (R _) r = R (pathInfo r, queryString r)
p :: [Text] -> Req
p x = (x,[],GET)
_p :: [Text] -> Req ->  Req
_p x (_,q',m') = (x,q',m')
q :: Query -> Req
q x = ([],x,GET)
_q :: Query -> Req -> Req
_q x (p',_,m') = (p',x,m')
m :: StdMethod -> Req
m x = ([],[],x)
_m :: StdMethod -> Req -> Req
_m x (p',q',_) = (p',q',x)

j :: Show a => Maybe (a,b) -> Expectation
j mv = fst <$> mv `shouldSatisfy` isJust
n :: Show a => Maybe (a,b) -> Expectation
n mv = fst <$> mv `shouldSatisfy` isNothing

v :: Maybe (a, t -> Bool) -> t -> Expectation
v mv f = snd (fromJust mv) f `shouldBe` True
vn :: Maybe (a, t -> Bool) -> t -> Expectation
vn mv f = case mv of
            Nothing -> (1 :: Int) `shouldBe` 1
            Just (_,k) -> k f `shouldBe` False

t1 :: Text -> Text -> Bool
t1 = (==)
t2 :: Text -> Text -> Text -> Text -> Bool
t2 a b a' b' = a == a' && b == b'
t3 :: Text -> Text -> Text -> Text -> Text -> Text -> Bool
t3 a b c a' b' c' = a == a' && b == b' && c == c'

t1u :: Text -> Bool
t1u _ = undefined
t2u :: Text -> Text -> Bool
t2u _ _ = undefined
t3u :: Text -> Text -> Text -> Bool
t3u _ _ _ = undefined

main :: IO ()
main = hspec $ do

  describe "matching" $ do
    it "should match first segment with path" $
      do j (path "foo" (p ["foo", "bar"]))
         n (path "foo" (p []))
         n (path "foo" (p ["bar", "foo"]))
    it "should match two paths combined with //" $
      do j ((path "a" // path "b") (p ["a", "b"]))
         n ((path "b" // path "a") (p ["a", "b"]))
         n ((path "b" // path "a") (p ["b"]))
    it "should pass url segment to segment" $
      do v (segment (p ["a"])) (t1 "a")
         vn (segment (p [])) t1u
         v (segment (p ["a", "b"])) (t1 "a")
    it "should match two segments combined with //" $
      do v ((segment // segment) (p ["a", "b"])) (t2 "a" "b")
         vn ((segment // segment) (p [])) t2u
         v ((segment // segment) (p ["a", "b", "c"])) (t2 "a" "b")
    it "should match path and segment combined with //" $
      do v ((path "a" // segment) (p ["a", "b"])) (t1 "b")
         vn ((path "a" // segment) (p ["b", "b"])) t1u
         v ((segment // path "b") (p ["a", "b"])) (t1 "a")
    it "should match many segments and paths together" $
       do v ((path "a" // segment // path "c" // path "d")
             (p ["a","b","c", "d"])) (t1 "b")
          v ((segment // path "b" // segment // segment)
             (p ["a","b","c", "d", "e"])) (t3 "a" "c" "d")
          vn ((segment // path "b" // segment) (p ["a", "b"])) t2u
          vn ((segment // path "a" // segment) (p ["a", "b"])) t2u
    it "should match query parameters with param" $
      do v (param "foo" (q [("foo", Nothing)])) (t1 "")
         vn (param "foo" (q [])) t1u
    it "should match combined param and paths with /?" $
      do v ((path "a" /? param "id") (_p ["a"] $ q [("id", Just "x")])) (t1 "x")
         vn ((path "a" /? param "id") (_p ["b"] $ q [("id", Just "x")])) t1u
         vn ((path "a" /? param "id") (_p [] $ q [("id", Just "x")])) t1u
         vn ((path "a" /? param "id") (_p ["a"] $ q [("di", Just "x")])) t1u
    it "should match combining param, path, segment" $
      do v ((path "a" // segment /? param "id")
             (_p ["a", "b"] $ q [("id", Just "x")])) (t2 "b" "x")
         vn ((path "a" // segment // segment /? param "id")
               (_p ["a", "b"] $ q [("id", Just "x")])) t3u
    it "should apply matchers with ==>" $
      do (path "a" ==> const ())
           (R (["a"], []))
           `shouldSatisfy` isJust
         (segment ==> \_ (_ :: Text) -> ())
            (R (["a"], []))
            `shouldSatisfy` isJust
         (segment // path "b" ==> \_ x -> x == ("a" :: Text))
           (R (["a", "b"], []))
           `shouldSatisfy` fromJust
         (segment // path "b" ==> \_ x -> x == ("a" :: Text))
           (R (["a", "a"], []))
           `shouldSatisfy` isNothing
         (segment // path "b" ==> \_ x -> x == ("a" :: Text))
           (R (["a"], []))
           `shouldSatisfy` isNothing
    it "should always pass a value with paramOpt" $
      do snd (fromJust (paramOpt "id" (q [])))
             (isLeft :: Either ParamError [Text] -> Bool)
             `shouldBe` True
         snd (fromJust (paramOpt "id" (q [("id", Just "foo")])))
                            (== Right (["foo"] :: [Text]))
                            `shouldBe` True
    it "should match end against no further path segments" $
      do j (end (p []))
         j (end (_p [] $ q [("foo", Nothing)]))
         n (end (p ["a"]))
    it "should match end after path and segments" $
      do j ((path "a" // end) (p ["a"]))
         v ((segment // end) (p ["a"])) (t1 "a")
    it "should match anything" $
      do j (anything (p []))
         j (anything (p ["f","b"]))

    it "should match against method" $
       do j ((method GET) (m GET))
          n ((method GET) (m POST))

  describe "route" $ do
    it "should match route to parameter" $
      do r <- route (R (["a"], [])) [segment ==> (\_ a -> if a == ("a"::Text) then okText "" else return Nothing)]
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
