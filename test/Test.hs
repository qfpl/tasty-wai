{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.Wai        (Application)
import qualified Network.Wai        as W

import qualified Network.HTTP.Types as H

import           Test.Tasty         (defaultMain, testGroup)
import           Test.Tasty.Wai     (assertBody, assertStatus, assertStatus',
                                     get, post, testWai)

testApp :: Application
testApp rq cb = do
  let
    mkresp s = W.responseLBS s []
    resp404 = mkresp H.status404
    resp200 = mkresp H.status200

  resp <- case (W.requestMethod rq, W.pathInfo rq) of

    -- Ye olde...
    ("GET", ["hello"]) -> pure $ resp200 "world!"

    -- Echo me this!
    ("POST", ["echo"]) -> resp200 <$> W.strictRequestBody rq

    -- Well, then...
    _ -> pure $ resp404 "no route"

  cb resp

main :: IO ()
main = defaultMain $ testGroup "Tasty-Wai Tests"

  [ testWai testApp "Hello to World" $ do
      res <- get "hello"
      assertBody "world!" res

  , testWai testApp "Echo to thee" $ do
      res <- post "echo" "thus"
      assertStatus' H.status200 res -- Use functions from Network.HTTP.Types
      assertStatus 200 res          -- Use raw ints
      assertBody "thus" res

  , testWai testApp "Will die!" $ do
      res <- get "not-a-thing"
      assertStatus' H.status404 res
      assertBody "no route" res
  ]
