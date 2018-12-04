<img src="http://i.imgur.com/0h9dFhl.png" width="300px"/>

[![Build Status](https://travis-ci.org/qfpl/tasty-wai.svg?branch=master)](https://travis-ci.org/qfpl/tasty-wai)

# `tasty-wai`

This provides [`tasty`]( https://hackage.haskell.org/package/tasty) integration
for [`wai`]( https://hackage.haskell.org/package/wai) via the components
provided by [`wai-extra`](https://hackage.haskell.org/package/wai-extra).

This is a simple package, it does not provide any resource management for
anything that your `Application` may require. Test databases and the like are
not handled. This package provides a nicer interface to running tests again the
endpoints and interrogating their results.

## An example of usage

There is an example of usage in `test/Test.hs` and it is included here.

Given this trivial `Application`:

```haskell
import           Network.Wai        (Application)
import qualified Network.Wai        as W

import qualified Network.HTTP.Types as H

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
```

We can write some tests to check the endpoints behave as we expect:

```haskell
testWai testApp "Hello to World" $ do
  res <- get "hello"
  assertBody "world!" res

testWai testApp "Echo to thee" $ do
  res <- post "echo" "thus"
  assertStatus' H.status200 res -- Use functions from Network.HTTP.Types
  assertStatus 200 res          -- Use raw ints
  assertBody "thus" res
```

We can check that our fall-through route works as intended:

```haskell
testWai testApp "Will die!" $ do
  res <- get "not-a-thing"
  assertStatus' H.status404 res
  assertBody "no route" res
```

These can be grouped up and run as per the `tasty` `TestTree`:

```haskell
import           Test.Tasty         (defaultMain, testGroup)
import           Test.Tasty.Wai     (assertBody, assertStatus, assertStatus',
                                     get, post, testWai)

main :: IO ()
main = defaultMain $ testGroup "Tasty-Wai Tests"

  [ testWai testApp "Hello to World" $ do
      res <- get "hello"
      assertBody "wrld!" res

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
```

Tasty then provides nicely formatted and grouped output, as you've come to expect:

```
Test suite tests: RUNNING...
Tasty-Wai Tests
  Hello to World: OK
  Echo to thee:   OK
  Will die!:      OK
```

With the errors from `wai-extra` helping us understanding where our tests went wrong:

```
Test suite tests: RUNNING...
Tasty-Wai Tests
  Hello to World: FAIL
    Expected response body "wrld!", but received "world!"
  Echo to thee:   OK
  Will die!:      OK
```
