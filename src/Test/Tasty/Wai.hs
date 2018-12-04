module Test.Tasty.Wai
  (
    -- * Types
    Sess (..)
  , RequestPath (..)

    -- * Creation
  , testWai

    -- * Helpers
  , get
  , post
  , put
  , assertStatus'

    -- * Request Builders
  , buildRequest
  , buildRequestWithBody

  , module Network.Wai.Test
  ) where

import qualified Control.Exception    as E

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

import           Network.HTTP.Types   (StdMethod)
import qualified Network.HTTP.Types   as HTTP

import           Test.Tasty.Providers (IsTest (..), Progress (..), TestName,
                                       TestTree, singleTest, testFailed,
                                       testPassed)
import           Test.Tasty.Runners   (formatMessage)

import           Network.Wai          (Application, Request, requestMethod)
import           Network.Wai.Test

data Sess = S Application TestName (Session ())

-- Although not exported, this newtype helps us keep our strings in line.
newtype RequestPath = RequestPath
  { unRequestPath :: BS.ByteString
  }

instance IsTest Sess where
  -- No options yet
  testOptions = mempty

  run _ (S app tName sess) yieldProgress = do

    -- We don't really have progress to report, so state that we're running a
    -- test but do nothing else.
    yieldProgress $ Progress ("Running " <> tName) 0

    -- The wai-extra testing uses `throwIO` to indicate a test failure and
    -- converts that error into a 'String'. The result of the individual
    -- 'Session a' isn't important for the test?
    E.try (runSession sess app) >>= either toFailure toPass
    where
      toFailure (WaiTestFailure s) = testFailed <$> (formatMessage s)
      toPass     _                 = pure (testPassed mempty)

-- | Create an empty 'Request' using the given HTTP Method and route.
buildRequest
  :: StdMethod
  -> RequestPath
  -> Request
buildRequest mth rpath = flip setPath (unRequestPath rpath) $ defaultRequest
  { requestMethod = HTTP.renderStdMethod mth
  }

-- | As per 'buildRequest' but requires body content.
buildRequestWithBody
  :: StdMethod
  -> RequestPath
  -> LBS.ByteString
  -> SRequest
buildRequestWithBody mth rpath =
  SRequest (buildRequest mth rpath)

-- | Run a test case against a 'Application'.
--
-- This module re-exports the functions from 'wai-extra' for constructing the
-- 'Session' that is executed against a given endpoint.
--
-- A small test case may look like:
--
-- @
-- import MyApp (app)
--
-- testWai app "List Topics" $ do
--       res <- get "fudge/view"
--       assertStatus' HTTP.status200 res
-- @
--
testWai :: Application -> TestName -> Session () -> TestTree
testWai a tn = singleTest tn . S a tn

get :: BS.ByteString -> Session SResponse
get = request . buildRequest HTTP.GET . RequestPath

post :: BS.ByteString -> LBS.ByteString -> Session SResponse
post r = srequest . buildRequestWithBody HTTP.POST (RequestPath r)

put :: BS.ByteString -> LBS.ByteString -> Session SResponse
put r = srequest . buildRequestWithBody HTTP.PUT (RequestPath r)

assertStatus' :: HTTP.Status -> SResponse -> Session ()
assertStatus' c = assertStatus (HTTP.statusCode c)
