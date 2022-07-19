-- | Types and functions for testing 'wai' endpoints using the 'tasty' testing framework.
--
module Test.Tasty.Wai
  (
    -- * Types
    Sess (..)

    -- * Creation
  , testWai

    -- * Helpers
  , get
  , head
  , post
  , put
  , assertStatus'

    -- * Request Builders
  , buildRequest
  , buildRequestWithBody
  , buildRequestWithHeaders

  , module Network.Wai.Test
  ) where

import qualified Control.Exception    as E
import           Prelude              hiding (head)

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Monoid          ((<>))

import           Network.HTTP.Types   (RequestHeaders, StdMethod)
import qualified Network.HTTP.Types   as HTTP

import           Test.HUnit.Lang      (HUnitFailure (HUnitFailure), formatFailureReason)

import           Test.Tasty.Providers (IsTest (..), Progress (..), TestName,
                                       TestTree, singleTest, testFailed,
                                       testPassed)
import           Test.Tasty.Runners   (formatMessage)

import           Network.Wai          (Application, Request, requestHeaders,
                                       requestMethod)
import           Network.Wai.Test

-- | Data structure for carrying around the info needed to build and run a test.
data Sess = S Application TestName (Session ())

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
      toFailure (HUnitFailure _ s) = testFailed <$> (formatMessage (formatFailureReason s))
      toPass     _                 = pure (testPassed mempty)

-- | Create an empty 'Request' using the given HTTP Method and route.
buildRequest
  :: StdMethod
  -> BS.ByteString
  -> Request
buildRequest mth rpath = flip setPath rpath $ defaultRequest
  { requestMethod = HTTP.renderStdMethod mth
  }

-- | As per 'buildRequest' but requires body content.
buildRequestWithBody
  :: StdMethod
  -> BS.ByteString
  -> LBS.ByteString
  -> SRequest
buildRequestWithBody mth rpath =
  SRequest (buildRequest mth rpath)

-- | As per 'buildRequestWithBody' but allows for the setting of 'RequestHeaders'.
buildRequestWithHeaders
  :: StdMethod
  -> BS.ByteString
  -> LBS.ByteString
  -> RequestHeaders
  -> SRequest
buildRequestWithHeaders mthd pth bdy hdrs =
  rq { simpleRequest = (simpleRequest rq) { requestHeaders = hdrs } }
  where rq = buildRequestWithBody mthd pth bdy

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

-- | Submit a 'HTTP.HEAD' request to the provided endpoint.
head :: BS.ByteString -> Session SResponse
head = request . buildRequest HTTP.HEAD

-- | Submit a 'HTTP.GET' request to the provided endpoint.
get :: BS.ByteString -> Session SResponse
get = request . buildRequest HTTP.GET

-- | Submit a 'HTTP.POST' request to the given endpoint with the provided
-- 'LBS.ByteString' as the body content.
post :: BS.ByteString -> LBS.ByteString -> Session SResponse
post r = srequest . buildRequestWithBody HTTP.POST r

-- | Submit a 'HTTP.PUT' request to the given endpoint with the provided
-- 'LBS.ByteString' as the body content.
put :: BS.ByteString -> LBS.ByteString -> Session SResponse
put r = srequest . buildRequestWithBody HTTP.PUT r

-- | An alternative helper function for checking the status code on a response
-- that lets you use the functions from 'Network.HTTP.Types' as opposed to bare
-- numbers.
assertStatus' :: HTTP.Status -> SResponse -> Session ()
assertStatus' c = assertStatus (HTTP.statusCode c)
