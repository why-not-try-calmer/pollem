{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Server
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = businessLogicSpec

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp =
  -- testWithApplication makes sure the action is executed after the server has
  -- started and is being properly shutdown.
  Warp.testWithApplication (pure userApp)


businessLogicSpec :: Spec
businessLogicSpec =
  -- `around` will start our Server before the tests and turn it off after
  around withUserApp $ do
    -- create a test client function
    let createUser = client (Proxy :: Proxy UserApi)
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    -- testing scenarios start here
    describe "POST /user" $ do
      it "should create a user with a high enough ID" $ \port -> do
        result <- runClientM (createUser 50001) (clientEnv port)
        result `shouldBe` (Right $ User { name = "some user", user_id = 50001})
      it "will it fail with a too-small ID?" $ \port -> do
        result <- runClientM (createUser 4999) (clientEnv port)
        result `shouldBe` (Right $ User { name = "some user", user_id = 50001})