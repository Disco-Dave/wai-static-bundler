module Network.Wai.StaticBundlerSpec where

import           Data.Foldable                  ( for_ )
import           Network.Wai                    ( Application )
import           Test.Hspec
import           Test.Hspec.Wai

import qualified Network.Wai.StaticBundler     as StaticBundler
import qualified Network.Wai                   as Wai
import qualified Network.HTTP.Types            as HTTP

spec :: Spec
spec = do
  describe "makeApplication" $ do
    it "should throw exception for files that that don't exist" $ do
      let jsBundle = StaticBundler.bundlesFromList
            [("js_bundle", StaticBundler.JsFiles ["does_not_exist.js"])]
          cssBundle = StaticBundler.bundlesFromList
            [("css_bundle", StaticBundler.CssFiles ["does_not_exist.css"])]
      for_ [jsBundle, cssBundle] $ \bundle ->
        StaticBundler.makeApplication bundle `shouldThrow` anyException

    with testApplication $ do
      it "responds with 404 when bundle is not found" $                   
        get "/does_not_exist.js" `shouldRespondWith` 404

      it "responds 405 for all methods aside from GET" $ do
        let badMethods =
              [ HTTP.methodPost
              , HTTP.methodHead
              , HTTP.methodPut
              , HTTP.methodDelete
              , HTTP.methodPatch
              ]
        for_ badMethods $ \badMethod -> do
          let badRequest = request badMethod "/example_bundle.js" mempty mempty
          badRequest `shouldRespondWith` 405

      it "retrieves a javascript bundle correctly" $ do
        let bundledJs = "function someExample2(bar){someExample1(foo)}\nfunction someExample2(bar){someExample1(foo)}"
        get "example_bundle.js" `shouldRespondWith` bundledJs
          { matchHeaders = ["Content-Type" <:> "text/javascript"]
          , matchStatus  = 200
          }

        let bundledJs2 = "console.log(\"I'm alone\")"
        get "some/lonely/bundle.js" `shouldRespondWith` bundledJs2
          { matchHeaders = ["Content-Type" <:> "text/javascript"]
          , matchStatus  = 200
          }

      it "retrieves a css bundle correctly" $ do
        let bundledCss = "body{color:red}a{font-weight:400}"
        get "example_bundle.css" `shouldRespondWith` bundledCss
          { matchHeaders = ["Content-Type" <:> "text/css"]
          , matchStatus  = 200
          }

        let bundledCss2 = "div.container{display:flex}"
        get "some/lonely/bundle.css" `shouldRespondWith` bundledCss2
          { matchHeaders = ["Content-Type" <:> "text/css"]
          , matchStatus  = 200
          }

  describe "makeMiddleware" $
    with testMiddleware $ do
      it "forwards to wrapped application when root does not match" $ 
        get "some/request" `shouldRespondWith` "OK!" { matchStatus = 200 }

      it "forwards to static bundler when root does match" $ do
        let bundledJs = "function someExample2(bar){someExample1(foo)}\nfunction someExample2(bar){someExample1(foo)}"
        get "our_bundles/example_bundle.js" `shouldRespondWith` bundledJs
          { matchHeaders = ["Content-Type" <:> "text/javascript"]
          , matchStatus  = 200
          }
        let bundledCss = "body{color:red}a{font-weight:400}"
        get "our_bundles/example_bundle.css" `shouldRespondWith` bundledCss
          { matchHeaders = ["Content-Type" <:> "text/css"]
          , matchStatus  = 200
          }


testApplication :: IO Application
testApplication = StaticBundler.makeApplication testBundle

testMiddleware :: IO Application
testMiddleware = do
  middleware <- StaticBundler.makeMiddleware "our_bundles" testBundle
  pure $ middleware sampleApplication
 where
  sampleApplication _ send = send $ Wai.responseLBS HTTP.status200 [] "OK!"

testBundle :: StaticBundler.Bundles
testBundle = StaticBundler.bundlesFromList
  [ ( "example_bundle.js"
    , StaticBundler.JsFiles
      [ "test/files/javascript/example1.js"
      , "test/files/javascript/example2.js"
      , "test/files/javascript/example2.js"
      ]
    )
  , ( "some/lonely/bundle.js"
    , StaticBundler.JsFiles ["test/files/javascript/some_lone_file.js"]
    )
  , ( "example_bundle.css"
    , StaticBundler.CssFiles
      ["test/files/css/example1.css", "test/files/css/example2.css"]
    )
  , ( "some/lonely/bundle.css"
    , StaticBundler.CssFiles ["test/files/css/lone_file.css"]
    )
  ]
