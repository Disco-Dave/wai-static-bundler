module Network.Wai.StaticBundler
  ( BundleFiles(..)
  , Bundles
  , bundlesFromList
  , makeApplication
  , makeMiddleware
  )
where

import           Control.Monad                  ( (<$!>) )
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Map                       ( Map )
import           Data.Text                      ( Text )
import           Network.Wai                    ( Application
                                                , Middleware
                                                )

import qualified Data.ByteString               as ByteString
import qualified Data.ByteString.Lazy          as LByteString
import qualified Data.Map                      as Map
import qualified Data.Text                     as Text
import qualified Data.Text.Encoding            as Text
import qualified Data.Text.IO                  as Text
import qualified Hasmin
import qualified Network.HTTP.Types            as HTTP
import qualified Network.Wai                   as Wai
import qualified Text.Jasmine                  as Jasmine
import qualified UnliftIO.Exception            as UnliftIO


data BundleFiles
  = CssFiles ![FilePath]
  | JsFiles ![FilePath]

type Bundles = Map Text BundleFiles

bundlesFromList :: [(Text, BundleFiles)] -> Bundles
bundlesFromList = Map.fromList

data BundledFile
  = BundledCss !ByteString
  | BundledJs !ByteString


makeApplication :: Bundles -> IO Application
makeApplication bundles = do
  bundledFiles <- bundleEverything bundles
  pure $ \request send -> do
    let pathInfo      = Wai.pathInfo request
        requestedFile = Text.intercalate "/" pathInfo
    send $ if Wai.requestMethod request /= HTTP.methodGet
      then Wai.responseBuilder HTTP.methodNotAllowed405 [] mempty
      else case Map.lookup requestedFile bundledFiles of
        Nothing -> Wai.responseBuilder HTTP.notFound404 [] mempty
        Just (BundledCss css) ->
          Wai.responseLBS HTTP.ok200 [("Content-Type", "text/css")] css
        Just (BundledJs js) ->
          Wai.responseLBS HTTP.ok200 [("Content-Type", "text/javascript")] js

makeMiddleware :: Text -> Bundles -> IO Middleware
makeMiddleware root bundles = do
  bundleApplication <- makeApplication bundles
  pure $ \application request send ->
    case Wai.pathInfo request of
      p : t | p == root ->
        let modifiedRequest = request { Wai.pathInfo = t }
        in  bundleApplication modifiedRequest send
      _ -> application request send


bundleEverything :: Bundles -> IO (Map Text BundledFile)
bundleEverything bundles = Map.fromList <$!> bundledFiles
  where
    bundle (name, CssFiles files) = (name, ) <$> bundleCss files
    bundle (name, JsFiles files ) = (name, ) <$> bundleJs files
    bundledFiles = traverse bundle (Map.assocs bundles)


bundleCss :: [FilePath] -> IO BundledFile
bundleCss filePaths = do
  files <- traverse Text.readFile filePaths
  let contents = Text.intercalate "\n" files
  case Hasmin.minifyCSS contents of
    Left err -> UnliftIO.throwString err
    Right css ->
      let cssByteString = LByteString.fromStrict $ Text.encodeUtf8 css
      in  pure $ BundledCss cssByteString

bundleJs :: [FilePath] -> IO BundledFile
bundleJs filePaths = do
  files <- traverse ByteString.readFile filePaths
  let contents = ByteString.intercalate "\n" files
  case Jasmine.minifym (LByteString.fromStrict contents) of
    Left  err -> UnliftIO.throwString err
    Right js  -> pure $ BundledJs js
