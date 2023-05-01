{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Common
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import GHC.Generics
import Lens.Micro.Platform (ix, (%~), (&), (.~))
import qualified Lucid as L
import Lucid.Base
import Miso hiding (now, send)
import Myocardio.ConfigJson (readDataFile, writeDataFile)
import Myocardio.Exercise (commit, repsL, toggleTag)
import Myocardio.ExerciseData (ExerciseData, exercisesL)
import Myocardio.Ranking (reorderExercises)
import Network.HTTP.Types hiding (Header)
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger
import Servant
import qualified System.IO as IO

main :: IO ()
main = do
  IO.hPutStrLn IO.stderr "Running on port 3002..."
  run 3002 $ logStdout (compress app)
  where
    compress = gzip def {gzipFiles = GzipCompress}

app :: Application
app = serve (Proxy @ API) (static :<|> handleGetExercises :<|> handleTagExercise :<|> handleChangeRep :<|> handleCommit :<|> serverHandlers :<|> pure misoManifest :<|> Tagged handle404)
  where
    static = serveDirectoryWith (defaultWebAppSettings "static")

-- | Wrapper for setting HTML doctype and header
newtype Wrapper a = Wrapper a
  deriving (Show, Eq)

-- | Convert client side routes into server-side web handlers
type ServerRoutes = ToServerRoutes ClientRoutes Wrapper Action

-- | API type
type API =
  ("static" :> Raw)
    :<|> ("exercises" :> Get '[JSON] ExerciseData)
    :<|> ("exercises" :> Capture "index" Int :> Get '[JSON] ExerciseData)
    :<|> ("exercises" :> ReqBody '[JSON] RepInfo :> Post '[JSON] ExerciseData)
    :<|> ("exercises" :> "commit" :> Post '[JSON] ExerciseData)
    :<|> ServerRoutes
    :<|> ("manifest.json" :> Get '[JSON] Manifest)
    :<|> Raw

data Manifest = Manifest
  { name :: Text,
    short_name :: Text,
    start_url :: Text,
    display :: Text,
    theme_color :: Text,
    description :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON Manifest

misoManifest :: Manifest
misoManifest =
  Manifest
    { name = "Haskell Miso",
      short_name = "Miso",
      start_url = ".",
      display = "standalone",
      theme_color = "#00d1b2",
      description = "A tasty Haskell front-end framework"
    }

handleTagExercise :: Int -> Handler ExerciseData
handleTagExercise index = do
  now' <- liftIO getCurrentTime
  exerciseData <- liftIO readDataFile
  let newExerciseData = exerciseData & exercisesL . ix index %~ toggleTag now'
  liftIO (writeDataFile newExerciseData)
  pure newExerciseData

handleChangeRep :: RepInfo -> Handler ExerciseData
handleChangeRep repInfo = do
  exerciseData <- liftIO readDataFile
  let newExerciseData = exerciseData & exercisesL . ix (repIdx repInfo) . repsL .~ repValue repInfo
  liftIO (writeDataFile newExerciseData)
  pure newExerciseData

handleCommit :: Handler ExerciseData
handleCommit = do
  exerciseData <- liftIO readDataFile
  let newExerciseData = exerciseData & exercisesL %~ (reorderExercises . (commit <$>))
  liftIO (writeDataFile newExerciseData)
  pure newExerciseData

handleGetExercises :: Handler ExerciseData
handleGetExercises = do
  exercises <- liftIO readDataFile
  pure exercises

handle404 :: Application
handle404 _ respond =
  respond
    $ responseLBS
      status404
      [("Content-Type", "text/html")]
    $ renderBS
    $ toHtml
    $ Wrapper
    $ the404 Model {uri = goHome, loadedExerciseData = NotAsked, now = Nothing, repEdit = Nothing}

instance L.ToHtml a => L.ToHtml (Wrapper a) where
  toHtmlRaw = L.toHtml
  toHtml (Wrapper x) = do
    L.doctype_
    L.html_ [L.lang_ "en"] $ do
      L.head_ $ do
        L.title_ "💪 myocardio - magic for your exercise plans!"
        L.link_
          [ L.rel_ "stylesheet",
            L.href_ "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/css/bootstrap.min.css"
          ]
        L.link_
          [ L.rel_ "manifest",
            L.href_ "/manifest.json"
          ]
        L.link_ [L.rel_ "icon", L.href_ "https://www.haskell.org/img/favicon.ico", L.type_ "image/x-icon"]
        L.meta_ [L.charset_ "utf-8"]
        L.meta_ [L.name_ "theme-color", L.content_ "#00d1b2"]
        L.meta_
          [ L.httpEquiv_ "X-UA-Compatible",
            L.content_ "IE=edge"
          ]
        L.meta_
          [ L.name_ "viewport",
            L.content_ "width=device-width, initial-scale=1"
          ]
        jsRef "static/all.js"
      L.body_ (L.toHtml x <> jsRef "https://cdn.jsdelivr.net/npm/bootstrap@5.2.3/dist/js/bootstrap.bundle.min.js")
    where
      jsRef href =
        L.with
          (L.script_ mempty)
          [ makeAttribute "src" href,
            makeAttribute "async" mempty,
            makeAttribute "defer" mempty
          ]

serverHandlers ::
  Handler (Wrapper (View Action))
    :<|> Handler (Wrapper (View Action))
serverHandlers =
  visualHandler :<|> homeHandler
  where
    send f u =
      pure $
        Wrapper $
          f
            Model
              { uri = u,
                loadedExerciseData = NotAsked,
                now = Nothing,
                repEdit = Nothing
              }
    homeHandler = send home goHome
    visualHandler = send visual goVisual
