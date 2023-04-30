{-# LANGUAGE RecordWildCards #-}

module Main where

import Common
import Data.Aeson (eitherDecodeStrict)
import Data.Proxy
import JavaScript.Web.XMLHttpRequest (Method (GET), Request (Request), RequestData (NoData), contents, reqData, reqHeaders, reqLogin, reqMethod, reqURI, reqWithCredentials, xhrByteString)
import Miso
import Miso.String
import Myocardio.ExerciseData (ExerciseData)

getRemoteExerciseData :: IO ExerciseData
getRemoteExerciseData = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String ExerciseData of
    Left s -> error s
    Right j -> pure j
  where
    req =
      Request
        { reqMethod = GET,
          reqURI = pack "/exercises",
          reqLogin = Nothing,
          reqHeaders = [],
          reqWithCredentials = False,
          reqData = NoData
        }

main :: IO ()
main = miso $ \currentURI ->
  App
    { model = Model currentURI False Loading,
      view = viewModel,
      ..
    }
  where
    initialAction = FetchExercises
    mountPoint = Nothing
    update = updateModel
    events = defaultEvents
    subs = [uriSub HandleURI]
    logLevel = DebugPrerender
    viewModel m =
      case runRoute (Proxy :: Proxy ClientRoutes) handlers uri m of
        Left _ -> the404 m
        Right v -> v

updateModel :: Action -> Model -> Effect Action Model
updateModel FetchExercises m =
  m <# do
    FetchExercisesDone <$> getRemoteExerciseData
updateModel (FetchExercisesDone newExs) m =
  m {loadedExerciseData = Success newExs} <# do
    pure NoOp
updateModel (HandleURI u) m =
  m {uri = u} <# do
    pure NoOp
updateModel (ChangeURI u) m =
  m {navMenuOpen = False} <# do
    pushURI u
    pure NoOp
updateModel Alert m@Model {..} =
  m <# do
    alert $ pack (show uri)
    pure NoOp
updateModel ToggleNavMenu m@Model {..} =
  m {navMenuOpen = not navMenuOpen} <# do
    pure NoOp
updateModel NoOp m = noEff m
