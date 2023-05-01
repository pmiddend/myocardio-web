{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Common
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Proxy
import Data.Time.Clock (getCurrentTime)
import JavaScript.Web.XMLHttpRequest (Method (GET, POST), Request (Request), RequestData (NoData, StringData), contents, reqData, reqHeaders, reqLogin, reqMethod, reqURI, reqWithCredentials, xhrByteString)
import Lens.Micro (ix, to, (^?))
import Miso hiding (now)
import Miso.String hiding (unpack)
import Myocardio.Exercise (repsL)
import Myocardio.ExerciseData (ExerciseData, exercisesL)

changeReps :: RepInfo -> IO ExerciseData
changeReps repInfo = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String ExerciseData of
    Left s -> error s
    Right j -> pure j
  where
    req =
      Request
        { reqMethod = POST,
          reqURI = pack "/exercises",
          reqLogin = Nothing,
          reqHeaders = [("Content-type", "application/json")],
          reqWithCredentials = False,
          reqData = StringData (pack (unpack (encode repInfo)))
        }

commit :: IO ExerciseData
commit = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String ExerciseData of
    Left s -> error s
    Right j -> pure j
  where
    req =
      Request
        { reqMethod = POST,
          reqURI = pack "/exercises/commit",
          reqLogin = Nothing,
          reqHeaders = [],
          reqWithCredentials = False,
          reqData = NoData
        }

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

toggleTagged :: Int -> IO ExerciseData
toggleTagged idx = do
  Just resp <- contents <$> xhrByteString req
  case eitherDecodeStrict resp :: Either String ExerciseData of
    Left s -> error s
    Right j -> pure j
  where
    req =
      Request
        { reqMethod = GET,
          reqURI = pack ("/exercises/" <> show idx),
          reqLogin = Nothing,
          reqHeaders = [],
          reqWithCredentials = False,
          reqData = NoData
        }

main :: IO ()
main = miso $ \currentURI ->
  App
    { model = Model currentURI Loading Nothing Nothing,
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
updateModel (OpenRepEdit idx) m =
  (m {repEdit = (loadedExerciseData m) ^? _Success . exercisesL . ix idx . repsL . to (\rl -> RepInfo idx rl)}) <# do
    pure NoOp
updateModel CancelEditIdx m =
  (m {repEdit = Nothing}) <# do
    pure NoOp
updateModel (ChangeEditIdx _ newValue) m =
  (m {repEdit = (\re -> re {repValue = newValue}) <$> (repEdit m)}) <# do
    pure NoOp
updateModel (ConfirmEditIdx idx newValue) m =
  m <# do
    NewExercisesReceived <$> changeReps (RepInfo idx newValue)
updateModel FetchExercises m =
  m <# do
    FetchExercisesDone <$> getCurrentTime <*> getRemoteExerciseData
updateModel Commit m =
  m <# do
    NewExercisesReceived <$> commit
updateModel (ToggleTagged idx) m =
  m <# do
    NewExercisesReceived <$> toggleTagged idx
updateModel (FetchExercisesDone now newExs) m =
  m {loadedExerciseData = Success newExs, now = Just now} <# do
    pure NoOp
updateModel (NewExercisesReceived newExs) m =
  m {loadedExerciseData = Success newExs, repEdit = Nothing} <# do
    pure NoOp
updateModel (HandleURI u) m =
  m {uri = u} <# do
    pure NoOp
updateModel (ChangeURI u) m =
  m <# do
    pushURI u
    pure NoOp
updateModel Alert m@Model {..} =
  m <# do
    alert $ pack (show uri)
    pure NoOp
updateModel NoOp m = noEff m
