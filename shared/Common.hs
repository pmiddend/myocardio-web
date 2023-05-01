{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Common where

import Data.Aeson (FromJSON, ToJSON)
import Data.List (zipWith)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Proxy
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Lens.Micro.Platform (Traversal', to, (^.))
import Miso hiding (now)
import Miso.String hiding (zipWith)
import Myocardio.Exercise (lastL, nameL, repsL, taggedL)
import Myocardio.ExerciseData (ExerciseData, exercisesL)
import Myocardio.FormatTime (formatTimeDiff)
import Servant.API
import Servant.Links

-- | We can pretty much share everything
--
-- model, action, view, router, links, events map
-- decoders are all shareable
data RemoteData a = NotAsked | Loading | Success a deriving (Show, Eq, Functor)

_Success :: Traversal' (RemoteData a) a
_Success f (Success a) = Success <$> f a
_Success _ NotAsked = pure NotAsked
_Success _ Loading = pure Loading

data RepInfo = RepInfo
  { repIdx :: Int,
    repValue :: Text
  }
  deriving (Generic, FromJSON, ToJSON, Eq, Show)

-- | Model
data Model = Model
  { uri :: URI,
    loadedExerciseData :: RemoteData ExerciseData,
    now :: Maybe UTCTime,
    repEdit :: Maybe RepInfo
  }
  deriving (Show, Eq)

-- | Event Actions
data Action
  = Alert
  | ChangeURI URI
  | HandleURI URI
  | NoOp
  | FetchExercises
  | Commit
  | NewExercisesReceived ExerciseData
  | ToggleTagged Int
  | FetchExercisesDone UTCTime ExerciseData
  | OpenRepEdit Int
  | ChangeEditIdx Int Text
  | ConfirmEditIdx Int Text
  | CancelEditIdx
  deriving (Show, Eq)

-- | Router
type ClientRoutes = Home :<|> Visual

-- | Handlers
handlers :: (Model -> View Action) :<|> (Model -> View Action)
handlers =
  home :<|> visual

-- | Client Routes
type Visual = "visual" :> View Action

type Home = View Action

-- | Views
visual :: Model -> View Action
visual = template v
  where
    v =
      div_
        [class_ "animated fadeIn"]
        [ a_
            [href_ "https://github.com/dmjio/miso"]
            [ img_
                [ width_ "100",
                  class_ "animated bounceInDown",
                  src_ misoSrc,
                  alt_ "miso logo"
                ]
            ],
          h1_
            [ class_ "title animated pulse",
              style_ $
                M.fromList
                  [ (pack "font-size", pack "82px"),
                    (pack "font-weight", pack "100")
                  ]
            ]
            [text "community"],
          h2_
            [class_ "subtitle animated pulse"]
            [ a_
                [ href_ "https://haskell-miso-slack.herokuapp.com/",
                  target_ "_blank"
                ]
                [text "Slack"],
              text " / ",
              a_
                [ href_ "https://www.irccloud.com/invite?channel=%23haskell-miso&hostname=irc.libera.chat&port=6697&ssl=1",
                  target_ "_blank"
                ]
                [text "#haskell-miso"]
            ]
        ]

misoSrc :: MisoString
misoSrc = pack "https://em-content.zobj.net/thumbs/240/apple/325/steaming-bowl_1f35c.png"

home :: Model -> View Action
home m = template v m
  where
    repsLink idx exs = a_ [href_ "#", onClick (OpenRepEdit idx)] [text (exs ^. repsL . to toMisoString)]
    makeTableRow idx exs =
      tr_
        []
        [ td_
            []
            [ input_
                [ type_ "checkbox",
                  class_ "form-check-input",
                  checked_ (isJust (exs ^. taggedL)),
                  onClick (ToggleTagged idx)
                ]
            ],
          td_ [] [text (exs ^. nameL . to toMisoString)],
          td_
            []
            [ case repEdit m of
                Just repEdit' ->
                  if idx == repIdx repEdit'
                    then
                      div_
                        [class_ "hstack gap-1"]
                        [ input_
                            [ type_ "text",
                              value_ (toMisoString (repValue repEdit')),
                              class_ "form-control form-control-sm",
                              onInput (ChangeEditIdx idx . fromMisoString)
                            ],
                          div_ [class_ "vr"] [],
                          button_
                            [ class_ "btn btn-outline-success btn-sm",
                              onClick (ConfirmEditIdx idx (repValue repEdit'))
                            ]
                            [text "✅"],
                          button_
                            [ class_ "btn btn-outline-danger btn-sm",
                              onClick CancelEditIdx
                            ]
                            [text "❌"]
                        ]
                    else repsLink idx exs
                _ -> repsLink idx exs
            ],
          td_ [] [text (toMisoString (maybe "" (maybe (const "") formatTimeDiff (now m)) (exs ^. lastL)))]
        ]
    v =
      div_
        [class_ "container mt-3"]
        [ div_
            [class_ "hstack gap-3"]
            [ button_ [class_ "btn btn-outline-secondary btn-sm", onClick FetchExercises] [text "🔄 Reload"],
              div_ [class_ "vr"] [],
              button_ [class_ "btn btn-outline-primary btn-sm", onClick Commit] [text "✅ Commit"]
            ],
          case loadedExerciseData m of
            NotAsked -> text "Not asked"
            Loading -> text "Loading"
            Success loadedExercises ->
              table_
                [class_ "table"]
                ( ( tr_
                      []
                      [ th_ [] [text "Done"],
                        th_ [] [text "Name"],
                        th_ [] [text "Reps"],
                        th_ [] [text "Last"]
                      ]
                  )
                    : (zipWith makeTableRow [0 ..] (loadedExercises ^. exercisesL))
                )
        ]

template :: View Action -> Model -> View Action
template content Model {..} =
  div_ [] [hero content uri]

the404 :: Model -> View Action
the404 = template v
  where
    v =
      div_
        []
        [ a_
            [href_ "https://github.com/dmjio/miso"]
            [ img_
                [ width_ "100",
                  class_ "animated bounceOutUp",
                  src_ misoSrc,
                  alt_ "miso logo"
                ]
            ],
          h1_
            [ class_ "title",
              style_ $
                M.fromList
                  [ (pack "font-size", pack "82px"),
                    (pack "font-weight", pack "100")
                  ]
            ]
            [text "404"],
          h2_
            [class_ "subtitle animated pulse"]
            [ text "No soup for you! ",
              a_ [href_ "/", onPreventClick (ChangeURI goHome)] [text " - Go Home"]
            ]
        ]

-- | Links
goHome, goVisual :: URI
(goHome, goVisual) =
  ( linkURI (safeLink routes homeProxy),
    linkURI (safeLink routes visualProxy)
  )

homeProxy :: Proxy Home
homeProxy = Proxy

visualProxy :: Proxy Visual
visualProxy = Proxy

routes :: Proxy ClientRoutes
routes = Proxy

-- | Hero
hero :: View Action -> URI -> View Action
hero content uri' =
  div_
    []
    [ nav_
        [class_ "navbar navbar-expand bg-light"]
        [ ul_
            [class_ "navbar-nav"]
            [ li_
                [class_ "nav-item"]
                [ a_
                    [href_ "/", onPreventClick (ChangeURI goHome), class_ ("nav-link" <> if uriPath uri' == "/" || uriPath uri' == "" then " active" else "")]
                    [text "🏡 Home"]
                ],
              li_
                [class_ "nav-item"]
                [ a_
                    [href_ "/visual", onPreventClick (ChangeURI goVisual), class_ ("nav-link" <> if uriPath uri' == "/visual" then " active" else "")]
                    [text "💪 Visualized"]
                ]
            ]
        ],
      div_
        [class_ "container"]
        [ content
        ]
    ]

onPreventClick :: Action -> Attribute Action
onPreventClick action =
  onWithOptions
    defaultOptions {preventDefault = True}
    "click"
    emptyDecoder
    (\() -> action)
