{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Common where

import Data.Bool
import qualified Data.Map as M
import Data.Proxy
import Lens.Micro.Platform (to, (^.))
import Miso
import Miso.String
import Myocardio.Exercise (nameL)
import Myocardio.ExerciseData (ExerciseData, exercisesL)
import Servant.API
import Servant.Links

-- | We can pretty much share everything
--
-- model, action, view, router, links, events map
-- decoders are all shareable
data RemoteData a = NotAsked | Loading | Success a deriving (Show, Eq)

-- | Model
data Model = Model
  { uri :: URI,
    navMenuOpen :: Bool,
    loadedExerciseData :: RemoteData ExerciseData
  }
  deriving (Show, Eq)

-- | Event Actions
data Action
  = Alert
  | ChangeURI URI
  | HandleURI URI
  | ToggleNavMenu
  | NoOp
  | FetchExercises
  | FetchExercisesDone ExerciseData
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
    makeTableRow exs = tr_ [] [td_ [] [text (exs ^. nameL . to toMisoString)]]
    v =
      div_
        [class_ "container"]
        [ button_ [class_ "btn btn-primary", onClick FetchExercises] [text "fetch"],
          case loadedExerciseData m of
            NotAsked -> text "Not asked"
            Loading -> text "Loading"
            Success loadedExercises ->
              table_
                [class_ "table"]
                ( (tr_ [] [th_ [] [text "Name"]])
                    : (makeTableRow <$> (loadedExercises ^. exercisesL))
                )
        ]

template :: View Action -> Model -> View Action
template content Model {..} =
  div_ [] [hero content uri navMenuOpen]

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
hero :: View Action -> URI -> Bool -> View Action
hero content uri' navMenuOpen' =
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
                    [text "Home"]
                ],
              li_
                [class_ "nav-item"]
                [ a_
                    [href_ "/visual", onPreventClick (ChangeURI goVisual), class_ ("nav-link" <> if uriPath uri' == "/visual" then " active" else "")]
                    [text "Visualized"]
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
