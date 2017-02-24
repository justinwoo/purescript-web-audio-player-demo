module Main where

import Prelude
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as D
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import DOM.Classy.HTMLElement (fromHTMLElement)
import DOM.File.FileList (item)
import DOM.HTML (window)
import DOM.HTML.HTMLInputElement (files)
import DOM.HTML.HTMLMediaElement (currentTime, setCurrentTime)
import DOM.HTML.Types (htmlAudioElementToHTMLMediaElement)
import DOM.HTML.URL (createObjectURL, revokeObjectURL)
import DOM.HTML.Window (url)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Nullable (toMaybe)

newtype ObjectURL = ObjectURL String
derive instance newtypeFilePath :: Newtype ObjectURL _
derive newtype instance filePathShow :: Show ObjectURL

type State =
  { file :: Maybe ObjectURL
  }

data SkipDir = Bck | Fwd
data SkipSize = Sm | Md | Lg

data Query a
  = FileSet a
  | Skip SkipDir SkipSize a

type AppEffects eff =
  Aff
  ( console :: CONSOLE
  , dom :: DOM
  | eff)

ui :: forall eff. H.Component HH.HTML Query Unit Void (AppEffects eff)
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }

  where
    initialState =
      { file: Nothing
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.class_ $ wrap "container" ]
        [ HH.div
          [ HP.class_ $ wrap "root" ]
          [ HH.h1_ [HH.text "glorious web audio thing"]
          , HH.div_
              [ HH.input
                [ HP.ref $ wrap "input"
                , HP.type_ HP.InputFile
                , HP.prop (wrap "accept")  "audio/*"
                , HE.onChange (HE.input_ FileSet)
                ]
              ]
          , HH.div_
              [ HH.audio
                [ HP.ref $ wrap "audio"
                , HP.src $ unwrap $ fromMaybe (wrap "") state.file
                , HP.controls true
                , HP.autoplay true
                ]
              []
            ]
          , HH.div
              [ HP.class_ $ wrap "buttons" ]
              [ HH.button [HE.onClick (HE.input_ $ Skip Bck Lg)] [HH.text "<<<"]
              , HH.button [HE.onClick (HE.input_ $ Skip Bck Md)] [HH.text "<<"]
              , HH.button [HE.onClick (HE.input_ $ Skip Bck Sm)] [HH.text "<"]
              , HH.button [HE.onClick (HE.input_ $ Skip Fwd Sm)] [HH.text ">"]
              , HH.button [HE.onClick (HE.input_ $ Skip Fwd Md)] [HH.text ">>"]
              , HH.button [HE.onClick (HE.input_ $ Skip Fwd Lg)] [HH.text ">>>"]
              ]
          ]
      ]

    eval :: Query ~> H.ComponentDSL State Query Void (AppEffects eff)
    eval (FileSet next) = do
      input <- H.getHTMLElementRef $ wrap "input"
      case input >>= fromHTMLElement of
        Just el -> do
          nxs <- H.liftEff <<< files $ el
          case toMaybe nxs >>= toMaybe <<< item 0 of
            Just file -> do
              url <- H.liftEff $ url =<< window
              blob <- H.liftEff $ createObjectURL file url
              prevBlob <- H.gets _.file
              case prevBlob of
                Just x ->
                  H.liftEff $ revokeObjectURL (unwrap x) url
                _ -> pure unit
              H.modify \s ->
                s {file = Just <<< wrap $ blob}
            _ -> H.liftAff $ log "No file found"
        _ -> H.liftAff $ log "No input ref found"
      pure next

    eval (Skip dir size next) = do
      let skip = case size of
            Lg -> 30.0
            Md -> 10.0
            Sm -> 5.0
      let delta = skip * case dir of
            Bck -> -1.0
            _ -> 1.0
      audio <- H.getHTMLElementRef $ wrap "audio"
      case audio >>= fromHTMLElement of
        Just el -> do
          let el' = htmlAudioElementToHTMLMediaElement el
          current <- H.liftEff $ currentTime el'
          H.liftEff $ setCurrentTime (current + delta) el'
        _ -> H.liftAff $ log "No audio ref found"
      pure next

main :: forall e.
  Eff
    ( avar :: AVAR
    , ref :: REF
    , err :: EXCEPTION
    , dom :: DOM
    , console :: CONSOLE
    | e
    )
    Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- D.runUI ui unit body

  log "Running"
