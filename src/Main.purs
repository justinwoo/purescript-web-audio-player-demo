module Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.Except.Trans (lift, runExceptT)
import DOM (DOM)
import DOM.Classy.HTMLElement (fromHTMLElement)
import DOM.File.FileList (item)
import DOM.HTML (window)
import DOM.HTML.HTMLInputElement (files)
import DOM.HTML.HTMLMediaElement (currentTime, setCurrentTime, setPlaybackRate)
import DOM.HTML.Types (htmlAudioElementToHTMLMediaElement)
import DOM.HTML.URL (createObjectURL, revokeObjectURL)
import DOM.HTML.Window (url)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as D

newtype ObjectURL = ObjectURL String
derive instance newtypeFilePath :: Newtype ObjectURL _

type State =
  { file :: Maybe ObjectURL
  }

data SkipDir = Bck | Fwd
data SkipSize = Sm | Md | Lg
data Speed = One | OneHalf | Two

data Query a
  = FileSet a
  | Skip SkipDir SkipSize a
  | SetSpeed Speed a

type AppEffects eff =
  ( console :: CONSOLE
  , dom :: DOM
  | eff)

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (AppEffects eff))
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
                , HP.src $ fromMaybe "" (unwrap <$> state.file)
                , HP.controls true
                , HP.autoplay true
                ]
              []
            ]
          , HH.div
              [ HP.class_ $ wrap "buttons" ]
              [ HH.button [HE.onClick $ HE.input_ (Skip Bck Lg)] [HH.label_ [HH.text "<<<"]]
              , HH.button [HE.onClick $ HE.input_ (Skip Bck Md)] [HH.label_ [HH.text "<<"]]
              , HH.button [HE.onClick $ HE.input_ (Skip Bck Sm)] [HH.label_ [HH.text "<"]]
              , HH.button [HE.onClick $ HE.input_ (Skip Fwd Sm)] [HH.label_ [HH.text ">"]]
              , HH.button [HE.onClick $ HE.input_ (Skip Fwd Md)] [HH.label_ [HH.text ">>"]]
              , HH.button [HE.onClick $ HE.input_ (Skip Fwd Lg)] [HH.label_ [HH.text ">>>"]]
              ]
          , HH.div
              [ HP.class_ $ wrap "speed" ]
              [ HH.button [HE.onClick $ HE.input_ (SetSpeed One)] [HH.label_ [HH.text "1x"]]
              , HH.button [HE.onClick $ HE.input_ (SetSpeed OneHalf)] [HH.label_ [HH.text "1.5x"]]
              , HH.button [HE.onClick $ HE.input_ (SetSpeed Two)] [HH.label_ [HH.text "2x"]]
              ]
          ]
      ]

    log' = H.liftAff <<< log

    eval :: Query ~> H.ComponentDSL State Query Void (Aff (AppEffects eff))
    eval (FileSet next) = do
      result <- runExceptT do
        em <- onNothing "no input found" =<< (lift <<< H.getHTMLElementRef $ wrap "input")
        el <- onNothing "conversion failed" $ fromHTMLElement em
        fs <- onNothing "no file found" =<< (lift <<< H.liftEff <<< files $ el)
        onNothing "files was empty" $ item 0 fs
      case result of
        Left e -> log' e
        Right file -> do
          url <- H.liftEff $ url =<< window
          blob <- H.liftEff $ createObjectURL file url
          prevBlob <- H.gets _.file
          case prevBlob of
            Just x -> H.liftEff $ revokeObjectURL (unwrap x) url
            _ -> pure unit
          H.modify \s ->
            s {file = pure $ wrap blob}
      pure next
      where
        onNothing :: forall m. Monad m => String -> Maybe ~> ExceptT String m
        onNothing s = maybe (throwError s) pure

    eval (Skip dir size next) = do
      audio <- H.getHTMLElementRef $ wrap "audio"
      case htmlAudioElementToHTMLMediaElement <$> (fromHTMLElement =<< audio) of
        Just el -> do
          current <- H.liftEff $ currentTime el
          H.liftEff $ setCurrentTime (current + delta) el
        _ -> log' "No audio ref found"
      pure next
      where
        skip = case size of
          Lg -> 30.0
          Md -> 10.0
          Sm -> 5.0
        delta = skip * case dir of
          Bck -> -1.0
          _ -> 1.0
    eval (SetSpeed speed next) = do
      audio <- H.getHTMLElementRef $ wrap "audio"
      case htmlAudioElementToHTMLMediaElement <$> (fromHTMLElement =<< audio) of
        Just el -> do
          H.liftEff $ setPlaybackRate rate el
        _ -> log' "No audio ref found"
      pure next
      where
        rate = case speed of
          One -> 1.0
          OneHalf -> 1.5
          Two -> 2.0

main :: forall e.
  Eff
    (AppEffects
      ( avar :: AVAR
      , ref :: REF
      , exception :: EXCEPTION
      | e
      )
    )
    Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- D.runUI ui unit body

  log "Running"
