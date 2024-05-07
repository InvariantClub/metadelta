module Pages.Home_ exposing (..)

import Components.Footer as Footer
import Components.Header as Header
import Effect exposing (Effect)
import Extras exposing (..)
import Html
    exposing
        ( Html
        , a
        , div
        , h1
        , h2
        , h3
        , h4
        , li
        , p
        , small
        , span
        , text
        , ul
        )
import Html.Attributes
    exposing
        ( class
        , href
        , id
        )
import Http exposing (Error(..))
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)
import Missing exposing (..)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import View exposing (View)
import Web exposing (loadJson)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = const Sub.none
        , view = view
        }


type alias Model =
    { examples : List String
    , header : Header.Model
    , footer : Footer.Model
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { examples = []
      , header = Header.init { activePage = Just Path.Home_ }
      , footer = Footer.init ()
      }
    , Effect.batch
        [ Effect.sendCmd <|
            loadJson "/all-examples.json" JsonLoaded decodeExamples
        , Effect.sendCmd <| Shared.hideFilterWindow ()
        ]
    )


type Msg
    = JsonLoaded (Result Http.Error (List String))
    | HeaderEvent (Header.Msg Msg)
    | FooterEvent (Footer.Msg Msg)


decodeExamples : Decoder (List String)
decodeExamples =
    D.list D.string


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        HeaderEvent innerMsg ->
            ( model, Effect.none )

        FooterEvent innerMsg ->
            ( model, Effect.none )

        JsonLoaded r ->
            let
                newModel =
                    case r of
                        Err _ ->
                            model

                        Ok examples ->
                            { model | examples = examples }
            in
            ( newModel
            , Effect.none
            )


view : Model -> View Msg
view model =
    { title = "Home | Metadelta"
    , body =
        let
            mkLink t =
                li [] [ a [ href <| "/explorer?src=/examples/" ++ t ] [ text t ] ]

            exs =
                ul [] <| List.map mkLink model.examples
        in
        [ Header.new
            { model = model.header
            , toMsg = HeaderEvent
            }
            |> Header.view
        , h3 [] [ text "Examples" ]
        , div
            [ class "content" ]
            [ p []
                [ text "Below are some pre-computed diffs from our "
                , a [ href "https://github.com/InvariantClub/demo-database" ]
                    [ text "demo-database" ]
                , text " repo;"
                , text " take a look to get a feeling for Metadelta!"
                ]
            , exs
            ]
        , Footer.new
            { model = model.footer
            , toMsg = FooterEvent
            }
            |> Footer.view
        ]
    }
