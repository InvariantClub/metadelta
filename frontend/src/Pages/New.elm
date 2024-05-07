module Pages.New exposing (Model, Msg, page)

import Components.Footer as Footer
import Components.Header as Header
import Decoding exposing (decodePermissionData)
import Effect exposing (Effect, pushRoutePath)
import Html
    exposing
        ( Html
        , a
        , button
        , code
        , div
        , h1
        , h3
        , h4
        , input
        , label
        , li
        , ol
        , p
        , pre
        , span
        , text
        , textarea
        )
import Html.Attributes
    exposing
        ( class
        , disabled
        , href
        , placeholder
        , target
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (decodeString)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Types exposing (PermissionData)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    { header : Header.Model
    , footer : Footer.Model
    , shared : Shared.Model
    , pasteState : PasteState
    }


type alias PasteState =
    { jsonValid : Maybe Bool
    , error : Maybe String
    }


emptyPasteState : PasteState
emptyPasteState =
    { jsonValid = Nothing
    , error = Nothing
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { header = Header.init { activePage = Just Path.New }
      , footer = Footer.init ()
      , shared = shared
      , pasteState = emptyPasteState
      }
    , Effect.sendCmd <| Shared.hideFilterWindow ()
    )



-- UPDATE


type Msg
    = NoOp
    | HeaderEvent (Header.Msg Msg)
    | FooterEvent (Footer.Msg Msg)
    | ValidatePastedJson String
    | ViewPermissionData


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        HeaderEvent innerMsg ->
            ( model, Effect.none )

        FooterEvent innerMsg ->
            ( model, Effect.none )

        NoOp ->
            ( model
            , Effect.none
            )

        ViewPermissionData ->
            ( model
            , pushRoutePath Path.Explorer
            )

        ValidatePastedJson s ->
            let
                a =
                    decodeString decodePermissionData s

                ( pd, v, err ) =
                    case ( a, s ) of
                        ( Ok pd_, _ ) ->
                            ( Just pd_, Just True, Nothing )

                        ( Err _, "" ) ->
                            ( Nothing, Nothing, Nothing )

                        ( Err e, _ ) ->
                            ( Nothing, Just False, Just <| Json.Decode.errorToString e )

                ps =
                    model.pasteState

                newPasteState =
                    { ps | jsonValid = v, error = err }

                f : Maybe PermissionData -> Maybe PermissionData
                f x =
                    x
            in
            ( { model | pasteState = newPasteState }
            , Effect.updatePermissionData (f pd)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "New | Metadelta"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    [ Header.new
        { model = model.header
        , toMsg = HeaderEvent
        }
        |> Header.view
    , pasteUI model
    , Footer.new
        { model = model.footer
        , toMsg = FooterEvent
        }
        |> Footer.view
    ]


pasteUI : Model -> Html Msg
pasteUI model =
    let
        elt =
            case model.pasteState.jsonValid of
                Nothing ->
                    [ span [] [ text "" ] ]

                Just False ->
                    [ span [ class "invalid" ] [ text "Invalid" ] ]

                Just True ->
                    [ span [ class "valid" ] [ text "Valid" ] ]

        err =
            case model.pasteState.error of
                Nothing ->
                    []

                Just e ->
                    [ pre [] [ text e ] ]
    in
    div
        []
        [ h3 [] [ text "Getting data into Metadelta" ]
        , div [ class "content" ]
            [ p []
                [ text "There are a few ways to get data into the Metadelta UI:"
                ]
            , ol []
                [ li [] [ text "Paste in a JSON file (see below)," ]
                , li []
                    [ text "Provide a URL of a JSON file via the "
                    , a [ href "/explorer?src=" ]
                        [ code [] [ text "src" ]
                        , text " parameter on the "
                        , text "explorer page"
                        ]
                    , text " (see the "
                    , a [ href "/" ] [ text "home page" ]
                    , text " for examples)"
                    , text ","
                    ]
                , li []
                    [ text "Use the "
                    , a [ href "https://github.com/InvariantClub/metadelta-github-action" ] [ text "GitHub Action integration" ]
                    , text "."
                    ]
                ]
            ]
        , h3 [] [ text "Load a new Metadelta JSON file" ]
        , div
            [ class "content" ]
            [ p [] [ text "Paste a JSON file." ]
            , p []
                [ text "You can generate a one by using the "
                , a [ href "https://invariant.club/", target "_blank" ] [ text "Metadelta cli" ]
                , text "."
                ]
            ]
        , div [ class "paste" ] <|
            [ textarea [ onInput ValidatePastedJson ] []
            , div [ class "buttons" ] <|
                elt
                    ++ [ button
                            [ class "b"
                            , disabled (Maybe.withDefault True (Maybe.map not model.pasteState.jsonValid))
                            , onClick ViewPermissionData
                            ]
                            [ text "View" ]
                       ]
            ]
                ++ err
        ]
