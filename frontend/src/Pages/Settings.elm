port module Pages.Settings exposing (Model, Msg, page)

import Components.Footer as Footer
import Components.Header as Header
import Effect exposing (Effect)
import Html
    exposing
        ( Html
        , a
        , div
        , h1
        , h3
        , input
        , label
        , p
        , span
        , text
        )
import Html.Attributes
    exposing
        ( class
        , href
        , placeholder
        , type_
        , value
        )
import Html.Events exposing (onInput)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import View exposing (View)


githubTokenKeyName : String
githubTokenKeyName =
    "githubToken"


port writeLocalStorage : { key : String, value : String } -> Cmd msg


port queryGithubToken : { key : String } -> Cmd msg


port readGithubToken : (String -> msg) -> Sub msg


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
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { header = Header.init { activePage = Just Path.Settings }
      , footer = Footer.init ()
      , shared = shared
      }
    , Effect.batch
        [ Effect.sendCmd <| queryGithubToken { key = githubTokenKeyName }
        , Effect.sendCmd <| Shared.hideFilterWindow ()
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | HeaderEvent (Header.Msg Msg)
    | FooterEvent (Footer.Msg Msg)
    | UpdateGitHubToken String
    | ReceiveGitHubToken String


updateGithubToken : String -> Model -> Model
updateGithubToken token model =
    let
        shared =
            model.shared

        localSettings =
            model.shared.localSettings

        newLocalSettings =
            if token /= "" then
                { localSettings | githubToken = Just token }

            else
                { localSettings | githubToken = Nothing }

        newShared =
            { shared | localSettings = newLocalSettings }
    in
    { model | shared = newShared }


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReceiveGitHubToken token ->
            ( updateGithubToken token model
            , Effect.none
            )

        UpdateGitHubToken token ->
            ( updateGithubToken token model
            , Effect.sendCmd <|
                writeLocalStorage { key = githubTokenKeyName, value = token }
            )

        HeaderEvent innerMsg ->
            ( model, Effect.none )

        FooterEvent innerMsg ->
            ( model, Effect.none )

        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    readGithubToken ReceiveGitHubToken



-- VIEW


view : Model -> View Msg
view model =
    { title = "Settings | Metadelta"
    , body = body model
    }


body : Model -> List (Html Msg)
body model =
    [ Header.new
        { model = model.header
        , toMsg = HeaderEvent
        }
        |> Header.view
    , h3 [] [ text "Local settings" ]
    , div [ class "content" ]
        [ p [ class "content" ]
            [ text """ The following are all local settings that are stored in your
          browsers localStorage. Nothing from here is sent to our servers in any way.
          """
            ]
        ]
    , h3 [] [ text "GitHub Integration" ]
    , githubOptions model
    , Footer.new
        { model = model.footer
        , toMsg = FooterEvent
        }
        |> Footer.view
    ]


githubOptions : Model -> Html Msg
githubOptions model =
    div
        [ class "content options" ]
        [ p []
            [ text "If you would like to use the "
            , a [ href "https://github.com/InvariantClub/metadelta-github-action" ] [ text "GitHub Action integration" ]
            , text " , which creates a GitHub Artifact for the diff"
            , text " between a PR and the main branch, then it is necessary to "
            , text " configure a GitHub token."
            ]
        , div [ class "option" ]
            [ span [ class "name" ] [ text "Personal acccess token" ]
            , div
                [ class "inputs" ]
                [ input
                    [ class "token"
                    , type_ "text"
                    , placeholder "abc123..."
                    , onInput UpdateGitHubToken
                    , value (Maybe.withDefault "" model.shared.localSettings.githubToken)
                    ]
                    []
                , div
                    [ class "help" ]
                    [ p []
                        [ text "GitHub requires a personal access token with scope "
                        , span [ class "tt" ] [ text "repo" ]
                        , text " to download artifacts."
                        , text " See "
                        , a [ href "https://docs.github.com/en/rest/actions/artifacts?apiVersion=2022-11-28#download-an-artifact" ]
                            [ text "GitHub Rest API - Download an artifact" ]
                        , text " for more details."
                        ]
                    , p []
                        [ text "Visit "
                        , a [ href "https://github.com/settings/tokens" ] [ text "github.com/settings/tokens" ]
                        , text " to create one."
                        ]
                    ]
                ]
            ]
        ]
