port module Shared exposing
    ( Flags
    , Model
    , Msg
    , decoder
    , hideFilterWindow
    , init
    , subscriptions
    , update
    )

import Effect exposing (Effect)
import Json.Decode
import Json.Decode as D
import Json.Decode.Pipeline as D exposing (required)
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg


port hideFilterWindow : () -> Cmd msg



-- FLAGS


type alias Flags =
    { localSettings : Shared.Model.LocalSettings }


decoder : Json.Decode.Decoder Flags
decoder =
    D.succeed Flags
        |> required "localSettings" decodeLocalSettings


decodeLocalSettings : Json.Decode.Decoder Shared.Model.LocalSettings
decodeLocalSettings =
    D.succeed Shared.Model.LocalSettings
        |> required "githubToken" (D.maybe D.string)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    let
        localSettings =
            case flagsResult of
                Ok flags ->
                    flags.localSettings

                Err _ ->
                    { githubToken = Nothing }
    in
    ( { localSettings = localSettings
      , permissionData = Nothing
      }
    , Effect.sendCmd <| hideFilterWindow ()
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.UpdatePermissionData pd ->
            ( { model | permissionData = pd }, Effect.none )

        Shared.Msg.NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
