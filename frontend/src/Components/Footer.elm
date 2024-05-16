module Components.Footer exposing
    ( Footer
    , Model
    , Msg
    , init
    , new
    , view
    )

import Effect exposing (Effect)
import Html
    exposing
        ( Html
        , a
        , div
        , p
        , text
        )
import Html.Attributes
    exposing
        ( class
        , href
        )


type Footer msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    }
    -> Footer msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        }


type Model
    = Model


init : () -> Model
init props =
    Model



-- Update


type Msg msg
    = NoOp


update :
    { msg : Msg msg
    , model : Model
    , toModel : Model -> model
    , toMsg : Msg msg -> msg
    }
    -> ( model, Effect msg )
update props =
    let
        Model =
            props.model

        toParentModel : ( Model, Effect msg ) -> ( model, Effect msg )
        toParentModel ( innerModel, effect ) =
            ( props.toModel innerModel
            , effect
            )
    in
    toParentModel <|
        case props.msg of
            NoOp ->
                ( Model, Effect.none )



-- View


view : Footer msg -> Html msg
view (Settings settings) =
    div
        [ class "footer" ]
        [ p []
            [ text "Built with love ❤️ on our one and only planet 🌏 :)"
            ]
        , p []
            [ a [ href "https://invariant.club" ] [ text "invariant.club" ]
            , text " — "
            , a [ href "https://github.com/InvariantClub" ] [ text "GitHub" ]
            ]
        ]
