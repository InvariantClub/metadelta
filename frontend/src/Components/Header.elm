module Components.Header exposing
    ( Header
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
        , h1
        , span
        , text
        )
import Html.Attributes
    exposing
        ( class
        , href
        )
import Route.Path as Path exposing (Path)


type Header msg
    = Settings
        { model : Model
        , toMsg : Msg msg -> msg
        }


new :
    { model : Model
    , toMsg : Msg msg -> msg
    }
    -> Header msg
new props =
    Settings
        { model = props.model
        , toMsg = props.toMsg
        }


type Model
    = Model { activePage : Maybe Path }


init : { activePage : Maybe Path } -> Model
init props =
    Model
        { activePage = props.activePage
        }



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
        (Model model) =
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
                ( Model model, Effect.none )



-- View


view : Header msg -> Html msg
view (Settings settings) =
    let
        (Model model) =
            settings.model

        attrs p =
            if model.activePage == Just p then
                [ class "active" ]

            else
                []
    in
    div
        [ class "banner" ]
        [ h1 []
            [ a [ href "/" ] [ text "Metadelta" ]
            , span [ class "beta" ] [ text "beta" ]
            ]
        , div
            [ class "links" ]
            [ div
                [ class "pages" ]
                [ span (attrs Path.Home_) [ a [ href "/" ] [ text "Home" ] ]
                , span (attrs Path.Explorer) [ a [ href "/explorer" ] [ text "Permission Explorer" ] ]
                , span (attrs Path.New) [ a [ href "/new" ] [ text "New" ] ]
                ]
            , span (attrs Path.Settings) [ a [ href "/settings" ] [ text "Settings" ] ]
            ]
        ]
