port module Pages.Explorer exposing (Model, Msg, page)

import Components.Footer as Footer
import Components.Header as Header
import Decoding exposing (..)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Extras exposing (..)
import Grouping exposing (..)
import Html
    exposing
        ( Html
        , a
        , b
        , button
        , code
        , details
        , div
        , em
        , h1
        , h2
        , h3
        , h4
        , hr
        , input
        , label
        , li
        , p
        , pre
        , progress
        , small
        , span
        , summary
        , text
        , textarea
        , ul
        )
import Html.Attributes as A
    exposing
        ( checked
        , class
        , disabled
        , for
        , height
        , href
        , id
        , name
        , title
        , type_
        , width
        )
import Html.Events exposing (onClick, onInput)
import Http exposing (Error(..))
import Json.Decode exposing (decodeString)
import Missing exposing (..)
import Page exposing (Page)
import PermissionViews
    exposing
        ( defaultShowPermissionAtom
        )
import Regex as R
import Route exposing (Route)
import Route.Path as Path
import Set exposing (Set)
import Shared
import Types
    exposing
        ( Column
        , ComparableAction
        , FieldDiffOp(..)
        , ListItemDiffOp(..)
        , Mode(..)
        , New(..)
        , Old(..)
        , PermissionAtom
        , PermissionData
        , PermissionWithMetadata
        , Role
        , ShowPermissionAtom
        , SomeAction(..)
        , Src
        , Table
        , comparableAction
        , extractListItem
        , fieldDiffOpToList
        , getColumns
        , listItemDiffOpToList
        , mapListItemDiffOp
        , showAction
        )
import View exposing (View)
import Web exposing (loadJson)


type alias ArtifactInfo =
    { artifact_id : String
    , owner : String
    , repo : String
    , token : String
    }


port downloadGithubArtifact : ArtifactInfo -> Cmd msg


port receiveUnzippedJson : (String -> msg) -> Sub msg


port notifyError : (String -> msg) -> Sub msg


port regexFilterUpdate : ({ field : String, regex : String } -> msg) -> Sub msg


port onlyChangedUpdate : (Bool -> msg) -> Sub msg


port showFilterWindow : () -> Cmd msg


mkRegex : String -> R.Regex
mkRegex =
    Maybe.withDefault R.never << R.fromString


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    let
        wrap s =
            JsonLoaded <| liftError <| decodeString decodePermissionData s

        liftError : Result Json.Decode.Error a -> Result Http.Error a
        liftError e =
            case e of
                Err m ->
                    Err <| BadBody <| Json.Decode.errorToString m

                Ok s ->
                    Ok s

        mkRegexEvent fv =
            let
                ev =
                    case fv.field of
                        "Database" ->
                            SomeFilteringEvent <| Regex DatabaseField (fieldDiffOpToList << .database) fv.regex

                        "Schema" ->
                            SomeFilteringEvent <| Regex SchemaField (fieldDiffOpToList << .schema) fv.regex

                        "Table" ->
                            SomeFilteringEvent <| Regex TableField (fieldDiffOpToList << .table) fv.regex

                        "Role" ->
                            SomeFilteringEvent <| Regex RoleField (fieldDiffOpToList << .role) fv.regex

                        "Column" ->
                            SomeFilteringEvent <| Regex ColumnField (List.concatMap getColumns << fieldDiffOpToList << .action) fv.regex

                        _ ->
                            NoOp
            in
            ev
    in
    Page.new
        { init = init shared route
        , update = update route
        , subscriptions =
            const <|
                Sub.batch
                    [ notifyError NotifyJavascriptError
                    , receiveUnzippedJson wrap
                    , onlyChangedUpdate (\_ -> SomeFilteringEvent ToggleOnlyChanged)
                    , regexFilterUpdate mkRegexEvent
                    ]
        , view = view
        }


type GroupOption
    = ByOperation
    | ByRole
    | ByTable


type alias Model =
    { groupLevel1 : GroupOption
    , groupLevel2 : GroupOption
    , originalPermissions : List PermissionWithMetadata
    , viewablePermissions : List PermissionWithMetadata
    , viewedItems : Set Int
    , fatalError : Maybe (Html Msg)
    , mode : Maybe Mode
    , filters : Filters
    , permissionData : Maybe PermissionData
    , header : Header.Model
    , footer : Footer.Model
    , shared : Shared.Model
    , loading : Bool
    }


tokenError : Html msg
tokenError =
    p []
        [ text """You appear to be trying to get an artifact from GitHub, but
          you don't have a GitHub token set in local storage. Head over
          to """
        , a [ href "/settings" ] [ text "the settings page" ]
        , text " to specify your GitHub personal access token."
        ]


urlDecodeError : Html msg
urlDecodeError =
    p []
        [ text """We attempted to decode the url to find
  information so we can download a GitHub artifact; we failed."""
        , text " We expected the "
        , span [ class "tt" ] [ text "?githubArtifact" ]
        , text " parameter to be like "
        , span [ class "tt" ] [ text "?githubArtifact=<artifact_id>/<owner>/<repo>" ]
        , text "."
        ]


needUrlError : Html msg
needUrlError =
    p []
        [ text "The "
        , code [] [ text "src" ]
        , text " parameter is blank. Please input the URL of a "
        , text "publically-accessible metadelta json file."
        ]


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared route () =
    let
        ( eff, fatalError ) =
            case getSource route of
                Just "" ->
                    ( Effect.none, Just needUrlError )

                Just url ->
                    ( Effect.sendCmd <| loadJson url JsonLoaded decodePermissionData, Nothing )

                Nothing ->
                    case ( shared.localSettings.githubToken, getGithubArtifact route ) of
                        --
                        -- 1. No token but they are trying to get a GitHub asset. Error.
                        ( Nothing, Just _ ) ->
                            ( Effect.none, Just tokenError )

                        --
                        -- 2. Token, and an artifact url; we're good!
                        ( Just token, Just artifactStr ) ->
                            case String.split "/" artifactStr of
                                [ id, owner, repo ] ->
                                    let
                                        info =
                                            { artifact_id = id
                                            , repo = repo
                                            , owner = owner
                                            , token = token
                                            }
                                    in
                                    ( Effect.batch
                                        [ Effect.sendMsg ToggleLoading
                                        , Effect.sendCmd (downloadGithubArtifact info)
                                        ]
                                    , Nothing
                                    )

                                _ ->
                                    ( Effect.none, Just urlDecodeError )

                        --
                        -- 3. No url provided; that's fine!
                        _ ->
                            ( Effect.none, Nothing )
    in
    ( { groupLevel1 = ByOperation
      , groupLevel2 = ByRole
      , originalPermissions = []
      , viewablePermissions = []
      , viewedItems = Set.empty
      , fatalError = fatalError
      , mode = Nothing
      , filters = emptyFilters
      , permissionData = shared.permissionData
      , header = Header.init { activePage = Just Path.Explorer }
      , footer = Footer.init ()
      , shared = shared
      , loading = False
      }
    , Effect.batch
        [ Effect.sendCmd <| showFilterWindow ()
        , eff
        , Effect.sendMsg ViewPermissionData
        ]
    )


type alias Filters =
    { onlyChanged : Bool
    , tableMatches : Maybe RegexMatcher
    , roleMatches : Maybe RegexMatcher
    , columnMatches : Maybe RegexMatcher
    , schemaMatches : Maybe RegexMatcher
    , databaseMatches : Maybe RegexMatcher
    }


type RegexMatcher
    = RegexMatcher (PermissionAtom -> List String) String


emptyFilters : Filters
emptyFilters =
    { onlyChanged = True
    , tableMatches = Nothing
    , roleMatches = Nothing
    , columnMatches = Nothing
    , schemaMatches = Nothing
    , databaseMatches = Nothing
    }


getSource : Route () -> Maybe String
getSource route =
    Dict.get "src" route.query


getGithubArtifact : Route () -> Maybe String
getGithubArtifact route =
    Dict.get "githubArtifact" route.query



-- UPDATE


type Msg
    = NoOp
    | FooterEvent (Footer.Msg Msg)
    | HeaderEvent (Header.Msg Msg)
    | JsonLoaded (Result Http.Error PermissionData)
    | NotifyJavascriptError String
    | SetGroupLevel1 GroupOption
    | SetGroupLevel2 GroupOption
    | SomeFilteringEvent FilteringEvent
    | ToggleLoading
    | ToggleViewed Int
    | ViewPermissionData


type FilteringEvent
    = Regex RegexField (PermissionAtom -> List String) String
    | ToggleOnlyChanged


type RegexField
    = TableField
    | RoleField
    | ColumnField
    | SchemaField
    | DatabaseField


errorString : Http.Error -> Html Msg
errorString e =
    let
        show m =
            p [] [ text m ]

        errorElt =
            case e of
                BadBody s ->
                    div []
                        [ p [] [ text "Bad content" ]
                        , pre [] [ text s ]
                        ]

                BadStatus i ->
                    show <| "Bad status: " ++ String.fromInt i

                BadUrl s ->
                    show <| "Bad url: " ++ s

                NetworkError ->
                    show <| "Network error"

                Timeout ->
                    show <| "Timeout"
    in
    errorElt


runFiltering : Model -> Model
runFiltering model =
    let
        fs =
            model.filters

        ps0 =
            if fs.onlyChanged then
                List.filter .changed model.originalPermissions

            else
                model.originalPermissions

        filterRegex : List PermissionWithMetadata -> RegexMatcher -> List PermissionWithMetadata
        filterRegex pms (RegexMatcher selector expr) =
            let
                r =
                    mkRegex expr
            in
            List.filter (isMatch selector r << .permission) pms

        all : List RegexMatcher
        all =
            catMaybes
                [ fs.tableMatches
                , fs.roleMatches
                , fs.columnMatches
                , fs.schemaMatches
                , fs.databaseMatches
                ]

        ps =
            List.foldl (flip filterRegex) ps0 all
    in
    { model | viewablePermissions = ps }


isMatch : (PermissionAtom -> List String) -> R.Regex -> ListItemDiffOp PermissionAtom -> Bool
isMatch f r dp =
    let
        items =
            List.concatMap f (listItemDiffOpToList dp)
    in
    List.foldl (||) False (List.map (R.contains r) items)


updateFilters : FilteringEvent -> Model -> ( Model, Effect Msg )
updateFilters f model =
    case f of
        ToggleOnlyChanged ->
            let
                fs =
                    model.filters

                newFilter =
                    { fs | onlyChanged = not model.filters.onlyChanged }
            in
            ( runFiltering { model | filters = newFilter }, Effect.none )

        Regex field selector regexString ->
            let
                fs =
                    model.filters

                ms =
                    if regexString == "" then
                        Nothing

                    else
                        Just (RegexMatcher selector regexString)

                newFilter =
                    case field of
                        TableField ->
                            { fs | tableMatches = ms }

                        RoleField ->
                            { fs | roleMatches = ms }

                        ColumnField ->
                            { fs | columnMatches = ms }

                        SchemaField ->
                            { fs | schemaMatches = ms }

                        DatabaseField ->
                            { fs | databaseMatches = ms }
            in
            ( runFiltering { model | filters = newFilter }, Effect.none )


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        NotifyJavascriptError e ->
            let
                fe =
                    p [] [ text e ]
            in
            ( { model | fatalError = Just fe }, Effect.none )

        HeaderEvent innerMsg ->
            ( model, Effect.none )

        FooterEvent innerMsg ->
            ( model, Effect.none )

        ViewPermissionData ->
            let
                fs =
                    model.filters

                newFilters =
                    { fs | onlyChanged = isDiffMode newModel }

                newModel =
                    case model.permissionData of
                        Just pd ->
                            { model
                                | originalPermissions = pd.items
                                , viewablePermissions = pd.items
                                , mode = Just pd.mode
                            }

                        Nothing ->
                            model
            in
            ( runFiltering { newModel | filters = newFilters }, Effect.none )

        SomeFilteringEvent f ->
            updateFilters f model

        JsonLoaded r ->
            let
                ( newModel, eff ) =
                    case r of
                        Err e ->
                            ( { model | fatalError = Just <| errorString e }
                            , Effect.none
                            )

                        Ok pd ->
                            ( { model | permissionData = Just pd }
                            , Effect.sendMsg ViewPermissionData
                            )
            in
            ( { newModel | loading = False }
            , eff
            )

        SetGroupLevel1 op ->
            ( { model | groupLevel1 = op }
            , Effect.none
            )

        SetGroupLevel2 op ->
            ( { model | groupLevel2 = op }
            , Effect.none
            )

        ToggleViewed id ->
            let
                s =
                    if Set.member id model.viewedItems then
                        Set.remove id model.viewedItems

                    else
                        Set.insert id model.viewedItems
            in
            ( { model | viewedItems = s }
            , Effect.none
            )

        ToggleLoading ->
            ( { model | loading = not model.loading }
            , Effect.none
            )



-- VIEW


view : Model -> View Msg
view model =
    { title = "Permission explorer | Metadelta"
    , body =
        [ div [ id "top" ] []
        , Header.new
            { model = model.header
            , toMsg = HeaderEvent
            }
            |> Header.view
        ]
            ++ (case model.fatalError of
                    Just e ->
                        [ h3 [] [ text "Oops!" ]
                        , div [ class "content error" ]
                            [ b [] [ text "We encountered a problem ... 🙂" ]
                            , e
                            ]
                        ]

                    Nothing ->
                        body model
               )
    }


hasPermissions : Model -> Bool
hasPermissions model =
    List.length model.originalPermissions > 0


isDiffMode : Model -> Bool
isDiffMode model =
    case model.mode of
        Just (Diff _ _) ->
            True

        _ ->
            False


infoHeader : Model -> Html Msg
infoHeader model =
    let
        mkElt cls src =
            case src.link of
                Nothing ->
                    span [ class <| "src " ++ cls ] [ text src.label ]

                Just lnk ->
                    a [ href lnk ] [ span [ class <| "src " ++ cls ] [ text src.label ] ]
    in
    case model.mode of
        Nothing ->
            div [] []

        Just (SingleSource src) ->
            div
                [ class "content" ]
                [ p []
                    [ text "You are viewing the permissions of "
                    , mkElt "solo" src
                    , text "."
                    ]
                ]

        Just (Diff (Old o) (New n)) ->
            div
                [ class "content" ]
                [ p []
                    [ text "The changes needed to convert "
                    , mkElt "old" o
                    , text " into "
                    , mkElt "new" n
                    , text "."
                    ]
                ]


body : Model -> List (Html Msg)
body model =
    let
        ( cls1, cls2 ) =
            if model.loading then
                ( " hidden", "" )

            else
                ( "", " hidden" )

        cls1_ =
            if not (hasPermissions model) then
                " hidden"

            else
                ""

        noPermissions =
            div []
                [ h3 [] [ text "No permissions to view ..." ]
                , div [ class "content" ]
                    [ p []
                        [ text "Head on over to the "
                        , a [ href "/new" ] [ text "New" ]
                        , text " tab to load some in."
                        ]
                    ]
                ]

        html =
            [ div [ class <| "most-things" ++ cls1_ ]
                [ h3 [] [ text "Information" ]
                , infoHeader model
                , h3 [] [ text "Permissions" ]
                , groupingWidget model
                , showPermissions model
                ]
            , if not model.loading && isDiffMode model then
                showLegend

              else
                text ""
            , if not (hasPermissions model) then
                noPermissions

              else
                text ""
            , div [ class <| "loading" ++ cls2 ] [ span [ class "loader" ] [] ]
            , Footer.new
                { model = model.footer
                , toMsg = FooterEvent
                }
                |> Footer.view
            , status model
            ]
    in
    html


status : Model -> Html Msg
status model =
    let
        fpC =
            List.length model.originalPermissions

        foundPermissions =
            String.fromInt fpC

        vC =
            Set.size <| model.viewedItems

        viewed =
            String.fromInt vC

        filteredC =
            fpC - List.length model.viewablePermissions

        filtered =
            String.fromInt filteredC

        pct =
            String.fromFloat <| (toFloat vC / toFloat (fpC - filteredC)) * 100

        finished =
            vC == (fpC - filteredC)

        extraClass =
            if finished then
                "wiggle"

            else
                ""
    in
    div
        [ class <| "status " ++ extraClass ]
        [ div [ class "text" ]
            [ text <| foundPermissions ++ " permissions,"
            , text " "
            , text <| viewed ++ " viewed,"
            , text " "
            , text <| filtered ++ " hidden by filters."
            ]
        , div [ class "progress" ]
            [ span [ class "progress", A.style "width" (pct ++ "%") ] [ text "" ]
            ]
        ]


showLegend : Html Msg
showLegend =
    div [ class "legend" ]
        [ h3 [] [ text "Legend" ]
        , div
            [ class "legend-items" ]
            [ div [ class "operation operation-added" ] [ text "Field added" ]
            , div [ class "operation operation-removed" ] [ text "Field removed" ]
            , div [ class "sp operation" ] [ div [ class "indicator diff-item-added" ] [ text "" ], span [ class "t" ] [ text "Permission added" ] ]
            , div [ class "sp operation" ] [ div [ class "indicator diff-item-removed" ] [ text "" ], span [ class "t" ] [ text "Permission removed" ] ]
            , div [ class "i" ] [ span [ class "sql-column diff-item-added" ] [ text "+Column added" ] ]
            , div [ class "i" ] [ span [ class "sql-column diff-item-removed" ] [ text "-Column removed" ] ]
            , div [ class "i" ]
                [ span [ class "simple-text diff-item-removed" ] [ text "Old" ]
                , text "→"
                , span [ class "simple-text diff-item-added" ] [ text "New" ]
                ]
            , div [ class "i" ] [ span [ class "id-num changed" ] [ text "n" ], text " Permission changed" ]
            ]
        ]



{- In Elm this is the best we can do; really these are all 'comparables';
   but elm doesn't understand that, so we need to return a String instead.
-}


opToSelector : GroupOption -> (PermissionAtom -> ( String, String ))
opToSelector op =
    let
        ed : FieldDiffOp String -> ( String, String )
        ed df =
            case df of
                FieldNoOp a ->
                    ( "", a )

                FieldReplaced (Old a) (New b) ->
                    ( a, b )
    in
    case op of
        ByOperation ->
            comparableAction

        -- TODO: This is only on the _table_ not the schema+table, which is what
        -- it really needs to be. I.e. this will be confused if we see a table
        -- with the same name but a different schema; but it's only for grouping
        -- I think, so its fine.
        ByTable ->
            ed << .table

        ByRole ->
            ed << .role


opToShowFs : GroupOption -> ShowPermissionAtom msg -> ShowPermissionAtom msg
opToShowFs op s =
    case op of
        ByOperation ->
            s

        ByTable ->
            { s | showTable = const [] }

        ByRole ->
            { s | showRole = const [] }


showPermissions : Model -> Html Msg
showPermissions model =
    let
        s1 =
            opToSelector model.groupLevel1 << extractListItem << .permission

        s2 =
            opToSelector model.groupLevel2 << extractListItem << .permission

        showFs =
            List.foldl opToShowFs defaultShowPermissionAtom [ model.groupLevel1, model.groupLevel2 ]

        grouped =
            groupToMapBy2 s1 s2

        result =
            if model.groupLevel1 == model.groupLevel2 then
                div [ class "content diff-groups" ]
                    [ em [] [ text "Please select different groups when grouping." ] ]

            else
                show2 model showFs <| grouped model.viewablePermissions
    in
    result


nameFor : GroupOption -> String
nameFor op =
    case op of
        ByOperation ->
            "Operation"

        ByTable ->
            "Table"

        ByRole ->
            "Role"


groupingWidget : Model -> Html Msg
groupingWidget model =
    let
        -- Note: Apparently better to let the first one be always-clickable.
        b1 =
            b SetGroupLevel1 .groupLevel1 Nothing

        b2 =
            b SetGroupLevel2 .groupLevel2 (Just .groupLevel1)

        b msg f mg op =
            let
                c =
                    if f model == op then
                        [ class "grouping selected" ]

                    else
                        []

                d =
                    flip Maybe.map mg <|
                        \g ->
                            if g model == op then
                                [ class "grouping", disabled True ]

                            else
                                []

                attrs =
                    [ onClick (msg op) ] ++ c ++ Maybe.withDefault [] d
            in
            button attrs [ text <| nameFor op ]
    in
    div
        [ class "content" ]
        [ div
            [ id "groupingWidget" ]
            [ div [ class "column" ]
                [ text "Group by "
                , b1 ByOperation
                , b1 ByRole
                , text " or "
                , b1 ByTable
                ]
            , div [ class "column" ]
                [ text " and then by"
                , b2 ByOperation
                , b2 ByRole
                , text " or "
                , b2 ByTable
                ]
            ]
        ]


show2 :
    Model
    -> ShowPermissionAtom Msg
    -> Group2 ( String, String ) ( String, String ) PermissionWithMetadata
    -> Html Msg
show2 model showFs group =
    let
        elts tup =
            case tup of
                ( "", a ) ->
                    [ span [] [ text a ] ]

                ( a, b ) ->
                    [ span [ class "group-header diff-item-removed" ] [ text a ]
                    , text "→"
                    , span [ class "group-header diff-item-added" ] [ text b ]
                    ]

        labelFor tup =
            case tup of
                ( "", a ) ->
                    a

                ( a, b ) ->
                    a ++ "-" ++ b

        f ( tup, v ) =
            div [ class "level-1" ]
                [ details
                    [ A.attribute "open" "" ]
                    [ summary []
                        [ small [ class "label" ] [ text <| nameFor model.groupLevel1 ]
                        , h2 [] <| elts tup

                        -- No anchor for now; it's broken by hash-routing anyway.
                        -- a [ A.name <| nameFor model.groupLevel1
                        --              , class "anchor"
                        --              , href <| "#" ++ labelFor tup
                        --              ] [ text "§" ], text " " ] ++
                        -- elts tup
                        ]
                    , div [] <| List.map g (toSortedList v)
                    ]
                ]

        g ( tup, v ) =
            div [ class "level-2" ]
                [ small [ class "label" ] [ text <| nameFor model.groupLevel2 ]
                , h3 [] <| elts tup
                , div [ class "all-permissions" ] <| List.map (showPermission model.viewedItems showFs) v
                ]

        m =
            List.map f (toSortedList group)
    in
    Html.div [] m


showPermission :
    Set Int
    -> ShowPermissionAtom Msg
    -> PermissionWithMetadata
    -> Html Msg
showPermission viewedItems showFs pwm =
    -- Note: We _never_ get a 'replacement' here; this is due to line 40 in
    -- Hasa/Diff.hs; basically, we don't consider a replacement here; we only
    -- consider deeper differences.
    --
    -- So in this sense, it's completely fine to manually extract the item and
    -- also determine the class like so.
    let
        p =
            extractListItem pwm.permission

        ( diffClass, theTitle ) =
            case pwm.permission of
                ListItemAdded _ ->
                    ( "diff-item-added", "This permission has been added" )

                ListItemRemoved _ ->
                    ( "diff-item-removed", "This permission has been removed" )

                _ ->
                    ( "", "" )

        elts =
            showFs.showDatabase p.database
                ++ showFs.showSchema p.schema
                ++ showFs.showTable p.table
                ++ showFs.showRole p.role
                ++ showFs.showAction p.action
                ++ showFs.showRowFilter p.rowFilter

        idRef =
            String.fromInt pwm.ref

        isViewed =
            Set.member pwm.ref viewedItems

        viewedClass =
            if isViewed then
                "viewed"

            else
                ""

        changedClass =
            if pwm.changed then
                "changed"

            else
                ""
    in
    div [ class <| "permission-atom " ++ viewedClass, title theTitle ] <|
        [ div [ class <| "indicator " ++ diffClass ] [ text " " ]
        , div [ class "permission-content" ] <|
            [ span [ class <| "id-num " ++ changedClass ] [ text idRef ]
            , div [ class <| "cdots " ++ viewedClass ] [ text "⋯" ]
            , div [ class <| "content " ++ viewedClass ] elts
            ]
        , div [ class "tick-box" ]
            [ input
                [ type_ "checkbox"
                , id <| "id-" ++ idRef
                , onClick <| ToggleViewed pwm.ref
                , checked isViewed
                ]
                []
            , label [ for <| "id-" ++ idRef ] [ text "Viewed" ]
            ]
        ]
