module PermissionViews exposing (..)

import Html
    exposing
        ( Html
        , button
        , div
        , h1
        , h2
        , h3
        , h4
        , p
        , pre
        , small
        , span
        , text
        )
import Html.Attributes exposing (class, title)
import Json.Print exposing (prettyString)
import Maybe exposing (map, withDefault)
import Missing exposing (..)
import Types
    exposing
        ( FieldDiffOp(..)
        , Filter
        , ListItemDiffOp(..)
        , New(..)
        , Old(..)
        , ShowPermissionAtom
        , SomeAction(..)
        , mapFieldDiffOp
        , showAction
        )


defaultShowPermissionAtom : ShowPermissionAtom msg
defaultShowPermissionAtom =
    { showDatabase = viewDiffSimple "database"
    , showRole = viewDiffSimple "role"
    , showSchema = viewDiffSimple "schema"
    , showTable = viewDiffSimple "table"
    , showAction = viewDiffAction
    , showRowFilter = viewDiffFilter "pre-filter"
    }


viewSimple : String -> String -> String -> Html msg
viewSimple label extraClass role =
    div [ class <| "operation " ++ extraClass ]
        [ prefix label <| div [] [ text role ]
        ]


viewDiffSimple : String -> FieldDiffOp String -> List (Html msg)
viewDiffSimple label df =
    let
        elts =
            case df of
                FieldNoOp f ->
                    div [ class "op" ] [ text f ]

                FieldReplaced (Old a) (New b) ->
                    div [ class "op" ]
                        [ span
                            [ class "simple-text diff-item-removed"
                            , title "This is the old value, now changed."
                            ]
                            [ text a ]
                        , text "→"
                        , span
                            [ class "simple-text diff-item-added"
                            , title "This is the new value, added in these changes."
                            ]
                            [ text b ]
                        ]
    in
    [ prefix label <| elts ]


viewDiffFilter : String -> FieldDiffOp (Maybe Filter) -> List (Html msg)
viewDiffFilter label df =
    case df of
        FieldNoOp f ->
            viewFilter label "" f ""

        FieldReplaced (Old a) (New b) ->
            viewFilter label "operation-removed" a "Filted removed"
                ++ viewFilter label "operation-added" b "Filter added"


viewFilter : String -> String -> Maybe Filter -> String -> List (Html msg)
viewFilter label extraClass f t =
    case map .cond f of
        Just cond ->
            let
                config =
                    { indent = 2, columns = 200 }

                pretty =
                    Result.withDefault "~ error ~" <| prettyString config cond
            in
            [ div [ class <| "operation " ++ extraClass, title t ]
                [ prefix label <| pre [ class "row-filter" ] [ text pretty ] ]
            ]

        Nothing ->
            []


prefix : String -> Html msg -> Html msg
prefix p e =
    div
        [ class "permission-item" ]
        [ span
            [ class "prefix" ]
            [ span [ class "actual-text" ] [ text p ] ]
        , e
        ]


viewColumns : List (ListItemDiffOp String) -> List (Html msg)
viewColumns columns =
    let
        mkElt item cls pre t =
            span [ class <| "sql-column " ++ cls, title t ] [ text <| pre ++ item ]

        mkCol dc =
            case dc of
                ListItemReplaced (Old a) (New b) ->
                    [ mkElt a "diff-item-removed" "-" "Column removed"
                    , mkElt b "diff-item-added" "+" "Column added"
                    ]

                ListItemNoOp a ->
                    [ mkElt a "" "" "" ]

                ListItemAdded a ->
                    [ mkElt a "diff-item-added" "+" "Column added" ]

                ListItemRemoved a ->
                    [ mkElt a "diff-item-removed" "-" "Column removed" ]
    in
    List.concatMap mkCol columns


viewAction : SomeAction -> String -> List (Html msg)
viewAction a extraClass =
    let
        mkElt n =
            span [ class <| "op " ++ n ] [ text n ]

        mkThing opts =
            [ mkElt <| showAction a
            , div [ class "sql-columns" ] <| viewColumns opts.columns
            ]

        elts =
            case a of
                Delete ->
                    [ mkElt "Delete" ]

                Insert opts ->
                    mkThing opts

                Update opts ->
                    mkThing opts

                Select opts ->
                    mkThing opts

        basicOp =
            prefix "operation" <| div [ class "op" ] elts

        otherOps =
            case a of
                Update opts ->
                    viewDiffFilter "post-filter" opts.postFilter

                Select opts ->
                    let
                        bt b =
                            if b then
                                "Yes"
                            else
                                "No"

                        rl m =
                            case m of
                                Nothing ->
                                    "None"

                                Just n ->
                                    String.fromInt n

                        aggs =
                            viewDiffSimple "aggregations" (mapFieldDiffOp bt opts.allowAggregation)

                        lims =
                            viewDiffSimple "row limit   " (mapFieldDiffOp rl opts.rowLimit)
                    in
                    aggs ++ lims

                _ ->
                    []

        single =
            div [ class <| "operation " ++ extraClass ] <| basicOp :: otherOps
    in
    [ single ]


viewDiffAction : FieldDiffOp SomeAction -> List (Html msg)
viewDiffAction da =
    case da of
        FieldNoOp a ->
            viewAction a ""

        FieldReplaced (Old a) (New b) ->
            viewAction a "operation-removed"
                ++ viewAction b "operation-added"
