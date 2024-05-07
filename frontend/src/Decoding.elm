module Decoding exposing (..)

import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)
import Missing exposing (..)
import Types exposing (..)


decodeFieldDiffOp : Decoder a -> Decoder (FieldDiffOp a)
decodeFieldDiffOp g =
  let
    decide : String -> Decoder (FieldDiffOp a)
    decide x =
      case x of
        "FieldReplaced" ->
            let old = D.index 0 (decodeOld g)
                new = D.index 1 (decodeNew g)
            in
            D.field "contents" <| D.map2 FieldReplaced old new

        "FieldNoOp" ->
            D.field "contents" <| D.map FieldNoOp g

        c ->
            D.fail <| "FieldDiffOp doesn't have constructor: " ++ c
  in
  D.andThen decide (D.field "tag" D.string)


decodeListItemDiffOp : Decoder a -> Decoder (ListItemDiffOp a)
decodeListItemDiffOp g =
  let
    decide : String -> Decoder (ListItemDiffOp a)
    decide x =
      case x of
        "ListItemAdded" ->
          D.field "contents" <| D.map ListItemAdded g

        "ListItemRemoved" ->
          D.field "contents" <| D.map ListItemRemoved g

        "ListItemNoOp" ->
          D.field "contents" <| D.map ListItemNoOp g

        "ListItemReplaced" ->
          let old = D.index 0 (decodeOld g)
              new = D.index 1 (decodeNew g)
          in D.field "contents" <| D.map2 ListItemReplaced old new

        c ->
            D.fail <| "ListItemDiffOp doesn't have constructor: " ++ c

    mkRep lst =
      case lst of
        a :: b :: [] ->
          D.map2 ListItemReplaced (decodeOld a) (decodeNew b)
        _ -> D.fail "Wrong number of parameters for Replace!"
  in
  D.andThen decide (D.field "tag" D.string)


decodeFilter : Decoder Filter
decodeFilter = D.succeed Filter |> required "cond" D.string


decodeAction : Decoder SomeAction
decodeAction =
  let
    decide : String -> Decoder SomeAction
    decide x =
      case x of
        "Select" ->
          D.field "contents" <| D.map Select decodeSelectProps

        "Insert" ->
          D.field "contents" <| D.map Insert decodeInsertProps

        "Update" ->
          D.field "contents" <| D.map Update decodeUpdateProps

        "Delete" ->
          D.succeed Delete

        c ->
          D.fail <| "Action doesn't have constructor: " ++ c
  in
  D.andThen decide (D.field "tag" D.string)


decodeSelectProps : Decoder SelectProps
decodeSelectProps =
  D.succeed SelectProps
    |> required "columns" (D.list (decodeListItemDiffOp D.string))
    |> required "rowLimit" (decodeFieldDiffOp <| D.maybe D.int)
    |> required "allowAggregation" (decodeFieldDiffOp D.bool)


decodeInsertProps : Decoder InsertProps
decodeInsertProps =
  D.succeed InsertProps
    |> required "columns" (D.list <| decodeListItemDiffOp D.string)


decodeUpdateProps : Decoder UpdateProps
decodeUpdateProps =
  D.succeed UpdateProps
    |> required "columns" (D.list <| decodeListItemDiffOp D.string)
    |> required "postFilter" (decodeFieldDiffOp (D.maybe decodeFilter))


decodePermissionAtom : Decoder PermissionAtom
decodePermissionAtom =
  D.succeed PermissionAtom
    |> required "database" (decodeFieldDiffOp D.string)
    |> required "role" (decodeFieldDiffOp D.string)
    |> required "schema" (decodeFieldDiffOp D.string)
    |> required "table" (decodeFieldDiffOp D.string)
    |> required "action" (decodeFieldDiffOp decodeAction)
    |> required "rowFilter" (decodeFieldDiffOp (D.maybe decodeFilter))


decodePermissionWithMetadata : Decoder PermissionWithMetadata
decodePermissionWithMetadata =
  D.succeed PermissionWithMetadata
    |> required "permission" (decodeListItemDiffOp decodePermissionAtom)
    |> required "changed" D.bool
    |> required "ref" D.int


decodePermissionData : Decoder PermissionData
decodePermissionData =
  D.succeed PermissionData
    |> required "version" D.int
    |> required "items" (D.list decodePermissionWithMetadata)
    |> required "mode" decodeMode


decodeMode : Decoder Mode
decodeMode =
  let
    decide : String -> Decoder Mode
    decide x =
      case x of
        "Diff" ->
          let old = D.index 0 (decodeOld decodeSrc)
              new = D.index 1 (decodeNew decodeSrc)
          in D.field "contents" <| D.map2 Diff old new

        "SingleSource" ->
          D.field "contents" <| D.map SingleSource decodeSrc

        c ->
          D.fail <| "Mode doesn't have constructor: " ++ c
  in
  D.andThen decide (D.field "tag" D.string)


decodeSrc : Decoder Src
decodeSrc =
  D.succeed Src
    |> required "label" D.string
    |> required "link" (D.maybe D.string)


decodeNew : Decoder a -> Decoder (New a)
decodeNew d = D.succeed New |> required "runNew" d


decodeOld : Decoder a -> Decoder (Old a)
decodeOld d = D.succeed Old |> required "runOld" d
