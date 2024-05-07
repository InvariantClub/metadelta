module Types exposing (..)

import Html exposing (Html)
import Missing exposing (..)


type Old a
    = Old a


type New a
    = New a


type alias Src =
    { label : String
    , link : Maybe String
    }


type Mode
    = Diff (Old Src) (New Src)
    | SingleSource Src


type alias PermissionWithMetadata =
    { permission : ListItemDiffOp PermissionAtom
    , changed : Bool
    , ref : Int
    }


type alias PermissionData =
    { version : Int
    , items : List PermissionWithMetadata
    , mode : Mode
    }


type ListItemDiffOp a
    = ListItemNoOp a
    | ListItemAdded a
    | ListItemRemoved a
    | ListItemReplaced (Old a) (New a)


mapListItemDiffOp : (a -> b) -> ListItemDiffOp a -> ListItemDiffOp b
mapListItemDiffOp f o =
    case o of
        ListItemNoOp a ->
            ListItemNoOp (f a)

        ListItemAdded a ->
            ListItemAdded (f a)

        ListItemRemoved a ->
            ListItemRemoved (f a)

        ListItemReplaced a b ->
            ListItemReplaced (mapOld f a) (mapNew f b)


mapOld : (a -> b) -> Old a -> Old b
mapOld f (Old a) =
    Old <| f a


mapNew : (a -> b) -> New a -> New b
mapNew f (New a) =
    New <| f a


extractListItem : ListItemDiffOp a -> a
extractListItem o =
    case o of
        ListItemNoOp a ->
            a

        ListItemAdded a ->
            a

        ListItemRemoved a ->
            a

        ListItemReplaced _ (New a) ->
            a


type FieldDiffOp a
    = FieldNoOp a
    | FieldReplaced (Old a) (New a)


mapFieldDiffOp : (a -> b) -> FieldDiffOp a -> FieldDiffOp b
mapFieldDiffOp f d =
    case d of
        FieldNoOp a ->
            FieldNoOp (f a)

        FieldReplaced (Old a) (New b) ->
            FieldReplaced (Old <| f a) (New <| f b)


extractField : FieldDiffOp a -> a
extractField o =
    case o of
        FieldNoOp a ->
            a

        FieldReplaced _ (New a) ->
            a


type alias PermissionAtom =
    { database : FieldDiffOp Database -- e.g. "default"
    , role : FieldDiffOp Role -- e.g. "staff"
    , schema : FieldDiffOp Schema -- e.g. "public"
    , table : FieldDiffOp Table -- e.g. "plant"
    , action : FieldDiffOp SomeAction -- e.g. Insert
    , rowFilter : FieldDiffOp (Maybe Filter)
    }


type alias ShowPermissionAtom msg =
    { showDatabase : FieldDiffOp Database -> List (Html msg)
    , showRole : FieldDiffOp Role -> List (Html msg)
    , showSchema : FieldDiffOp Schema -> List (Html msg)
    , showTable : FieldDiffOp Table -> List (Html msg)
    , showAction : FieldDiffOp SomeAction -> List (Html msg)
    , showRowFilter : FieldDiffOp (Maybe Filter) -> List (Html msg)
    }


type alias Role =
    String


type alias Table =
    String


type alias Column =
    String


type alias Schema =
    String


type alias Database =
    String


type alias Filter =
    { cond : String
    }


type SomeAction
    = Select SelectProps
    | Update UpdateProps
    | Insert InsertProps
    | Delete



-- Only strings are comparable :(


type alias ComparableAction =
    String


comparableAction : PermissionAtom -> ( ComparableAction, ComparableAction )
comparableAction p =
    case p.action of
        FieldNoOp a ->
            ( "", compareSomeAction a )

        FieldReplaced (Old a) (New b) ->
            ( compareSomeAction a, compareSomeAction b )


compareSomeAction =
    showAction


showAction : SomeAction -> String
showAction a =
    case a of
        Select _ ->
            "Select"

        Update _ ->
            "Update"

        Insert _ ->
            "Insert"

        Delete ->
            "Delete"


getColumns : SomeAction -> List String
getColumns a =
    let
        f cs =
            List.concatMap listItemDiffOpToList cs
    in
    case a of
        Select p ->
            f p.columns

        Insert p ->
            f p.columns

        Update p ->
            f p.columns

        Delete ->
            []


type alias SelectProps =
    { columns : List (ListItemDiffOp Column)
    , rowLimit : FieldDiffOp (Maybe Int)
    , allowAggregation : FieldDiffOp Bool
    }


type alias UpdateProps =
    { columns : List (ListItemDiffOp Column)
    , postFilter : FieldDiffOp (Maybe Filter)
    }


type alias InsertProps =
    { columns : List (ListItemDiffOp Column)
    }


fieldDiffOpToList : FieldDiffOp a -> List a
fieldDiffOpToList f =
    case f of
        FieldNoOp a ->
            [ a ]

        FieldReplaced (Old a) (New b) ->
            [ a, b ]


listItemDiffOpToList : ListItemDiffOp a -> List a
listItemDiffOpToList f =
    case f of
        ListItemReplaced (Old a) (New b) ->
            [ a, b ]

        _ ->
            [ extractListItem f ]
