module Shared.Msg exposing (Msg(..))

import Types exposing (PermissionData)

type Msg
    = NoOp
    | UpdatePermissionData (Maybe PermissionData)
