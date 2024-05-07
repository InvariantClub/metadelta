module Shared.Model exposing (LocalSettings, Model)

import Types exposing (PermissionData)


type alias Model =
    { localSettings : LocalSettings
    , permissionData : Maybe PermissionData
    }


type alias LocalSettings =
    { githubToken : Maybe String
    }
