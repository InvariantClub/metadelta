module Web exposing (..)

import Http exposing (Error(..), Response(..))
import Json.Decode exposing (Decoder)


loadJson :
    String
    -> (Result Http.Error a -> msg)
    -> Decoder a
    -> Cmd msg
loadJson path mkMsg decoder =
    Http.get
        { url = path
        , expect = Http.expectJson mkMsg decoder
        }
