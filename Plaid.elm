port module Plaid exposing (request, successes, exits)

import Json.Encode exposing (Value)

port request : () -> Cmd msg
port successes : ((String, Json.Encode.Value) -> msg) -> Sub msg
port exits : ((Maybe String, Json.Encode.Value) -> msg) -> Sub msg
