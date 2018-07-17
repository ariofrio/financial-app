module StoreDecode exposing (..)

import Json.Decode as J
import Json.Decode.Extra as JE
import Json.Decode.Pipeline as JP
import Time.Date as Date
import Time.Date exposing (Date)

import Store exposing (..)

------------------------------------------------------------------------------
-- MODEL

-- Account

accountFromJson : J.Decoder Account
accountFromJson =
  JP.decode Account
    |> JP.required "id" (J.map wrapAccountId <| JE.maybeNull J.int)
    |> JP.required "name" J.string
    |> JP.required "kind" accountKindFromJson

accountKindFromJson : J.Decoder AccountKind
accountKindFromJson =
  J.
  J.oneOf
    [ JP.decode RealAccount
        |> JP.required "type" "RealAccount" -- what goes here?
    ]
  JP.decode (\f t i -> t i)
    |> JP.required ""
    |> JP.required "info"

literal : Decoder a -> a -> J.Decoder ()
literal decoder value = map (\v ->
    if v == value then

    else
      
  ) decoder

------------------------------------------------------------------------------
-- MESSAGE

messageFromJson : J.Decoder Msg
messageFromJson msg =
