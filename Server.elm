port module Server exposing (..)

import Store
import WebSocketServer

-- MAIN

type alias Flags =
  { actions : List Store.Msg
  }

main : Program Flags Model Msg
main =
  Platform.programWithFlags
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { clients : List WebSocketServer.Socket
  , actions : List Store.Msg
  }

init : Flags -> (Model, Cmd Msg)
init flags =
  ( { actions = flags.actions }
  , Cmd.none
  )

-- UPDATE

type Msg
  = ClientConnected 
  | PerformAction Store.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ClientConnected
    PerformAction action ->
      ( {model | actions = action :: model.actions }
      , List.map (\ws -> WebSocketServer.send ws action) model.clients
        -- only send to clients for the same user, and make sure they are authenticated
      )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  WebSocketServer.listen 
  Sub.none

-- PORTS


