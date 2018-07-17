import Html exposing (Html, button, div, text, input, a)
import Html exposing (Html, table, thead, tbody, tr, th, td)
import Html exposing (Html, ul, li)
import Html.Attributes
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onInput, onClick)
import List
import Dict exposing (Dict)
import Time.Date as Date
import Time.Date exposing (Date)
import Maybe.Extra

--import Plaid
import Store

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model = Store.Model

init : (Model, Cmd Msg)
init = (Store.model, Cmd.none)

-- UPDATE

type Msg
  = StoreMsg Store.Msg
  | RequestAddAccount
  | ResponseAddAccount -- TODO

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RequestAddAccount ->
      (model, Cmd.none) --Plaid.addAccount ResponseAddAccount)
    ResponseAddAccount ->
      (model, Cmd.none) -- TODO
    StoreMsg storeMsg ->
      (Store.update storeMsg model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
  --Sub.batch
  --  [ Plaid.successes ,
  --    Plaid.exits  ]

------------------------------------------------------------------------------
-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ externalResources
    , accountsList model
    , transactionsTable model
    ]

accountsList : Model -> Html Msg
accountsList model =
  ul [] <|
    (List.map accountView (Dict.values model.accounts))
    ++
    [ li [] [ button [ onClick RequestAddAccount ] [ text "Link account" ] ]
    , li [] [ button [ onClick <| StoreMsg <| Store.CreateAccount
        { id = model.nextAccountId
        , name = "<New Account>"
        , kind = Store.RealAccount { connection = Nothing }
        } ] [ text "Add manual account" ] ]
    ]

accountView : Store.Account -> Html Msg
accountView act =
  li [] [ a [] [ text act.name ] ]

transactionsTable : Store.Model -> Html Msg
transactionsTable model =
  div []
    [ table [ class "table table-striped table-hover" ]
        [ thead []
          [ tr []
            [ th [] [ text "Date" ]
            , th [] [ text "Account" ]
            , th [] [ text "Payee" ]
            , th [] [ text "Category" ]
            , th [] [ text "Memo" ]
            , th [] [ text "Amount" ]
            ]
          ]
        , tbody []
          (List.map (transactionEditor model) (Dict.values model.transactions))
        ]
    , button
        [ onClick <| StoreMsg <| Store.CreateTransaction <|
            { id = model.nextTransactionId
            , date = Date.date 2017 3 1
              -- TODO: Keep the current date stored in the model for use here.
              -- See https://becoming-functional.com/tasks-in-elm-0-18-2b64a35fd82e
            , payee = ""
            , memo = ""
            , posts =
              [ { payee = "", accountId = Nothing, memo = "", amount = Store.Amount Store.USD 0 }
              , { payee = "", accountId = Nothing, memo = "", amount = Store.Amount Store.USD 0 }
              ]
            }
        ]
        [ text "Add Transaction" ]
    ]

transactionEditor : Store.Model -> Store.Transaction -> Html Msg
transactionEditor model tx =
  let
    getAccountName maybeActId =
      maybeActId
        |> Maybe.andThen (\actId -> Store.getAccount actId model)
        |> Maybe.map .name
        |> Maybe.withDefault ""
  in
    case transactionIsSimple model tx of
      Just ((realPost, realAct), (nominalPost, nominalAct)) ->
        tr []
          [ transactionEditorCell (Date.toISO8601 tx.date) Nothing
          , transactionEditorCell (getAccountName realPost.accountId) Nothing
          , transactionEditorCell tx.payee Nothing
          , transactionEditorCell (getAccountName nominalPost.accountId) Nothing
          , transactionEditorCell tx.memo (Just (Store.UpdateTransactionMemo tx.id))
          , transactionEditorCell (Store.formatAmount realPost.amount) Nothing
          ]
      Nothing ->
        tr []
          [ td [] [ text "TODO" ] ]

transactionEditorCell : String -> (Maybe (String -> Store.Msg)) -> Html Msg
transactionEditorCell val handleInput =
  td []
    [ input
      (
        [ type_ "text"
        , class "form-control"
        , value val
        ] ++ Maybe.Extra.toList
          (Maybe.map (\f -> onInput (f >> StoreMsg)) handleInput)
      )
      []
    ]

transactionIsSimple : Store.Model -> Store.Transaction ->
  Maybe ((Store.Post, Maybe Store.RealAccountInfo), (Store.Post, Maybe Store.NominalAccountInfo))
transactionIsSimple model tx =
  let
    getAccountKind maybeActId =
      maybeActId |> Maybe.andThen (\actId -> Store.getAccount actId model) |> Maybe.map .kind
  in
    case tx.posts of
      p1 :: (p2 :: tail) ->
        if p1.payee == "" && p2.payee == "" && p1.memo == "" && p2.memo == "" then
          case (getAccountKind p1.accountId, getAccountKind p2.accountId) of
            (Just (Store.RealAccount rea), Just (Store.NominalAccount nom)) ->
              Just ((p1, Just rea), (p2, Just nom))
            (Just (Store.NominalAccount nom), Just (Store.RealAccount rea)) ->
              Just ((p2, Just rea), (p1, Just nom))
            (Nothing, Just (Store.NominalAccount nom)) -> Just ((p1, Nothing), (p2, Just nom))
            (Just (Store.NominalAccount nom), Nothing) -> Just ((p1, Nothing), (p2, Just nom))
            (Nothing, Just (Store.RealAccount rea))    -> Just ((p1, Just rea), (p2, Nothing))
            (Just (Store.RealAccount rea), Nothing)    -> Just ((p1, Just rea), (p2, Nothing))
            (Nothing, Nothing)                         -> Just ((p1, Nothing), (p2, Nothing))
            _ ->
              Nothing
        else
          Nothing
      _ ->
        Nothing

-- External Resources

externalResources : Html msg
externalResources =
  div []
    [ stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
    , stylesheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap-theme.min.css"
    , script "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"
    , script "https://cdn.plaid.com/link/v2/stable/link-initialize.js"
    ]

script : String -> Html msg
script src =
  Html.node "script" [ Html.Attributes.src src ] []

stylesheet : String -> Html msg
stylesheet href =
  Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href href ] []
