module StoreEncode exposing (..)

import Json.Encode as J
import Json.Encode.Extra as JE
import Time.Date as Date
import Time.Date exposing (Date)

import Store exposing (..)

------------------------------------------------------------------------------
-- MODEL

-- Account

accountToJson : Account -> J.Value
accountToJson act = J.object
  [ ("id", accountIdToJson act.id)
  , ("name", J.string act.name)
  , ("kind", accountKindToJson act.kind)
  ]

accountIdToJson : AccountId -> J.Value
accountIdToJson id = J.int <| unwrapActId id

accountKindToJson : AccountKind -> J.Value
accountKindToJson actKind =
  case actKind of
    RealAccount info -> J.object
      [ ("type", J.string "RealAccount")
      , ("info", realAccountInfoToJson info)
      ]
    NominalAccount info -> J.object
      [ ("type", J.string "NominalAccount")
      , ("info", nominalAccountInfoToJson info)
      ]

nominalAccountInfoToJson : NominalAccountInfo -> J.Value
nominalAccountInfoToJson info = J.object []

realAccountInfoToJson : RealAccountInfo -> J.Value
realAccountInfoToJson info = J.object
  [ ("connection", Maybe.map connectionToJson info.connection |> Maybe.withDefault J.null) ]

connectionToJson : PlaidAccountConnection -> J.Value
connectionToJson conn = J.object
  [ ("itemId", J.string conn.itemId)
  , ("publicToken", J.string conn.publicToken)
  , ("accessToken", JE.maybe J.string conn.accessToken)
      -- TODO: should never be sent to client
  ]

-- Transaction

transactionToJson : Transaction -> J.Value
transactionToJson tx = J.object
  [ ("id", transactionIdToJson tx.id)
  , ("date", dateToJson tx.date)
  , ("payee", J.string tx.payee)
  , ("memo", J.string tx.memo)
  , ("posts", J.list <| List.map postToJson tx.posts)
  ]

transactionIdToJson : TransactionId -> J.Value
transactionIdToJson id = J.int <| unwrapTxId id

postToJson : Post -> J.Value
postToJson post = J.object
  [ ("payee", J.string post.payee)
  , ("accountId", JE.maybe accountIdToJson post.accountId)
  , ("memo", J.string post.memo)
  , ("amount", amountToJson post.amount)
  ]

amountToJson : Amount -> J.Value
amountToJson amt = J.list
  [ commodityToJson amt.commodity
  , J.int amt.quantity
  ]

commodityToJson : Commodity -> J.Value
commodityToJson com =
  case com of
    USD -> J.string "USD"

-- Other

dateToJson : Date -> J.Value
dateToJson date = J.string <| Date.toISO8601 date

------------------------------------------------------------------------------
-- MESSAGE

messageToJson : Msg -> J.Value
messageToJson msg =
  case msg of
    CreateAccount act -> J.object
      [ ("type", J.string "CreateAccount")
      , ("account", accountToJson act)
      ]
    CreateTransaction tx -> J.object
      [ ("type", J.string "CreateTransaction")
      , ("transaction", transactionToJson tx)
      ]
    DeleteTransaction id -> J.object
      [ ("type", J.string "DeleteTransaction")
      , ("transactionId", transactionIdToJson id)
      ]
    UpdateTransactionDate id date -> J.object
      [ ("type", J.string "UpdateTransactionDate")
      , ("transactionId", transactionIdToJson id)
      , ("date", dateToJson date)
      ]
    UpdateTransactionPayee id payee -> J.object
      [ ("type", J.string "UpdateTransactionPayee")
      , ("transactionId", transactionIdToJson id)
      , ("payee", J.string payee)
      ]
    UpdateTransactionMemo id memo -> J.object
      [ ("type", J.string "UpdateTransactionMemo")
      , ("transactionId", transactionIdToJson id)
      , ("memo", J.string memo)
      ]
    InsertPost id index post -> J.object
      [ ("type", J.string "InsertPost")
      , ("transactionId", transactionIdToJson id)
      , ("postIndex", J.int index)
      , ("post", postToJson post)
      ]
    DeletePost id index -> J.object
      [ ("type", J.string "DeletePost")
      , ("transactionId", transactionIdToJson id)
      , ("postIndex", J.int index)
      ]
    UpdatePostAccount id index accountId -> J.object
      [ ("type", J.string "UpdatePostAccount")
      , ("transactionId", transactionIdToJson id)
      , ("postIndex", J.int index)
      , ("accountId", JE.maybe accountIdToJson accountId)
      ]
    UpdatePostMemo id index memo -> J.object
      [ ("type", J.string "UpdatePostMemo")
      , ("transactionId", transactionIdToJson id)
      , ("postIndex", J.int index)
      , ("memo", J.string memo)
      ]
    UpdatePostAmount id index amount -> J.object
      [ ("type", J.string "UpdatePostAmount")
      , ("transactionId", transactionIdToJson id)
      , ("postIndex", J.int index)
      , ("amount", amountToJson amount)
      ]
