module Store exposing
  ( Model
  , getAccount
  , AccountId -- but not the constructor
  , unwrapActId
  , Account
  , AccountKind(..)
  , RealAccountInfo
  , NominalAccountInfo
  , PlaidAccountConnection
  , getTransaction
  , TransactionId -- but not the constructor
  , unwrapTxId
  , Transaction
  , Post
  , Amount
  , Commodity(..)
  , formatAmount
  , Msg(..)
  , update
  , model
  )

import Maybe exposing (Maybe(..))
import Dict exposing (Dict)
import Time.Date as Date
import Time.Date exposing (Date)

------------------------------------------------------------------------------
-- MODEL

type alias Model =
  { accounts : Dict Int Account
  , transactions : Dict Int Transaction
  , nextAccountId : AccountId
  , nextTransactionId : TransactionId
  }

-- Account

type AccountId = AccountId Int

unwrapActId : AccountId -> Int -- internal
unwrapActId actId =
  case actId of 
    AccountId unwrapped -> unwrapped

getAccount : AccountId -> Model -> Maybe Account
getAccount actId model =
  Dict.get (unwrapActId actId) model.accounts

type alias Account =
  { id : AccountId
  , name : String
  , kind : AccountKind }

type AccountKind
  = RealAccount RealAccountInfo
  | NominalAccount NominalAccountInfo

type alias RealAccountInfo =
  { connection : Maybe PlaidAccountConnection }

type alias NominalAccountInfo = ()

type alias PlaidAccountConnection =
  { itemId : String
  , publicToken : String
  , accessToken : Maybe String -- should never be sent to client
  }

-- Transaction & Post

type TransactionId = TransactionId Int

unwrapTxId : TransactionId -> Int -- internal
unwrapTxId txId =
  case txId of 
    TransactionId unwrapped -> unwrapped

getTransaction : TransactionId -> Model -> Maybe Transaction
getTransaction txId model =
  Dict.get (unwrapTxId txId) model.transactions

type alias Transaction =
  { id : TransactionId
  , date : Date
  , payee : String
  , memo : String
  , posts : List Post
  }

type alias Post =
  { payee : String
  , accountId : Maybe AccountId
  , memo : String
  , amount : Amount
  }

-- Amount

type alias Amount =
  { commodity : Commodity
  , quantity : Int
  }

type Commodity = USD

commodityDecimals : Commodity -> Int
commodityDecimals com =
  case com of
    USD -> 2

formatAmount : Amount -> String
formatAmount amt =
  let
    decimals = commodityDecimals amt.commodity
    multiplier = 10^decimals
    integral = toString (amt.quantity // multiplier)
    fractional = toString (abs (rem amt.quantity multiplier))
  in
    integral ++ "." ++ (String.pad decimals '0' fractional) ++
      " " ++ (toString amt.commodity)

------------------------------------------------------------------------------
-- UPDATE

type Msg
  = CreateAccount          Account
  | CreateTransaction      Transaction
  | DeleteTransaction      TransactionId
  | UpdateTransactionDate  TransactionId Date
  | UpdateTransactionPayee TransactionId String
  | UpdateTransactionMemo  TransactionId String
  | InsertPost             TransactionId Int Post
  | DeletePost             TransactionId Int
  | UpdatePostAccount      TransactionId Int (Maybe AccountId)
  | UpdatePostMemo         TransactionId Int String
  | UpdatePostAmount       TransactionId Int Amount

update : Msg -> Model -> Model
update msg model =
  case msg of
    CreateAccount act ->
      { model | accounts = Dict.insert (unwrapActId act.id) act model.accounts
              , nextAccountId = AccountId ((unwrapActId act.id) + 1)
      }
    CreateTransaction tx ->
      { model | transactions = Dict.insert (unwrapTxId tx.id) tx model.transactions
              , nextTransactionId = TransactionId ((unwrapTxId tx.id) + 1)
      }
    DeleteTransaction id ->
      { model | transactions = Dict.remove (unwrapTxId id) model.transactions }
    UpdateTransactionDate id date ->
      updateTransaction id (\tx -> {tx | date = date}) model
    UpdateTransactionPayee id payee ->
      updateTransaction id (\tx -> {tx | payee = payee}) model
    UpdateTransactionMemo id memo ->
      updateTransaction id (\tx -> {tx | memo = memo}) model
    InsertPost id index post ->
      updateTransaction id (\tx -> {tx | posts =
        List.take index tx.posts ++ [post] ++ List.drop index tx.posts}) model
    DeletePost id index ->
      updateTransaction id (\tx -> {tx | posts =
        List.take index tx.posts ++ List.drop (index + 1) tx.posts}) model
    UpdatePostAccount id index accountId ->
      updatePost id index (\post -> {post | accountId = accountId}) model
    UpdatePostMemo id index memo ->
      updatePost id index (\post -> {post | memo = memo}) model
    UpdatePostAmount id index amount ->
      updatePost id index (\post -> {post | amount = amount}) model

updateTransaction : TransactionId -> (Transaction -> Transaction) -> Model -> Model
updateTransaction id f model =
  { model | transactions = Dict.update (unwrapTxId id) (Maybe.map f) model.transactions }

updatePost : TransactionId -> Int -> (Post -> Post) -> Model -> Model
updatePost id index f model =
  updateTransaction id (\tx -> 
    let
      before = List.take index tx.posts
      post = List.head (List.drop index tx.posts)
      after = List.drop (index + 1) tx.posts
    in
      case post of
        Just p ->
          {tx | posts = before ++ [f p] ++ after}
        Nothing ->
          tx
    ) model

------------------------------------------------------------------------------
-- Test Data

model : Model
model =
  { accounts =
      Dict.fromList
        [ ( 1
          , { id = AccountId 1
            , name = "WF Checking"
            , kind = RealAccount { connection = Nothing }
            }
          )
        , ( 2
          , { id = AccountId 2
            , name = "Restaurants"
            , kind = NominalAccount ()
            }
          )
        ]
  , nextAccountId = AccountId 3
  , transactions =
      Dict.fromList
        [ ( 1
          , { id = TransactionId 1
            , date = Date.date 2017 3 1
            , payee = "Los Arroyos"
            , memo = ""
            , posts =
                [ { payee = ""
                  , accountId = Just (AccountId 2)
                  , memo = ""
                  , amount = Amount USD 1245
                  }
                , { payee = ""
                  , accountId = Just (AccountId 1)
                  , memo = ""
                  , amount = Amount USD (-1245)
                  }
                ]
            }
          )
        ]
  , nextTransactionId = TransactionId 2
  }

