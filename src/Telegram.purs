module Telegram
  ( Env
  , sendMessage
  , readEnv
  )
  where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Fetch as Fetch
import Node.Process as Node
import Yoga.JSON as Json

type Env =
  { chatId :: String
  , botToken :: String
  }

endpoint :: String
endpoint = "https://api.telegram.org/bot"

readEnv :: Effect (Maybe Env)
readEnv = do
  chatId_ <- Node.lookupEnv "CHAT_ID"
  botToken_ <- Node.lookupEnv "BOT_TOKEN"
  pure $ case [chatId_, botToken_] of
    [Just chatId, Just botToken] -> Just { chatId, botToken }
    _ -> Nothing

uri :: String -> String -> String
uri method botToken =
  String.joinWith "/" [endpoint <> botToken, method]

createBody :: String -> String -> String
createBody text chatId = Json.writeJSON { chat_id: chatId, text: text }

sendMessage :: Env -> String -> Aff String
sendMessage env message =
  let url = uri "sendMessage" env.botToken
      method = Fetch.POST
      body = createBody message env.chatId
      headers = { "Content-Type": "application/json" } in do
    { text } <- Fetch.fetch url { method, body, headers }
    responseText <- text
    pure responseText
