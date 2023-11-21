module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
import Dotenv (loadFile) as Dotenv
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Morpho as Morpho
import Telegram as Telegram

checkApiAndSendMessage :: Telegram.Env -> Aff Unit
checkApiAndSendMessage env = do
  message <- Morpho.checkApiUp
  case message of
    Nothing -> pure unit
    Just m -> do
      result <- Telegram.sendMessage env m
      liftEffect do
        log $ "Message to send: " <> m
        log $ "Server response: " <> result

main :: Effect Unit
main = launchAff_ do
  Dotenv.loadFile
  env <- liftEffect Telegram.readEnv
  case env of
    Nothing -> liftEffect $ log "Missing variable in environment."
    Just env_ -> do
      liftEffect $ log "Starting querying Morpho API…"
      checkApiAndSendMessage env_
