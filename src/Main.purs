module Main (main) where

import Prelude

import Data.Maybe (Maybe(..))
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
      liftEffect $ log $ "Message to send: " <> m
      liftEffect $ log $ "Server response: " <> result

main :: Effect Unit
main = do
  env <- Telegram.readEnv
  case env of
    Just env_ -> launchAff_ $ checkApiAndSendMessage env_
    Nothing -> log "Missing variable in environment"
