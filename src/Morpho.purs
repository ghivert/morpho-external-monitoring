module Morpho where

import Prelude

import Control.Parallel (parSequence)
import Data.Array (filter, length)
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.String as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Now as Now
import Fetch (fetch)
import Partial.Unsafe (unsafePartial)

endpoint :: String
endpoint = "https://api.morpho.org"

allEndpoints :: Array String
allEndpoints =
  [ ""
  , "analytics"
  , "analytics/market?protocol=aave-v2&symbol=dai"
  , "metrics"
  , "metrics/markets"
  , "metrics/market?protocol=aave-v2&symbol=dai"
  , "metrics/totalSupply"
  , "rewards/emissions"
  , "rewards/emissions/history"
  , "rewards/emissions/age8"
  , "rewards/distribution/user/0xCbcb6Bd2a1E6085584e323eDAFb5cF9BB8D77E44"
  , "compound/user/0xCbcb6Bd2a1E6085584e323eDAFb5cF9BB8D77E44"
  ]

successMessage :: String
successMessage = "The Morpho API is working well. Have an excellent day, dear you!"

selectMessage :: Array (Maybe String) -> DateTime -> Maybe String
selectMessage results now =
  let failuresKeeped = results # filter Maybe.isJust
      isNotEmpty = length failuresKeeped /= 0 in
    if isNotEmpty then
      Just $ failuresKeeped <#> unsafePartial Maybe.fromJust # String.joinWith ", "
    else
      let hour = fromEnum $ DateTime.hour $ DateTime.time now in
      if hour < 8 || hour > 10 then Nothing else Just successMessage

fetchApi :: String -> Aff (Maybe String)
fetchApi endpt = do
  let url = String.joinWith "/" [ endpoint, endpt ]
  { status } <- fetch url {}
  pure $ if status /= 200 then Just url else Nothing

checkApiUp :: Aff (Maybe String)
checkApiUp = do
  now <- liftEffect Now.nowDateTime
  results <- parSequence $ allEndpoints <#> fetchApi
  pure $ selectMessage results now
