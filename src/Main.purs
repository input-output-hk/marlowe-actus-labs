module Main where

import Prelude

import Actus.Domain.ContractTerms (decodeCycle)
import Effect (Effect)
import Effect.Console (log, logShow)


main :: Effect Unit
main = do
  log "decodeCycle:"
  logShow $ decodeCycle "P1ML0"
