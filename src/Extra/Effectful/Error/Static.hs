module Extra.Effectful.Error.Static where

import Effectful (Eff, (:>))
import Effectful.Error.Static (Error)
import Effectful.Error.Static qualified as Error

runEitherEff :: forall l a es. (Error l :> es) => Eff es (Either l a) -> Eff es a
runEitherEff eff = do
  result <- eff
  case result of
    Left err -> Error.throwError err
    Right value -> return value
