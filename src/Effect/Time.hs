{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effect.Time where

import Control.Exception (IOException)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Error.Static (Error, throwError)

data TimeEffect :: Effectful.Effect where
  Now :: TimeEffect m UTCTime

type instance DispatchOf TimeEffect = 'Dynamic

now :: (TimeEffect :> es) => Eff es UTCTime
now = send Now

runTimeEffect ::
  (IOE :> es) =>
  Eff (TimeEffect : es) a ->
  Eff es a
runTimeEffect = interpret $ \_ -> \case
  Now -> liftIO getCurrentTime
