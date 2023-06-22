module Program where

import Control.Monad (foldM, foldM_)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Effect.Console (ConsoleEffect)
import Effect.Console qualified as ConsoleE
import Effect.LLM (LLMEffect, Language)
import Effect.LLM qualified as LLME
import Effectful (Eff, (:>))

data Options = Options
  { optFile :: FilePath,
    optLanguage :: Maybe Language
  }

program :: (ConsoleEffect :> es, LLMEffect :> es) => Options -> Eff es ()
program opt = do
  transcription <- LLME.transcribeAudio opt.optFile opt.optLanguage
  ConsoleE.print transcription
  pure ()