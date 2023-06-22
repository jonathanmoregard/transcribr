{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effect.LLM (LLMEffect, Language, completeText, transcribeAudio, runLLMEffect) where

import Control.Exception (IOException)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.Either (Either, either)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Effectful (Dispatch (..), DispatchOf, Eff, Effect, IOE, MonadIO (liftIO), (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Error.Static (Error, throwError)
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Client.TLS qualified as HTTPClient
import OpenAI.Client (AudioTranscriptionRequest (..), CompletionCreate (ccrMaxTokens), CompletionResponse, EmbeddingCreate (..), OpenAIClient, TextCompletionCreate (tccrMaxTokens))
import OpenAI.Client qualified as OpenAI
import OpenAI.Resources
  ( TextCompletion (tcChoices),
    TextCompletionChoice (tccText),
  )
import System.Environment (getEnv, lookupEnv)
import UnliftIO (catch)

data LLMEffect :: Effect where
  CompleteText :: Text -> LLMEffect m Text
  TranscribeAudio :: FilePath -> Maybe Language -> LLMEffect m Text

type instance DispatchOf LLMEffect = 'Dynamic

completeText :: LLMEffect :> es => Text -> Eff es Text
completeText prompt = send $ CompleteText prompt

transcribeAudio :: LLMEffect :> es => FilePath -> Maybe Language -> Eff es Text
transcribeAudio file language = send $ TranscribeAudio file language

newtype Language = Language {unLanguage :: Text}
  deriving (Eq, Show, IsString)

runLLMEffect ::
  (IOE :> es, Error Text :> es) =>
  OpenAIClient ->
  Eff (LLMEffect : es) a ->
  Eff es a
runLLMEffect client = interpret $ \_ -> \case
  CompleteText prompt -> do
    result <- adapt $ OpenAI.completeText client (OpenAI.defaultCompletionCreate (OpenAI.ModelId "gpt-3.5-turbo") prompt) {ccrMaxTokens = Just 1000}
    case result of
      Left err -> throwError $ "Client error:" <> T.pack (show err)
      Right response -> do
        let choices = OpenAI.crChoices response
        if null choices
          then throwError $ T.pack "No choices in response"
          else pure $ T.dropWhile (== '\n') $ OpenAI.cchText (head choices)
  TranscribeAudio filePath mLanguage -> do
    result <- adapt $ OpenAI.createTranscription client $ defaultAudioTranscriptionRequest filePath mLanguage
    case result of
      Left err -> throwError $ "Client error:" <> T.pack (show err)
      Right response -> pure $ OpenAI.audrdText response
  where
    adapt :: (IOE :> es, Error Text :> es) => IO a -> Eff es a
    adapt m = liftIO m `catch` \(e :: IOException) -> throwError $ T.pack (show e)
    defaultAudioTranscriptionRequest :: FilePath -> Maybe Language -> AudioTranscriptionRequest
    defaultAudioTranscriptionRequest file language =
      AudioTranscriptionRequest
        { audtsrFile = file,
          audtsrModel = OpenAI.ModelId "whisper-1",
          audtsrPrompt = Nothing,
          audtsrResponseFormat = Nothing,
          audtsrTemperature = Nothing,
          audtsrLanguage = unLanguage <$> language
        }