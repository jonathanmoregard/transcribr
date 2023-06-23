{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor (second)
import Data.Either (Either, either)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Effect.Console qualified as ConsoleE
import Effect.LLM qualified as LLME
import Effectful qualified as IOE
import Effectful.Error.Static (CallStack, prettyCallStack, throwError)
import Effectful.Error.Static qualified as Error
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import OpenAI.Client (OpenAIClient)
import OpenAI.Client qualified as OpenAI
import OpenAI.Resources (EngineId (EngineId))
import Options.Applicative
import Program
import System.Environment (getEnv, lookupEnv)

main :: IO ()
main = do
  opts <- execParser parserInfo
  manager <- newManager tlsManagerSettings
  configKvp <- fmap (second (T.replace "=" "") . T.breakOn "=") . T.lines <$> TIO.readFile "app.config" -- get config key value pairs
  let engineId = EngineId "text-davinci-002" -- EngineId may vary, check OpenAI API docs
  IOE.runEff $ ConsoleE.runConsoleE $ do
    (result :: Either (CallStack, Text) ()) <- Error.runError $ do
      client <- case lookup "OpenAiSecret" configKvp of
        Nothing -> throwError $ T.pack "Could not find OPENAI_KEY in app.config."
        Just apiKey -> pure $ OpenAI.makeOpenAIClient apiKey manager 8
      LLME.runLLMEffect client $ Program.program opts
    case result of
      Right () -> pure ()
      Left (cs, err) -> ConsoleE.print $ "Error: " <> show err <> ", callstack: " <> prettyCallStack cs
  where
    options :: Parser Options
    options =
      Options
        <$> strOption
          ( long "file"
              <> short 'f'
              <> metavar "FILEPATH"
              <> help "File to transcribe"
          )
        <*> optional
          ( strOption
              ( long
                  "language"
                  <> short 'l'
                  <> metavar "LANGUAGE"
                  <> help "Language for transcription"
              )
          )
        <*> switch
          ( long "summarize"
              <> short 's'
              <> help "Summarize the transcribed audio"
          )
    parserInfo :: ParserInfo Options
    parserInfo =
      info
        (options <**> helper)
        ( fullDesc
            <> progDesc "Transcribe the audio from FILE and print it to stdout"
            <> header "audio transcriber - a tool for audio transcription"
        )