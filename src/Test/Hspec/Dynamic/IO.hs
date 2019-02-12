{-# LANGUAGE RecordWildCards #-}
module Test.Hspec.Dynamic.IO (runTextIO) where

import           Control.Monad.Catch          (MonadMask)
import           Control.Monad.Free
import           Control.Monad.IO.Class
import           Data.Char                    (isSpace)
import           GHC.IO.Handle
import           System.Console.ANSI
import qualified System.IO                    as IO
import           Test.Hspec.Dynamic.TextBlock
import           Test.Hspec.Core.Format (FormatConfig(..))

runTextIO :: (MonadIO m, MonadMask m) => Handle -> FormatConfig -> Free TextF (m b) -> m b
runTextIO h config = iter (textAlg $ renderEnv h config)

renderEnv, renderText, renderHtml :: (MonadIO m, MonadMask m) => Handle -> FormatConfig -> TextEnvironment m
renderEnv h cfg
  | formatConfigHtmlOutput cfg = renderHtml h cfg
  | otherwise = renderText h cfg

renderHtml h _ = TextEnvironment{..}
  where
    envClearFromCursorToEnd = return ()
    envClearLine = return ()
    envMoveCursorUp _ = return ()
    envRenderTextSpan tb
      | Just cls <- styleToClass (tsStyle tb)
      = liftIO $ IO.hPutStrLn h $ "<span class=\"" ++ cls ++ "\">" ++ tsText tb ++ "</span>"
      | otherwise
      = liftIO $ IO.hPutStrLn h $ tsText tb
    styleToClass InfoStyle        = Just "hspec-info"
    styleToClass FailureStyle     = Just "hspec-failure"
    styleToClass PendingStyle     = Just "hspec-pending"
    styleToClass SuccessStyle     = Just "hspec-success"
    styleToClass DiffExtraStyle   = Just "hspec-failure"
    styleToClass DiffMissingStyle = Just "hspec-success"
    styleToClass _                = Nothing

renderText h config = TextEnvironment{..}
  where
    envRenderTextSpan tb = liftIO $ do
      let sgr = styleToSGR config (tsStyle tb) (tsText tb)
          codes =
            setSGRCode' sgr ++
            tsText tb ++
            resetSGRCode sgr
      IO.hPutStr h codes

    setSGRCode' []    = []
    setSGRCode' other = setSGRCode other

    resetSGRCode [] = []
    resetSGRCode _  = setSGRCode [Reset]

    envMoveCursorUp n = liftIO $
      case n of
        0 -> return ()
        _ | n>0 -> IO.hPutStr h $ cursorUpLineCode n
        _ -> IO.hPutStr h $ cursorDownLineCode (-n)

    envClearFromCursorToEnd = liftIO $ hClearFromCursorToScreenEnd h

    envClearLine = liftIO $ hClearLine h

    styleToSGR :: FormatConfig -> Style -> String -> [SGR]
    styleToSGR cfg InfoStyle        _ | formatConfigUseColor cfg = [SetColor Foreground Dull Cyan]
    styleToSGR cfg FailureStyle     _ | formatConfigUseColor cfg = [SetColor Foreground Dull Red]
    styleToSGR cfg SuccessStyle     _ | formatConfigUseColor cfg = [SetColor Foreground Dull Green]
    styleToSGR cfg PendingStyle     _ | formatConfigUseColor cfg = [SetColor Foreground Dull Yellow]
    styleToSGR cfg DiffMissingStyle t | formatConfigUseColor cfg && formatConfigUseDiff cfg = diffColorize Green t
    styleToSGR cfg DiffExtraStyle   t | formatConfigUseColor cfg && formatConfigUseDiff cfg = diffColorize Red t
    styleToSGR _   _ _ = []

    diffColorize :: Color -> String-> [SGR]
    diffColorize color s = [SetColor layer Dull color]
      where
        layer
          | all isSpace s = Background
          | otherwise = Foreground
