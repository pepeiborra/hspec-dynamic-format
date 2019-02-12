{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Test.Hspec.Dynamic.Format
  ( hspec
  , hspecWith
  , specdyn
  , module Test.Hspec
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Free
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Char                       (isSpace)
import           Data.Generics.Labels ()
import           Data.List
import           Data.Map.Strict                 (Map)
import qualified Data.Map.Strict                 as Map
import           Data.Maybe
import           Data.String
import           GHC.Generics
import           GHC.IO.Handle
import           System.IO                       as IO
import           System.IO.Silently
import           Test.Hspec hiding (hspec)
import           Test.Hspec.Core.Format
import           Test.Hspec.Core.Formatters.Diff
import           Test.Hspec.Core.Runner (Config, hspecCustomFormatters, hspecWithCustomFormatters)
import           Test.Hspec.Core.Spec (Spec)
import           Test.Hspec.Dynamic.IO
import           Test.Hspec.Dynamic.TextBlock

hspec :: Spec -> IO ()
hspec = hspecCustomFormatters [("dynamic", specdyn)]

hspecWith :: Config -> Spec -> IO ()
hspecWith = hspecWithCustomFormatters [("dynamic", specdyn)]

---------------------------------------------------------------------------

data MyState = MyState
  { successCount :: Int
  , pendingCount :: Int
  , blocks       :: TextBlocks
  , failMessages :: [FailureRecord]
  , running      :: Map Path TextBlockHandle
  , groups       :: Map [String] TextBlockHandle
  }
  deriving (Generic)

initialState :: MyState
initialState = MyState 0 0 mempty mempty mempty mempty

data FailureRecord = FailureRecord {
  _failureRecordLocation :: Maybe Location
, _failureRecordPath     :: Path
, _failureRecordMessage  :: FailureReason
}

type M = StateT MyState (Free TextF)
type TextBlockHandle = Int

writeTextBlock :: TextBlock () -> M TextBlockHandle
writeTextBlock tb = do
  blocks <- use #blocks
  let (blocks', ix) = appendTextBlock tb blocks
  #blocks .= blocks'
  void $ lift$ renderTextBlock tb
  return ix

insertTextBlock :: TextBlockHandle -> TextBlock () -> M TextBlockHandle
insertTextBlock l tb = do
  blocks <- use #blocks
  let (blocks', ix) = insertTextBlockAt l tb blocks
  #blocks .= blocks'
  lift $ reRenderTextBlocks blocks blocks'
  return ix

rewriteTextBlock
  :: TextBlockHandle -> (TextBlock () -> Maybe (TextBlock ())) -> M ()
rewriteTextBlock l f = do
  blocks <- use #blocks
  blocks' <- #blocks <%= modifyTextBlock l f
  lift $ reRenderTextBlocks blocks blocks'

specdyn :: FormatConfig -> Format M
specdyn cfg@FormatConfig{..} = Format{..}
  where
  formatAsynchronously = True
  formatRun it = do
    h' <- hDuplicate formatConfigHandle
    let run = mapStateT (runTextIO h' cfg . fmap return)
    (output, res) <-
      hCapture [IO.stdout, IO.stderr] $
      evalStateT (run (it) `finally` run failedFormatter) initialState
    hClose h'
    unless (null output) $ do
      putStrLn "PROGRAM OUTPUT:"
      putStrLn output
    return res

  formatGroupStarted (nesting, name) = do
    h <- writeTextBlock $ fromString(indentationFor nesting ++ name)
    #groups %= Map.insert (nesting ++ [name]) h

  formatGroupDone _ = return ()

  formatProgress ex@(group, _) p = do
    (current,total) <- case p of
      Started x -> do
        gg <- use #groups
        l  <- case Map.lookup group gg of
          Nothing -> writeTextBlock $ fromString $ renderExampleLine ex
          Just gl -> insertTextBlock gl $ fromString $ renderExampleLine ex
        #running %= Map.insert ex l
        #groups %= Map.insert group l
        return x
      Progress x -> return x

    unless (current == 0 && total == 0) $ do
      running <- use #running
      let prevLine = Map.lookup ex running
      case prevLine of
        Nothing -> error "exampleProgress" -- Progress cannot arise before Started
        Just l  -> do
          let formattedProgress
                | current == 0 && total == 0 = ""
                | total == 0 = show current
                | otherwise  = show current ++ "/" ++ show total
          rewriteTextBlock l $ const $ Just $ fromString $ renderExampleLine ex ++ ' ': formattedProgress

  formatItem ex@(nesting,_) Item{itemResult = Success, ..} = do
    #successCount += 1
    prevLine :: Maybe TextBlockHandle <- #running.at(ex) <<.= Nothing
    let output = case prevLine of
          Just l  -> rewriteTextBlock l . const . Just
          Nothing -> void . writeTextBlock
    output $ do
      line $ withSuccessColor (renderExampleLine ex)
      forM_ (lines itemInfo) $ \s ->
        lineS $ indentationFor ("" : nesting) ++ s

  formatItem ex@(nesting,_) Item{itemResult = Failure reason, ..} = do
    failMessages <- #failMessages <%= (FailureRecord itemLocation ex reason :)
    let n = length failMessages
    prevLine <- #running.at(ex) <<.= Nothing
    let output = case prevLine of
          Just l  -> rewriteTextBlock l . const . Just
          Nothing -> void . writeTextBlock
    output $ do
      line $ withFailColor $ renderExampleLine ex ++ " FAILED [" ++ show n ++ "]"
      forM_ (lines itemInfo) $ \ s ->
        lineS $ indentationFor ("" : nesting) ++ s

  formatItem ex@(nesting,_) Item{itemResult = Pending reason, ..} = do
    #pendingCount += 1
    prevLine <- #running.at(ex) <<.= Nothing
    let output = case prevLine of
          Just l  -> rewriteTextBlock l . const . Just
          Nothing -> void . writeTextBlock
    output $ do
      line $ withPendingColor $ renderExampleLine ex
      forM_ (lines itemInfo) $ \ s ->
        lineS $ indentationFor ("" : nesting) ++ s
      lineS $ indentationFor ("" : nesting) ++ "# PENDING: " ++ fromMaybe "No reason given" reason

  failedFormatter = do
    failures <- use #failMessages
    unless (null failures) $ void $ writeTextBlock $ do
        line ""
        line "failures:"
        line ""

        forM_ (zip [1::Int ..] failures) $ \x -> do
          formatFailure formatConfigUseDiff x
          line ""
        line $ fromString $ "Randomized with seed " ++ show formatConfigUsedSeed
        line ""

  formatFailure :: Bool -> (Int, FailureRecord) -> TextBlock ()
  formatFailure doDiff (n, FailureRecord mLoc path reason) = do
    forM_ mLoc $ \loc -> line $ withInfoColor $ formatLoc loc
    lineS $ "  " ++ show n ++ ") " ++ formatRequirement path
    case reason of
      NoReason -> return ()
      Reason err -> withFailColor $ indent err
      ExpectedButGot preface expected actual -> do
        mapM_ lineS $ fmap indent preface
        let chunks
              | doDiff = diff expected actual
              | otherwise = [First expected, Second actual]

        withFailColor $ indentation ++ "expected: "
        forM_ chunks $ \chunk -> case chunk of
          Both a _ -> indented PlainStyle a
          First a  -> indented DiffExtraStyle a
          Second _ -> return ()
        line ""

        withFailColor $ indentation ++ " but got: "
        forM_ chunks $ \chunk -> case chunk of
          Both a _ -> indented PlainStyle a
          First _  -> return ()
          Second a -> indented DiffMissingStyle a
        line ""
        where
          indented :: Style -> String -> TextBlock ()
          indented style text = case break (== '\n') text of
            (xs, "") -> withStyle style xs
            (xs, _ : ys) -> do
              line $ withStyle style xs
              fromString (indentation ++ "          ")
              indented style ys
      Error _ e -> withFailColor . indent $ (("uncaught exception: " ++) . formatException) e

    line ""
    line $ fromString ("  To rerun use: --match " ++ show (joinPath path))
    where
      indentation = "       "
      indent message = unlines $ map (indentation ++) (lines message)
      formatLoc (Location file l column) = "  " ++ file ++ ":" ++ show l ++ ":" ++ show column ++ ": "

  indentationFor nesting = replicate (length nesting * 2) ' '
  renderExampleLine (nesting, requirement) = indentationFor nesting ++ requirement

-- |
-- Join a `Path` with slashes.  The result will have a leading and a trailing
-- slash.
joinPath :: Path -> String
joinPath (groups, requirement) = "/" ++ intercalate "/" (groups ++ [requirement]) ++ "/"

formatException :: SomeException -> String
formatException = show

-- |
-- Try to create a proper English sentence from a path by applying some
-- heuristics.
formatRequirement :: Path -> String
formatRequirement (groups, requirement) = groups_ ++ requirement
  where
    groups_ = case break (any isSpace) groups of
      ([], ys) -> join ys
      (xs, ys) -> join (intercalate "." xs : ys)

    join xs = case xs of
      [x] -> x ++ " "
      ys  -> concatMap (++ ", ") ys
