{-# language MultiWayIf #-}
{-# language TypeFamilies #-}
{-# language RecordWildCards #-}

module Main ( main ) where

import           Relude                        as Prelude ( force )
import           Control.Comonad                ( extract )
import qualified Control.Exception             as Exception
import           GHC.Err                        ( errorWithoutStackTrace )
import           Control.Monad.Free
import           Control.Monad.Ref              ( MonadRef(readRef) )
import           Control.Monad.Catch
import           System.IO                      ( hPutStrLn )
import qualified Data.HashMap.Lazy             as M
import qualified Data.Map                      as Map
import           Data.Time
import qualified Data.Text.IO                  as Text
import           Text.Show.Pretty               ( ppShow )
import           Nix                     hiding ( force )
import           Nix.Convert
import           Nix.Json
import           Nix.Options.Parser
import           Nix.Standard
import           Nix.Thunk.Basic
import           Nix.Type.Env                   ( Env(..) )
import           Nix.Type.Type                  ( Scheme )
import qualified Nix.Type.Infer                as HM
import qualified Nix.TS.Infer                  as TS
import qualified Nix.TS.Monad                  as TS
import           Nix.Value.Monad
import           Options.Applicative     hiding ( ParserResult(..) )
import           Prettyprinter           hiding ( list )
import           Prettyprinter.Render.Text      ( renderIO )
import qualified Repl
import           Nix.Eval
import qualified Debug.Trace
import qualified Data.Text as T
import Data.Fix (foldFix, hoistFix)

main :: IO ()
main =
  do
    currentTime <- getCurrentTime
    opts <- execParser $ nixOptionsInfo currentTime

    main' opts

main' :: Options -> IO ()
main' opts@Options{..} = runWithBasicEffectsIO opts execContentsFilesOrRepl
 where
  --  2021-07-15: NOTE: This logic should be weaved stronger through CLI options logic (OptParse-Applicative code)
  -- As this logic is not stated in the CLI documentation, for example. So user has no knowledge of these.
  execContentsFilesOrRepl :: StdIO
  execContentsFilesOrRepl =
    fromMaybe
      loadFromCliFilePathList
      $ loadBinaryCacheFile <|>
        loadLiteralExpression <|>
        loadExpressionFromFile
   where
    -- | The base case: read expressions from the last CLI directive (@[FILE]@) listed on the command line.
    loadFromCliFilePathList :: StdIO
    loadFromCliFilePathList =
      case getFilePaths of
        []     -> runRepl
        ["-"]  -> readExpressionFromStdin
        _paths -> processSeveralFiles (coerce _paths)
     where
      -- | Fall back to running the REPL
      runRepl = error "disabled"

      readExpressionFromStdin =
        processExpr =<< liftIO Text.getContents

    processSeveralFiles :: [Path] -> StdIO
    processSeveralFiles = traverse_ processFile
     where
      processFile path = handleResult =<< parseNixFileLoc path

    -- |  The `--read` option: load expression from a serialized file.
    loadBinaryCacheFile :: Maybe StdIO
    loadBinaryCacheFile =
      (\ (binaryCacheFile :: Path) ->
        do
          let file = replaceExtension binaryCacheFile "nixc"
          processCLIOptions (pure file) =<< liftIO (readCache binaryCacheFile)
      ) <$> getReadFrom

    -- | The `--expr` option: read expression from the argument string
    loadLiteralExpression :: Maybe StdIO
    loadLiteralExpression = processExpr <$> getExpression

    -- | The `--file` argument: read expressions from the files listed in the argument file
    loadExpressionFromFile :: Maybe StdIO
    loadExpressionFromFile =
      -- We can start use Text as in the base case, requires changing Path -> Text
      -- But that is a gradual process:
      -- https://github.com/haskell-nix/hnix/issues/912
      (processSeveralFiles . (coerce . toString <$>) . lines <=< liftIO) .
        (\case
          "-" -> Text.getContents
          _fp -> readFile _fp
        ) <$> getFromFile

doIt :: Text -> IO ()
doIt src = do
  now <- getCurrentTime
  runWithBasicEffectsIO (defaultOptions now) $ processExpr src

processExpr :: Text -> StdIO
processExpr = handleResult . parseNixTextLoc

--  2021-07-15: NOTE: @handleResult@ & @process@ - have atrocious size & compexity, they need to be decomposed & refactored.
handleResult =
  either
    (\ err -> errorWithoutStackTrace $ "Parse failed: " <> show err
    )

    (\expr -> do
      putStrLn $ "BEFORE reduceExpr: " <> show (stripAnnotation expr)
      expr' <- liftIO $ reduceExpr Nothing expr
      putStrLn $ "AFTER reduceExpr: " <> show (stripAnnotation expr')
      either
        (\ err -> errorWithoutStackTrace $ "Type error: " <> T.unpack err)
        (liftIO . putStrLn . (<>) "Type of expression: " . ppShow
        )
        (TS.runTypeM $ TS.infer expr')

        -- liftIO $ putStrLn $ runST $
        --     runLintM opts . renderSymbolic =<< lint opts expr
    )

--  2021-07-15: NOTE: Logic of CLI Option processing is scattered over several functions, needs to be consolicated.
processCLIOptions :: Maybe Path -> NExprLoc -> StdIO
processCLIOptions mpath expr = error "disabled"

reduction path mpathToContext annExpr =
  do
    eres <-
      withNixContext
        mpathToContext
        $ reducingEvalExpr
            evalContent
            mpathToContext
            annExpr
    handleReduced path eres

handleReduced
  :: (MonadThrow m, MonadIO m)
  => Path
  -> (NExprLoc, Either SomeException (NValue t f m))
  -> m (NValue t f m)
handleReduced (coerce -> path) (expr', eres) =
  do
    liftIO $
      do
        putStrLn $ "Wrote sifted expression tree to " <> path
        writeFile path $ show $ prettyNix $ stripAnnotation expr'
    either throwM pure eres
