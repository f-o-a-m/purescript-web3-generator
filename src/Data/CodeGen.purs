module Data.CodeGen where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Control.Error.Util (note)
import Effect.Aff (Aff, try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Console (log)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (class MonadState, StateT, evalStateT, get, put)
import Control.Monad.Writer (class MonadTell, runWriterT, tell)
import Data.AbiParser (Abi(..), AbiDecodeError(..), AbiWithErrors, AbiType(..), SolidityFunction(..))
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object)
import Data.Array (catMaybes, concat, foldMap, length, null)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either)
import Data.Foldable (for_)
import Data.Generator (genCode)
import Data.Identity (Identity(..))
import Data.Lens ((^?))
import Data.Array as Array
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (un)
import Data.Map as Map
import Data.String (Pattern(..), Replacement(..), replaceAll, stripPrefix)
import Data.Traversable (for, traverse_)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, writeTextFile, readdir, stat)
import Node.FS.Stats as Stats
import Node.FS.Aff.Mkdirp (mkdirp)
import Node.Path (FilePath, basenameWithoutExt, extname)
import Tidy.Codegen as Gen
import Tidy.Codegen.Monad as TidyM
import Partial.Unsafe (unsafePartial)

type GeneratorOptions =
  { jsonDir :: FilePath
  , pursDir :: FilePath
  , truffle :: Boolean
  , exprPrefix :: String
  , modulePrefix :: String
  }

generatePS :: GeneratorOptions -> Aff ABIErrors
generatePS os = do
  let opts = os { pursDir = os.pursDir <> "/" <> replaceAll (Pattern ".") (Replacement "/") os.modulePrefix }
  fs <- getAllJsonFiles opts.jsonDir
  _ <- mkdirp opts.pursDir
  case fs of
    [] -> throwError <<< error $ "No abi json files found in directory: " <> opts.jsonDir
    fs' -> do
      errs <- join <$> for fs' \f -> do
        let f' = genPSFileName opts f
        Tuple _ errs <- runWriterT $ writeCodeFromAbi opts f f'
        liftEffect $ log
          if null errs then successCheck <> " contract module for " <> f <> " successfully written to " <> f'
          else warningCheck <> " (" <> show (length errs) <> ") contract module for " <> f <> " written to " <> f'
        pure errs
      unless (null errs) do
        liftEffect $ log $ errorCheck <> " got " <> show (length errs) <> " error(s) during generation"
        for_ errs \(ABIError err) ->
          liftEffect $ log $ errorCheck <> " while parsing abi type of object at index: " <> show err.idx <> " from: " <> err.abiPath <> " got error:\n    " <> err.error
      pure errs
  where
  successCheck = withGraphics (foreground Green) $ "✔"
  warningCheck = withGraphics (foreground Yellow) $ "⚠"
  errorCheck = withGraphics (foreground Red) $ "⚠"
  genPSFileName :: GeneratorOptions -> FilePath -> FilePath
  genPSFileName opts fp = opts.pursDir <> "/" <> basenameWithoutExt fp ".json" <> ".purs"

type ABIErrors = Array ABIError

newtype ABIError = ABIError { abiPath :: FilePath, idx :: Int, error :: String }

instance showABIError :: Show ABIError where
  show (ABIError r) = "(ABIError " <> show r <> ")"

generateCodeFromAbi :: GeneratorOptions -> Abi Identity -> FilePath -> String
generateCodeFromAbi opts (Abi abi) destFile = unsafePartial $
  let
    abi' = map Identity $ maybeAnnotateArity $ un Identity <$> abi
    moduleName = opts.modulePrefix <> "." <> basenameWithoutExt destFile ".purs"
    _module = TidyM.codegenModule moduleName $ do
      declarations <- genCode (Abi $ abi') { exprPrefix: opts.exprPrefix }
      traverse_ TidyM.write declarations
  in
    Gen.printModule _module

-- | read in json abi and write the generated code to a destination file
writeCodeFromAbi
  :: forall m
   . MonadAff m
  => MonadTell ABIErrors m
  => GeneratorOptions
  -> FilePath
  -> FilePath
  -> m Unit
writeCodeFromAbi opts abiPath destFile = do
  ejson <- jsonParser <$> liftAff (readTextFile UTF8 abiPath)
  json <- either (liftAff <<< throwError <<< error) pure ejson
  (Abi abiWithErrors) <- either (liftAff <<< throwError <<< error) pure $ parseAbi opts json
  abiUnAnn <- for abiWithErrors case _ of
    Left (AbiDecodeError err) -> do
      tell [ ABIError { abiPath, error: err.error, idx: err.idx } ]
      pure Nothing
    Right res -> pure $ Just $ Identity res
  let code = generateCodeFromAbi opts (Abi $ catMaybes abiUnAnn) destFile
  liftAff $ writeTextFile UTF8 destFile code

maybeAnnotateArity :: Array AbiType -> Array AbiType
maybeAnnotateArity abi =
  let
    Tuple nonFuncAbi funcAbi = foldMap groupingFunc abi
    nameToFunctions = Map.fromFoldableWith (<>) $ funcAbi <#> \fun@(SolidityFunction f) -> Tuple f.name [ fun ]
    functionsWithArity = Array.fromFoldable (Map.values nameToFunctions) >>= \fs -> if length fs > 1 then map go fs else fs
  in
    nonFuncAbi <> map AbiFunction functionsWithArity
  where
  groupingFunc :: AbiType -> Tuple (Array AbiType) (Array SolidityFunction)
  groupingFunc (AbiFunction f) = Tuple [] [ f ]
  groupingFunc a = Tuple [ a ] []

  go :: SolidityFunction -> SolidityFunction
  go (SolidityFunction f) = SolidityFunction f { name = f.name <> show (length f.inputs) }

parseAbi :: forall r. { truffle :: Boolean | r } -> Json -> Either String AbiWithErrors
parseAbi { truffle } abiJson = case truffle of
  false -> lmap printJsonDecodeError $ decodeJson abiJson
  true ->
    let
      mabi = abiJson ^? _Object <<< ix "abi"
    in
      note "truffle artifact missing abi field" mabi >>= \json -> lmap printJsonDecodeError $ decodeJson json

--------------------------------------------------------------------------------
-- | Helpers
--------------------------------------------------------------------------------

-- get all the "valid" directories rooted in a filepath
getAllDirectories
  :: forall m
   . MonadAff m
  => MonadState FilePath m
  => m (Array FilePath)
getAllDirectories = do
  currentDirectory <- get
  allFiles <- liftAff $ readdir currentDirectory
  mdirs <- for allFiles (validateRootedDir currentDirectory)
  pure $ catMaybes mdirs

-- determine whether or not a directory is valid (basically it's not dotted)
validateRootedDir
  :: forall m
   . MonadAff m
  => FilePath -- prefix
  -> FilePath -- dirname
  -> m (Maybe FilePath)
validateRootedDir prefix dir = liftAff $ do
  let fullPath = prefix <> "/" <> dir
  estat <- try $ stat fullPath
  pure case estat of
    Left _ -> Nothing
    Right s ->
      let
        isValid = Stats.isDirectory s && isNothing (stripPrefix (Pattern ".") dir)
      in
        if isValid then Just fullPath
        else Nothing

-- | get all files in a directory with a ".json" extension
getJsonFilesInDirectory
  :: forall m
   . MonadAff m
  => MonadState FilePath m
  => m (Array FilePath)
getJsonFilesInDirectory = do
  currentDirectory <- get
  allFiles <- liftAff $ readdir currentDirectory
  msolcs <- for allFiles (validateFile currentDirectory)
  pure $ catMaybes msolcs

-- | determine whether the file is a .json artifact
validateFile
  :: forall m
   . MonadAff m
  => FilePath -- dir
  -> FilePath -- filepath
  -> m (Maybe FilePath)
validateFile dir f = liftAff $ do
  let fullPath = dir <> "/" <> f
  estat <- try $ stat fullPath
  pure case estat of
    Left _ -> Nothing
    Right s ->
      let
        isValid = Stats.isFile s && extname f == ".json"
      in
        if isValid then Just fullPath
        else Nothing

getAllJsonFiles
  :: forall m
   . MonadAff m
  => FilePath
  -> m (Array FilePath)
getAllJsonFiles root = evalStateT getAllJsonFiles' root
  where
  getAllJsonFiles' :: StateT FilePath m (Array FilePath)
  getAllJsonFiles' = do
    hereFiles <- getJsonFilesInDirectory
    hereDirectories <- getAllDirectories
    if null hereDirectories then pure hereFiles
    else do
      thereFiles <- for hereDirectories $ \d -> do
        put d
        getAllJsonFiles'
      pure $ hereFiles <> concat thereFiles
