module Data.CodeGen where

import Prelude

import Ansi.Codes (Color(..))
import Ansi.Output (withGraphics, foreground)
import Control.Error.Util (note)
import Control.Monad.Aff (Aff, try)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (class MonadState, StateT, State, evalStateT, evalState, get, put, modify)
import Control.Monad.Writer (class MonadTell, runWriter, runWriterT, tell)
import Data.AbiParser (Abi(..), AbiDecodeError(..), AbiWithErrors, AbiType(..), SolidityFunction(..))
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object)
import Data.Array (catMaybes, concat, length, nub, null, sort)
import Data.Either (Either(..), either)
import Data.Foldable (foldl, for_)
import Data.Generator (Imports, ModuleImport(..), ModuleImports, ModuleName, Imported, genCode, mkComment, newLine1)
import Data.Identity (Identity(..))
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Map (Map, fromFoldableWith, insert, lookup, member, toAscUnfoldable)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (mempty)
import Data.Record.Extra (showRecord)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, stripPrefix)
import Data.Set as S
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile, readdir, stat)
import Node.FS.Stats as Stats
import Node.FS.Sync.Mkdirp (mkdirp)
import Node.Path (FilePath, basenameWithoutExt, extname)


type GeneratorOptions =
  { jsonDir :: FilePath
  , pursDir :: FilePath
  , truffle :: Boolean
  , exprPrefix :: String
  , modulePrefix :: String
  }

data IsCtrInImports = CtrIsInImports | CtrIsNotInImports
type ModuleImportsAcc = { types :: Map ModuleName IsCtrInImports, imports :: Array String }

runImported :: GeneratorOptions -> FilePath -> Imported String -> String
runImported opts destFile c =
  let (Tuple code accImports) = runWriter c
  in
    genPSModuleStatement opts destFile
      <> if code == ""
        then ""
        else "\n" <> runImports accImports <> "\n" <> code

runImports :: Imports -> String
runImports = mergeImports >>> map runImport >>> newLine1 >>> ("import Prelude \n\n" <> _)
  where
    runImport :: Tuple ModuleName ModuleImports -> String
    runImport (Tuple mName mImports) = "import " <> mName <> " (" <> joinWith ", " (runModuleImports mImports) <> ")"
    runModuleImports :: ModuleImports -> Array String
    runModuleImports =
      runAcc <<< foldl f { types: mempty, imports: mempty }
      where
      runAcc :: ModuleImportsAcc -> Array String
      runAcc acc = sort $ nub $ append acc.imports $ (toAscUnfoldable acc.types) >>= resolveCtrImports
      resolveCtrImports :: Tuple String IsCtrInImports -> Array String
      resolveCtrImports (Tuple typeName isCtrInImports) = case isCtrInImports of
        CtrIsInImports -> []
        CtrIsNotInImports -> [typeName]
      f :: ModuleImportsAcc -> ModuleImport -> ModuleImportsAcc
      f acc = case _ of
        IType a ->
          if member a acc.types
            then acc
            else acc{ types = insert a CtrIsNotInImports acc.types}
        ITypeCtr a ->
          case lookup a acc.types of
            Nothing ->
              { types: insert a CtrIsInImports acc.types, imports: acc.imports <> [ a <> "(..)" ]}
            Just CtrIsInImports ->
              acc
            Just CtrIsNotInImports ->
              { types: insert a CtrIsInImports acc.types, imports: acc.imports <> [ a <> "(..)" ]}
        ITypeOp a ->
          acc {imports = acc.imports <> [ "type (" <> a <> ")" ]}
        IClass a ->
          acc {imports = acc.imports <> [ "class " <> a ]}
        IVal a ->
          acc {imports = acc.imports <> [ a ]}
        IOp a ->
          acc {imports = acc.imports <> [ "(" <> a <> ")" ]}

    -- NOTE this also sorts modules as we use toAscUnfoldable
    mergeImports :: Imports -> Imports
    mergeImports = fromFoldableWith append >>> toAscUnfoldable

generatePS :: forall e . GeneratorOptions -> Aff (fs :: FS, console :: CONSOLE | e) ABIErrors
generatePS os = do
  let opts = os { pursDir = os.pursDir <> "/" <> replaceAll (Pattern ".") (Replacement "/") os.modulePrefix }
  fs <- getAllJsonFiles opts.jsonDir
  liftEff $ mkdirp opts.pursDir
  case fs of
    [] -> throwError <<< error $ "No abi json files found in directory: " <> opts.jsonDir
    fs' -> do
      errs <- join <$> for fs' \f -> do
        let f' = genPSFileName opts f
        Tuple _ errs <- runWriterT $ writeCodeFromAbi opts f f'
        log if null errs
          then successCheck <> " contract module for " <> f <> " successfully written to " <> f'
          else warningCheck <> " (" <> show (length errs) <> ") contract module for " <> f <> " written to " <> f'
        pure errs
      unless (null errs) do
        log $ errorCheck <> " got " <> show (length errs) <> " error(s) during generation"
        for_ errs \(ABIError err) ->
          log $ errorCheck <> " while parsing abi type of object at index: " <> show err.idx <> " from: " <> err.abiPath <> " got error:\n    " <> err.error
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
  show (ABIError r) = "(ABIError " <> showRecord r <> ")"

-- | read in json abi and write the generated code to a destination file
writeCodeFromAbi :: forall e m
  . MonadAff (fs :: FS, console :: CONSOLE | e) m
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
      Right res -> pure $ Just $ Identity $ res
    let abi = map Identity $ maybeAnnotateArity $ map (\(Identity a) -> a) $ catMaybes (abiUnAnn)
    genCode (Abi $ abi) { exprPrefix: opts.exprPrefix, indentationLevel: 0 }
      # runImported opts destFile
      # writeTextFile UTF8 destFile
      # liftAff

maybeAnnotateArity :: Array AbiType -> Array AbiType
maybeAnnotateArity abi = evalState (traverse go abi) S.empty
  where
    go :: AbiType -> State (S.Set String) AbiType
    go fun@(AbiFunction (SolidityFunction f)) = do
      st <- get
      if f.name `S.member` st
         then pure $ AbiFunction $ SolidityFunction f {name = f.name <> show (length f.inputs)}
         else modify (S.insert f.name) *> pure fun
    go a = pure a

parseAbi :: forall r. {truffle :: Boolean | r} -> Json -> Either String AbiWithErrors
parseAbi {truffle} abiJson = case truffle of
  false -> decodeJson abiJson
  true -> let mabi = abiJson ^? _Object <<< ix "abi"
          in note "truffle artifact missing abi field" mabi >>= decodeJson

genPSModuleStatement :: GeneratorOptions -> FilePath -> String
genPSModuleStatement opts fp = comment <> "\n"
  <> "module " <> opts.modulePrefix <> "."
  <> basenameWithoutExt fp ".purs"
  <> " where\n"
    where
  comment = mkComment [basenameWithoutExt fp ".purs"]


--------------------------------------------------------------------------------
-- | Helpers
--------------------------------------------------------------------------------

-- get all the "valid" directories rooted in a filepath
getAllDirectories
  :: forall eff m.
     MonadAff (fs :: FS | eff) m
  => MonadState FilePath m
  => m (Array FilePath)
getAllDirectories = do
  currentDirectory <- get
  allFiles <- liftAff $ readdir currentDirectory
  mdirs <- for allFiles (validateRootedDir currentDirectory)
  pure $ catMaybes mdirs

-- determine whether or not a directory is valid (basically it's not dotted)
validateRootedDir
  :: forall eff m.
     MonadAff (fs :: FS | eff) m
  => FilePath -- prefix
  -> FilePath -- dirname
  -> m (Maybe FilePath)
validateRootedDir prefix dir = liftAff $ do
  let fullPath = prefix <> "/" <> dir
  estat <- try $ stat fullPath
  pure case estat of
    Left _ -> Nothing
    Right s ->
      let isValid = Stats.isDirectory s && isNothing (stripPrefix (Pattern ".") dir)
      in if isValid
        then Just fullPath
        else Nothing

-- | get all files in a directory with a ".json" extension
getJsonFilesInDirectory
  :: forall eff m.
     MonadAff (fs :: FS | eff) m
  => MonadState FilePath m
  => m (Array FilePath)
getJsonFilesInDirectory = do
  currentDirectory <- get
  allFiles <- liftAff $ readdir currentDirectory
  msolcs <- for allFiles (validateFile currentDirectory)
  pure $ catMaybes msolcs

-- | determine whether the file is a .json artifact
validateFile
  :: forall eff m.
     MonadAff (fs :: FS | eff) m
  => FilePath -- dir
  -> FilePath -- filepath
  -> m (Maybe FilePath)
validateFile dir f = liftAff $ do
  let fullPath = dir <> "/" <> f
  estat <- try $ stat fullPath
  pure case estat of
    Left _ -> Nothing
    Right s ->
      let isValid = Stats.isFile s && extname f == ".json"
      in if isValid
            then Just fullPath
            else Nothing

getAllJsonFiles
  :: forall eff m.
     MonadAff (fs :: FS | eff) m
  => FilePath
  -> m (Array FilePath)
getAllJsonFiles root = evalStateT getAllJsonFiles' root
  where
    getAllJsonFiles' :: StateT FilePath m (Array FilePath)
    getAllJsonFiles' = do
      cd <- get
      hereFiles <- getJsonFilesInDirectory
      hereDirectories <- getAllDirectories
      if null hereDirectories
         then pure hereFiles
         else do
              thereFiles <- for hereDirectories $ \d -> do
                              put d
                              getAllJsonFiles'
              pure $ hereFiles <> concat thereFiles
