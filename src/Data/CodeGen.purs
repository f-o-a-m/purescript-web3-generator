module Data.CodeGen where

import Prelude
import Ansi.Codes (Color(Green))
import Ansi.Output (withGraphics, foreground)
import Control.Error.Util (note)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Writer (runWriter)
import Data.AbiParser (Abi)
import Data.Argonaut (Json, decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Prisms (_Object)
import Data.Array (filter, nub, sort)
import Data.Either (Either, either)
import Data.Foldable (foldl, for_)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Map (Map, fromFoldableWith, insert, lookup, member, toAscUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split)
import Data.String.Regex (Regex, test) as Rgx
import Data.String.Regex.Flags (noFlags) as Rgx
import Data.String.Regex.Unsafe (unsafeRegex) as Rgx
import Data.Tuple (Tuple(..))
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (FS, readTextFile, writeTextFile, readdir, mkdir, exists)
import Node.Path (FilePath, basenameWithoutExt, extname)

import Data.Generator (ModuleName, Imports, ModuleImports, ModuleImport(..), genCode, newLine1, mkComment)


type GeneratorOptions =
  { jsonDir :: FilePath
  , pursDir :: FilePath
  , truffle :: Boolean
  , exprPrefix :: String
  , modulePrefix :: String
  }

data IsCtrInImports = CtrIsInImports | CtrIsNotInImports
type ModuleImportsAcc = { types :: Map ModuleName IsCtrInImports, imports :: Array String }

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

generatePS :: forall e . GeneratorOptions -> Aff (fs :: FS, console :: CONSOLE | e) Unit
generatePS os = do
    let opts = os { pursDir = os.pursDir <> "/" <> replaceAll (Pattern ".") (Replacement "/") os.modulePrefix }
    fs <- readdir opts.jsonDir
    mkdirP opts.pursDir
    case fs of
      [] -> throwError <<< error $ "No abi json files found in directory: " <> opts.jsonDir
      fs' -> for_ (filter (\f -> extname f == ".json") fs') $ \f -> do
        if Rgx.test fileNameRegex (basenameWithoutExt f ".json")
          then do
            let f' = genPSFileName opts f
            writeCodeFromAbi opts (opts.jsonDir <> "/" <> f) f'
            let successCheck = withGraphics (foreground Green) $ "✔"
                successMsg = successCheck <> " contract module for " <> f <> " successfully written to " <> opts.pursDir
            liftEff <<< log $ successMsg
          else
            throwError <<< error $ "Got abi json file wich has invalid name: `" <> f <> "`"
  where
    genPSFileName :: GeneratorOptions -> FilePath -> FilePath
    genPSFileName opts fp =
        opts.pursDir <> "/" <> basenameWithoutExt fp ".json" <> ".purs"

mkdirP :: forall r. FilePath -> Aff (fs :: FS, console :: CONSOLE | r) Unit
mkdirP dir =
  void $ foldl mkdirAppend (pure "") (split (Pattern "/") dir)
  where
  mkdirAppend prev current = do
    p <- prev
    let
      next = if p == ""
        then current
        else p <> "/" <> current
    folderExists <- exists next
    unless folderExists do
      liftEff $ log $ "Folder: `" <> next <> "` doesn't exists, creating."
      mkdir next
    pure next

fileNameRegex :: Rgx.Regex
fileNameRegex =
  Rgx.unsafeRegex "^[A-Z][A-Za-z\\d]*$" Rgx.noFlags

-- | read in json abi and write the generated code to a destination file
writeCodeFromAbi :: forall e . GeneratorOptions -> FilePath -> FilePath -> Aff (fs :: FS | e) Unit
writeCodeFromAbi opts abiFile destFile = do
    ejson <- jsonParser <$> readTextFile UTF8 abiFile
    json <- either (throwError <<< error) pure ejson
    (abi :: Abi) <- either (throwError <<< error) pure $ parseAbi opts json
    let (Tuple code accImports) = runWriter $ genCode abi {exprPrefix: opts.exprPrefix, indentationLevel: 0}
    writeTextFile UTF8 destFile $ genPSModuleStatement opts destFile <> "\n"
      <> if code == "" then "" else runImports accImports <> "\n" <> code

parseAbi :: forall r. {truffle :: Boolean | r} -> Json -> Either String Abi
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
