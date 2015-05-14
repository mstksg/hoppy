module Foreign.Cppop.Generator.Language.Haskell.General (
  getModuleName,
  toModuleName,
  HsExport,
  HsImport,
  Generator,
  runGenerator,
  execGenerator,
  abort,
  addExport,
  addExports,
  addImport,
  addImports,
  addImportSet,
  addQualifiedImports,
  sayLn,
  saysLn,
  ln,
  indent,
  sayLet,
  toHsTypeName,
  toHsEnumTypeName,
  toHsEnumCtorName,
  toHsClassName,
  toHsCastMethodName,
  toHsDataTypeName,
  toHsClassNullName,
  toHsClassDeleteFnName,
  toHsClassEncodeFnName,
  toHsClassDecodeFnName,
  toHsCallbackCtorName,
  toHsFnName,
  toArgName,
  HsTypeSide (..),
  encodingTypeForSide,
  cppTypeToHsTypeAndUse,
  addImportsForType,
  addImportsForClass,
  prettyPrint,
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, censor, runWriterT, tell)
import Data.Char (toLower, toUpper)
import Data.Foldable (forM_)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid, mappend, mconcat, mempty)
import qualified Data.Set as S
import Debug.Trace (trace)
import Foreign.Cppop.Generator.Spec
import qualified Language.Haskell.Pretty as P
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (Special, UnQual),
  HsSpecialCon (HsUnitCon),
  HsType (HsTyApp, HsTyCon, HsTyFun),
  )

getModuleName :: Interface -> Module -> String
getModuleName interface m =
  intercalate "." $
  fromMaybe ["Foreign", "Cppop", "Generated"] (interfaceHaskellModuleBase interface) ++
  fromMaybe [toModuleName $ moduleName m] (moduleHaskellName m)

toModuleName :: String -> String
toModuleName (x:xs) = toUpper x : map toLower xs
toModuleName "" = ""

type HsExport = String

type HsImport = String

{-
newtype HsImportSet = HsImportSet { getHsImportSet :: M.Map String (Maybe [String]) }

data HsImportSpecs = ...

instance Monoid HsImportSet where
  mempty = M.empty

  mappend (HsImportSet m) (HsImportSet m') = HsImportSet $ M.unionWith mergeImport m m'

  mconcat sets = M.unionsWith mergeImport map getHsImportSet sets

mergeImport :: Maybe [String] -> Maybe [String] -> Maybe [String]
mergeImport = liftM2 (++)
-}

type Generator = ReaderT Env (WriterT Output (Either String))

data Env = Env
  { envInterface :: Interface
  , envModuleName :: String
  }

askInterface :: Generator Interface
askInterface = envInterface <$> ask

askModuleName :: Generator String
askModuleName = envModuleName <$> ask

data Output = Output
  { outputExports :: [HsExport]
  , outputImports :: S.Set HsImport
  , outputBody :: [String]
  }

instance Monoid Output where
  mempty = Output [] S.empty []

  (Output e i b) `mappend` (Output e' i' b') =
    Output (e `mappend` e') (i `mappend` i') (b `mappend` b')

  mconcat os =
    Output (mconcat $ map outputExports os)
           (mconcat $ map outputImports os)
           (mconcat $ map outputBody os)

runGenerator :: Interface -> String -> Generator a -> Either String (String, a)
runGenerator iface moduleName generator = do
  (value, output) <- runWriterT $ runReaderT generator $ Env iface moduleName
  let imports = outputImports output
      body =
        intercalate "\n" $ concat
        [ [ "---------- GENERATED FILE, EDITS WILL BE LOST ----------"
          , ""
          ]
        , case outputExports output of
            [] -> [concat ["module ", moduleName, " where"]]
            exports ->
              concat ["module ", moduleName, " ("] :
              map (\export -> concat ["  ", export, ","]) exports ++
              ["  ) where"]
        , if S.null imports
          then []
          else "" : map (\i -> "import " ++ i) (S.toList imports)
        , [""]
        , outputBody output
        ]
  return (body, value)

execGenerator :: Interface -> String -> Generator a -> Either String String
execGenerator iface moduleName = fmap fst . runGenerator iface moduleName

-- | Halts generation and returns the given error message.
abort :: String -> Generator a
abort = lift . lift . Left

addExport :: HsExport -> Generator ()
addExport = addExports . (:[])

addExports :: [HsExport] -> Generator ()
addExports exports = tell $ mempty { outputExports = exports }

addImport :: HsImport -> Generator ()
addImport = addImportSet . S.singleton

addImports :: [HsImport] -> Generator ()
addImports = addImportSet . S.fromList

addImportSet :: S.Set HsImport -> Generator ()
addImportSet imports = tell $ mempty { outputImports = imports }

addQualifiedImports :: Generator ()
addQualifiedImports =
  addImports
  [ "qualified Foreign as F"
  , "qualified Foreign.C as FC"
  , "qualified Foreign.Cppop.Runtime.Support as FCRS"
  , "qualified Prelude as P"
  , "qualified System.IO.Unsafe as SIU"
  ]

sayLn :: String -> Generator ()
sayLn x = tell $ mempty { outputBody = [x] }

saysLn :: [String] -> Generator ()
saysLn = sayLn . concat

ln :: Generator ()
ln = sayLn ""

indent :: Generator a -> Generator a
indent = censor $ \o -> o { outputBody = map (\x -> ' ':' ':x) $ outputBody o }

sayLet :: [Generator ()] -> Maybe (Generator ()) -> Generator ()
sayLet bindings maybeBody = do
  sayLn "let"
  indent $ sequence_ bindings
  forM_ maybeBody $ \body ->
    -- Indent here in case we have a "let ... in ..." within a do block.
    indent $ do
      sayLn "in"
      indent body

toHsTypeName :: Constness -> ExtName -> String
toHsTypeName cst extName =
  (case cst of
      Const -> (++ "Const")
      Nonconst -> id) $
  case fromExtName extName of
    x:xs -> toUpper x:xs
    [] -> []

toHsEnumTypeName :: CppEnum -> String
toHsEnumTypeName = toHsTypeName Nonconst . enumExtName

toHsEnumCtorName :: CppEnum -> [String] -> String
toHsEnumCtorName enum words =
  concat $ toHsEnumTypeName enum : "_" : map capitalize words
  where capitalize "" = ""
        capitalize (c:cs) = toUpper c : map toLower cs

toHsClassName :: Constness -> Class -> String
toHsClassName cst cls = toHsTypeName cst (classExtName cls) ++ "Class"

toHsCastMethodName :: Constness -> Class -> String
toHsCastMethodName cst cls = "to" ++ toHsTypeName cst (classExtName cls)

toHsDataTypeName :: Constness -> Class -> String
toHsDataTypeName cst cls = toHsTypeName cst $ classExtName cls

toHsClassNullName :: Class -> String
toHsClassNullName cls = toHsFnName (classExtName cls) ++ "_null"

toHsClassDeleteFnName :: Class -> String
toHsClassDeleteFnName cls = 'd':'e':'l':'e':'t':'e':'\'':toHsDataTypeName Nonconst cls

toHsClassEncodeFnName :: Class -> String
toHsClassEncodeFnName cls = 'e':'n':'c':'o':'d':'e':'\'':toHsDataTypeName Nonconst cls

toHsClassDecodeFnName :: Class -> String
toHsClassDecodeFnName cls = 'd':'e':'c':'o':'d':'e':'\'':toHsDataTypeName Nonconst cls

toHsCallbackCtorName :: Callback -> String
toHsCallbackCtorName = toHsFnName . callbackExtName

-- TODO Rename this to toHsBindingName or toHsValueName; it's used for
-- non-function bindings too.
toHsFnName :: ExtName -> String
toHsFnName extName = case fromExtName extName of
  x:xs -> toLower x:xs
  [] -> []

toArgName :: Int -> String
toArgName = ("arg'" ++) . show

data HsTypeSide = HsCSide | HsHsSide

encodingTypeForSide :: HsTypeSide -> HaskellEncoding -> HsType
encodingTypeForSide HsCSide = haskellEncodingCType
encodingTypeForSide HsHsSide = haskellEncodingType

cppTypeToHsTypeAndUse :: HsTypeSide -> Type -> Generator (Maybe HsType)
cppTypeToHsTypeAndUse side t = case cppTypeToHsType side t of
  Nothing -> return Nothing
  result@(Just _) -> addImportsForType t >> return result

  where cppTypeToHsType :: HsTypeSide -> Type -> Maybe HsType
        cppTypeToHsType side t = case t of
          TVoid -> Just $ HsTyCon $ Special HsUnitCon
          TBool -> Just $ HsTyCon $ UnQual $ HsIdent "P.Bool"
          TChar -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CChar"
          TUChar -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CUChar"
          TShort -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CShort"
          TUShort -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CUShort"
          TInt -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CInt"
          TUInt -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CUInt"
          TLong -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CLong"
          TULong -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CULong"
          TLLong -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CLLong"
          TULLong -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CULLong"
          TFloat -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CFloat"
          TDouble -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CDouble"
          TSize -> Just $ HsTyCon $ UnQual $ HsIdent "FC.CSize"
          TSSize -> Nothing
          TEnum e -> Just $ HsTyCon $ UnQual $ HsIdent $ case side of
            HsCSide -> "FC.CInt"
            HsHsSide -> toHsEnumTypeName e
          TArray {} -> Nothing
          TPtr (TObj cls) ->
            Just $ HsTyCon $ UnQual $ HsIdent $ toHsTypeName Nonconst $ classExtName cls
          TPtr (TConst (TObj cls)) ->
            Just $ HsTyCon $ UnQual $ HsIdent $ toHsTypeName Const $ classExtName cls
          TPtr (TFn paramTypes retType) -> do
            paramHsTypes <- mapM (cppTypeToHsType side) paramTypes
            retHsType <- cppTypeToHsType side retType
            Just $
              (case side of
                 HsCSide -> HsTyApp (HsTyCon $ UnQual $ HsIdent "F.FunPtr")
                 HsHsSide -> id) $
              foldr HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") retHsType) paramHsTypes
          TPtr t' -> HsTyApp (HsTyCon $ UnQual $ HsIdent "F.Ptr") <$> cppTypeToHsType side t'
          TRef {} -> Nothing
          TFn paramTypes retType -> do
            paramHsTypes <- mapM (cppTypeToHsType side) paramTypes
            retHsType <- cppTypeToHsType side retType
            Just $ foldr HsTyFun
                         (HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") retHsType)
                         paramHsTypes
          TCallback cb ->
            (case side of
               HsCSide -> HsTyApp $ HsTyCon $ UnQual $ HsIdent "FCRS.CCallback"
               HsHsSide -> id) <$>
            cppTypeToHsType side (callbackToTFn cb)
          TObj cls -> fmap (encodingTypeForSide side) $ classHaskellType $ classEncoding cls
          TOpaque {} -> Nothing
          TBlob -> Nothing
          TConst t' -> cppTypeToHsType side t'

addImportsForType :: Type -> Generator ()
addImportsForType t' = case t' of
  TVoid -> return ()
  TBool -> return ()
  TChar -> return ()
  TUChar -> return ()
  TShort -> return ()
  TUShort -> return ()
  TInt -> return ()
  TUInt -> return ()
  TLong -> return ()
  TULong -> return ()
  TLLong -> return ()
  TULLong -> return ()
  TFloat -> return ()
  TDouble -> return ()
  TSize -> return ()
  TSSize -> return ()
  TEnum e -> importHsModuleForExtName $ enumExtName e
  TArray _ t'' -> addImportsForType t''
  TPtr (TObj cls) -> importHsModuleForExtName $ classExtName cls
  TPtr (TConst (TObj cls)) -> importHsModuleForExtName $ classExtName cls
  TPtr t'' -> addImportsForType t''
  TRef t'' -> error $
              "addImportsForType: TRef not implemented for type: " ++
              show t''
  TFn paramTypes retType -> mapM_ addImportsForType $ retType:paramTypes
  TCallback cb -> do
    importHsModuleForExtName $ callbackExtName cb
    addImportsForType $ callbackToTFn cb
  TObj cls ->
    forM_ (haskellEncodingImports <$> classHaskellType (classEncoding cls))
    addImportSet
  TOpaque {} -> return ()
  TBlob -> return ()
  TConst t'' -> addImportsForType t''

  where importHsModuleForExtName :: ExtName -> Generator ()
        importHsModuleForExtName extName = do
          iface <- askInterface
          case M.lookup extName $ interfaceNamesToModules iface of
            Just ownerModule -> do
              let ownerModuleName = getModuleName iface ownerModule
              currentModuleName <- askModuleName
              when (currentModuleName /= ownerModuleName) $
                trace (concat [">>> ", currentModuleName, " is importing ", ownerModuleName, " <<<"]) $
                addImport ownerModuleName
            Nothing ->
              abort $ "addImportsForType: Couldn't find module for ExtName: " ++
              show extName

addImportsForClass :: Class -> Generator ()
addImportsForClass = addImportsForType . TPtr . TObj

-- | Prints a value like 'P.prettyPrint', but removes newlines so that they
-- don't cause problems with this module's textual generation.  Should be mainly
-- used for printing types; stripping newlines from definitions for example
-- could go badly.
prettyPrint :: P.Pretty a => a -> String
prettyPrint = filter (/= '\n') . P.prettyPrint
