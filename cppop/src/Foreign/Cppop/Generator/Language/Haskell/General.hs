{-# LANGUAGE CPP #-}

module Foreign.Cppop.Generator.Language.Haskell.General (
  getModuleName,
  toModuleName,
  HsExport,
  -- * Code generators
  Partial (..),
  Output (..),
  Generator,
  runGenerator,
  evalGenerator,
  execGenerator,
  renderPartial,
  abort,
  -- * Exports
  addExport,
  addExports,
  -- * Imports
  addImports,
  importHsModuleForExtName,
  -- * Code generation
  sayLn,
  saysLn,
  ln,
  indent,
  sayLet,
  toHsTypeName,
  toHsEnumTypeName,
  toHsEnumCtorName,
  toHsValueClassName,
  toHsWithValuePtrName,
  toHsPtrClassName,
  toHsCastMethodName,
  toHsDataTypeName,
  toHsClassNullName,
  toHsClassDeleteFnName,
  toHsCallbackCtorName,
  toHsFnName,
  toArgName,
  HsTypeSide (..),
  cppTypeToHsTypeAndUse,
  addImportForClass,
  prettyPrint,
  ) where

import Control.Arrow (first)
import Control.Monad (when)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
#else
import Control.Monad.Error (ErrorT, runErrorT, throwError)
#endif
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, censor, runWriterT, tell)
import Data.Char (toLower, toUpper)
import Data.Foldable (forM_)
import Data.Function (on)
import Data.Functor (($>))
import Data.List (intercalate, intersperse)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Tuple (swap)
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
toModuleName (x:xs) = toUpper x : xs
toModuleName "" = ""

type HsExport = String

-- | Renders a set of imports in Haskell syntax on multiple lines.
renderImports :: HsImportSet -> [String]
renderImports = map renderModuleImport . M.assocs . getHsImportSet
  where -- | Renders an import as a string that contains one or more lines.
        renderModuleImport :: (HsImportKey, HsImportSpecs) -> String
        renderModuleImport (key, specs) =
          let moduleName = hsImportModule key
              maybeQualifiedName = hsImportQualifiedName key
              isQual = isJust maybeQualifiedName
              importPrefix = if hsImportSource specs
                             then "import {-# SOURCE #-} "
                             else "import "
              importQualifiedPrefix =
                if hsImportSource specs
                then "import {-# SOURCE #-} qualified "
                else "import qualified "
          in case getHsImportSpecs specs of
            Nothing -> case maybeQualifiedName of
              Nothing -> importPrefix ++ moduleName
              Just qualifiedName -> concat [importQualifiedPrefix, moduleName, " as ", qualifiedName]
            Just specMap ->
              let specWords :: [String]
                  specWords = concatWithCommas $ map renderSpecAsWords $ M.assocs specMap
                  singleLineImport :: String
                  singleLineImport =
                    concat $
                    (if isQual then importQualifiedPrefix else importPrefix) :
                    moduleName : " (" : intersperse " " specWords ++
                    case maybeQualifiedName of
                      Nothing -> [")"]
                      Just qualifiedName -> [") as ", qualifiedName]
              in if null $ drop maxLineLength singleLineImport
                 then singleLineImport
                 else intercalate "\n" $
                      (importPrefix ++ moduleName ++ " (") :
                      groupWordsIntoLines specWords ++
                      case maybeQualifiedName of
                        Nothing -> ["  )"]
                        Just qualifiedName -> ["  ) as " ++ qualifiedName]

        -- | Takes an import spec, and returns a list of words that comprise
        -- that spec.  Line breaking may be performed by the caller only between
        -- these words.
        renderSpecAsWords :: (HsImportName, HsImportVal) -> [String]
        renderSpecAsWords (name, val) = case val of
          HsImportVal -> [name]
          -- When we export specific subnames under a name, then we put a
          -- non-breaking space between the outer name and the first inner name,
          -- just for a little readability.
          HsImportValSome parts -> case parts of
            [] -> [name ++ " ()"]
            [part] -> [concat [name, " (", part, ")"]]
            part0:parts -> let (parts', [partN]) = splitAt (length parts - 1) parts
                           in concat [name, " (", part0, ","] :
                              map (++ ",") parts' ++
                              [partN ++ ")"]
          HsImportValAll -> [name ++ " (..)"]

        -- | Takes a list of list of words.  Concatenates to get a list of
        -- words, appending a comma to the final word in each list of words.
        concatWithCommas :: [[String]] -> [String]
        concatWithCommas [] = []
        concatWithCommas ss =
          let (ss', ssLast@[_]) = splitAt (length ss - 1) ss
          in concat $ map (onLast (++ ",")) ss' ++ ssLast

        -- | Applies a function to the final element of a list, if the list is
        -- nonempty.
        onLast :: (a -> a) -> [a] -> [a]
        onLast _ [] = []
        onLast f xs = let (xs', [x]) = splitAt (length xs - 1) xs
                      in xs' ++ [f x]

        -- | Takes a list of words, and returns a list of lines with the words
        -- flowed.
        groupWordsIntoLines :: [String] -> [String]
        groupWordsIntoLines [] = []
        groupWordsIntoLines words =
          let (wordCount, line, _) =
                last $
                takeWhile (\(wordCount, _, len) -> wordCount <= 1 || len <= maxLineLength) $
                scanl (\(wordCount, acc, len) word ->
                        (wordCount + 1,
                         concat [acc, " ", word],
                         len + 1 + length word))
                      (0, "", 0)
                      words
          in line : groupWordsIntoLines (drop wordCount words)

        maxLineLength :: Int
        maxLineLength = 100

type Generator = ReaderT Env (WriterT Output (Either String))

data Env = Env
  { envInterface :: Interface
  , envModuleName :: String
  }

askInterface :: Generator Interface
askInterface = envInterface <$> ask

askModuleName :: Generator String
askModuleName = envModuleName <$> ask

data Partial = Partial
  { partialModuleHsName :: String
  , partialOutput :: Output
  }

instance Eq Partial where
  (==) = (==) `on` partialModuleHsName

instance Ord Partial where
  compare = compare `on` partialModuleHsName

data Output = Output
  { outputExports :: [HsExport]
  , outputImports :: HsImportSet
  , outputBody :: [String]
  }

instance Monoid Output where
  mempty = Output mempty mempty mempty

  (Output e i b) `mappend` (Output e' i' b') =
    Output (e `mappend` e') (i `mappend` i') (b `mappend` b')

  mconcat os =
    Output (mconcat $ map outputExports os)
           (mconcat $ map outputImports os)
           (mconcat $ map outputBody os)

runGenerator :: Interface -> String -> Generator a -> Either String (Partial, a)
runGenerator iface modName generator =
  fmap (first (Partial modName) . swap) $ runWriterT $ runReaderT generator $ Env iface modName

evalGenerator :: Interface -> String -> Generator a -> Either String a
evalGenerator iface modName =
  fmap snd . runGenerator iface modName

execGenerator :: Interface -> String -> Generator a -> Either String Partial
execGenerator iface modName =
  fmap fst . runGenerator iface modName

renderPartial :: Partial -> String
renderPartial partial =
  let modName = partialModuleHsName partial
      output = partialOutput partial
      imports = outputImports output
      body =
        intercalate "\n" $ concat
        [ [ "---------- GENERATED FILE, EDITS WILL BE LOST ----------"
          , ""
          ]
        , case outputExports output of
            [] -> [concat ["module ", modName, " where"]]
            exports ->
              concat ["module ", modName, " ("] :
              map (\export -> concat ["  ", export, ","]) exports ++
              ["  ) where"]
        , if M.null $ getHsImportSet imports
          then []
          else "" : renderImports imports
        , [""]
        , outputBody output
        ]
  in body

-- | Halts generation and returns the given error message.
abort :: String -> Generator a
abort = lift . lift . Left

addExport :: HsExport -> Generator ()
addExport = addExports . (:[])

addExports :: [HsExport] -> Generator ()
addExports exports = tell $ mempty { outputExports = exports }

addImports :: HsImportSet -> Generator ()
addImports imports = tell mempty { outputImports = imports }

importHsModuleForExtName :: ExtName -> Generator ()
importHsModuleForExtName extName = do
  iface <- askInterface
  case M.lookup extName $ interfaceNamesToModules iface of
    Just ownerModule -> do
      let ownerModuleName = getModuleName iface ownerModule
      currentModuleName <- askModuleName
      when (currentModuleName /= ownerModuleName) $
        addImports $ hsWholeModuleImport ownerModuleName
    Nothing ->
      abort $ concat
      ["importHsModuleForExtName: Couldn't find module for ", show extName,
       ", maybe you forgot to include it in an exports list?"]

sayLn :: String -> Generator ()
sayLn x =
  if '\n' `elem` x
  then abort $ concat
       ["sayLn: Refusing to speak '\n'.  Use (mapM_ sayLn . lines) instead.  Received ",
        show x, "."]
  else tell $ mempty { outputBody = [x] }

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

-- | The name for the typeclass of types that can be represented as values of
-- the given C++ class.
toHsValueClassName :: Class -> String
toHsValueClassName cls = toHsDataTypeName Nonconst cls ++ "Value"

toHsWithValuePtrName :: Class -> String
toHsWithValuePtrName cls = concat ["with", toHsDataTypeName Nonconst cls, "Ptr"]

-- | The name for the typeclass of types are pointers to objects of the given
-- C++ class, or subclasses.
toHsPtrClassName :: Constness -> Class -> String
toHsPtrClassName cst cls = toHsDataTypeName cst cls ++ "Ptr"

toHsCastMethodName :: Constness -> Class -> String
toHsCastMethodName cst cls = "to" ++ toHsDataTypeName cst cls

toHsDataTypeName :: Constness -> Class -> String
toHsDataTypeName cst cls = toHsTypeName cst $ classExtName cls

toHsClassNullName :: Class -> String
toHsClassNullName cls = toHsFnName (classExtName cls) ++ "_null"

toHsClassDeleteFnName :: Class -> String
toHsClassDeleteFnName cls = 'd':'e':'l':'e':'t':'e':'\'':toHsDataTypeName Nonconst cls

toHsCallbackCtorName :: Callback -> String
toHsCallbackCtorName = toHsFnName . callbackExtName

-- TODO Rename this to toHsBindingName or toHsValueName; it's used for
-- non-function bindings too.
toHsFnName :: ExtName -> String
toHsFnName = internalToHsFnName

toArgName :: Int -> String
toArgName = ("arg'" ++) . show

data HsTypeSide = HsCSide | HsHsSide

-- | Returns the 'HsType' corresponding to a 'Type', and also adds imports to
-- the 'Generator' as necessary for Haskell types that the 'Type' references.
-- On failure, a 'Left' is returned; 'abort' is not called.
cppTypeToHsTypeAndUse :: HsTypeSide -> Type -> Generator (Either String HsType)
#if MIN_VERSION_mtl(2,2,1)
cppTypeToHsTypeAndUse side t = runExceptT $ cppTypeToHsTypeAndUse' side t
#else
cppTypeToHsTypeAndUse side t = runErrorT $ cppTypeToHsTypeAndUse' side t
#endif

#if MIN_VERSION_mtl(2,2,1)
cppTypeToHsTypeAndUse' :: HsTypeSide -> Type -> ExceptT String Generator HsType
#else
cppTypeToHsTypeAndUse' :: HsTypeSide -> Type -> ErrorT String Generator HsType
#endif
cppTypeToHsTypeAndUse' side t = case t of
  TVar _ -> throwError $ freeVarErrorMsg "cppTypeToHsTypeAndUse'" t
  TVoid -> return $ HsTyCon $ Special HsUnitCon
  TBool -> doImports hsImportForPrelude $> HsTyCon (UnQual $ HsIdent "CppopP.Bool")
  TChar -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CChar")
  TUChar -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CUChar")
  TShort -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CShort")
  TUShort -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CUShort")
  TInt -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CInt")
  TUInt -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CUInt")
  TLong -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CLong")
  TULong -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CULong")
  TLLong -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CLLong")
  TULLong -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CULLong")
  TFloat -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CFloat")
  TDouble -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CDouble")
  TPtrdiff -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CPtrdiff")
  TSize -> doImports hsImportForForeignC $> HsTyCon (UnQual $ HsIdent "CppopFC.CSize")
  TSSize -> doImports hsImportForSystemPosixTypes $> HsTyCon (UnQual $ HsIdent "CppopSPT.CSsize")
  TEnum e -> HsTyCon . UnQual . HsIdent <$> case side of
    HsCSide -> doImports hsImportForForeignC $> "CppopFC.CInt"
    HsHsSide -> doImportForExtName (enumExtName e) $> toHsEnumTypeName e
  TPtr (TObj cls) ->
    doImportForExtName (classExtName cls) $>
    HsTyCon (UnQual $ HsIdent $ toHsTypeName Nonconst $ classExtName cls)
  TPtr (TConst (TObj cls)) ->
    doImportForExtName (classExtName cls) $>
    HsTyCon (UnQual $ HsIdent $ toHsTypeName Const $ classExtName cls)
  TPtr (TFn paramTypes retType) -> do
    paramHsTypes <- mapM (cppTypeToHsTypeAndUse' side) paramTypes
    retHsType <- cppTypeToHsTypeAndUse' side retType
    sideFn <- case side of
      HsCSide -> do doImports hsImportForForeign
                    return $ HsTyApp $ HsTyCon $ UnQual $ HsIdent "CppopF.FunPtr"
      HsHsSide -> return id
    doImports hsImportForPrelude
    return $ sideFn $
      foldr HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopP.IO") retHsType) paramHsTypes
  TPtr t' -> do
    doImports hsImportForForeign
    HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopF.Ptr") <$> cppTypeToHsTypeAndUse' side t'
  TRef t' -> cppTypeToHsTypeAndUse' side $ TPtr t'
  TFn paramTypes retType -> do
    paramHsTypes <- mapM (cppTypeToHsTypeAndUse' side) paramTypes
    retHsType <- cppTypeToHsTypeAndUse' side retType
    doImports hsImportForPrelude
    return $
      foldr HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopP.IO") retHsType) paramHsTypes
  TCallback cb -> do
    hsType <- cppTypeToHsTypeAndUse' side $ callbackToTFn cb
    case side of
      HsHsSide -> return hsType
      HsCSide -> do
        doImports hsImportForSupport
        return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopFCRS.CCallback") hsType
  TObj cls -> case side of
    HsCSide -> cppTypeToHsTypeAndUse' side $ TPtr $ TConst t
    HsHsSide -> case classHaskellConversion (classConversions cls) of
      Nothing ->
        throwError $ concat
        ["cppTypeToHsTypeAndUse': Expected a Haskell type for ", show cls,
         ", but there isn't one."]
      Just hsConv -> lift $ classHaskellConversionType hsConv
  TObjToHeap cls -> cppTypeToHsTypeAndUse' side $ TPtr $ TObj cls
  TConst t' -> cppTypeToHsTypeAndUse' side t'
  where doImports = lift . addImports
        doImportForExtName = lift . importHsModuleForExtName

addImportForClass :: Class -> Generator ()
addImportForClass = importHsModuleForExtName . classExtName

-- | Prints a value like 'P.prettyPrint', but removes newlines so that they
-- don't cause problems with this module's textual generation.  Should be mainly
-- used for printing types; stripping newlines from definitions for example
-- could go badly.
prettyPrint :: P.Pretty a => a -> String
prettyPrint = filter (/= '\n') . P.prettyPrint
