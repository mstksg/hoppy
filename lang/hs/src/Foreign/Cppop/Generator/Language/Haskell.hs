module Foreign.Cppop.Generator.Language.Haskell (
  Generation,
  generate,
  generatedSource,
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow ((&&&), first)
import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, censor, runWriterT, tell)
import Data.Char (toLower, toUpper)
import Data.Foldable (forM_)
import Data.List (intercalate, intersperse)
import Data.Tree (flatten, unfoldTree)
import Data.Tuple (swap)
import Data.Maybe (isJust, mapMaybe)
import Foreign.Cppop.Common
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Language.Cpp (externalNameToCpp)
import Language.Haskell.Pretty (prettyPrint)
import Language.Haskell.Syntax (
  HsAsst,
  HsContext,
  HsName (HsIdent),
  HsQName (Special, UnQual),
  HsQualType (HsQualType),
  HsSpecialCon (HsUnitCon),
  HsType (HsTyApp, HsTyCon, HsTyFun, HsTyVar),
  )

data HsTypeSide = HsCSide | HsHsSide

encodingTypeForSide :: HsTypeSide -> HaskellEncoding -> HsType
encodingTypeForSide HsCSide = haskellEncodingCType
encodingTypeForSide HsHsSide = haskellEncodingType

data Generation = Generation { generatedSource :: String }

type Generator = WriterT [String] (Either String)

-- | Halts generation and returns the given error message.
abort :: String -> Generator a
abort = lift . Left

generate :: Interface -> Either String Generation
generate interface =
  fmap (Generation . fst) $ runGenerator $ generateSource interface

runGenerator :: Generator a -> Either String (String, a)
runGenerator = fmap (first (intercalate "\n") . swap) . runWriterT

generateSource :: Interface -> Generator ()
generateSource interface = do
  saysLn ["module ", getModuleName interface, " where"]
  ln
  sayLn "import qualified Foreign as F"
  sayLn "import qualified Foreign.C as FC"
  sayLn "import qualified Foreign.Cppop.Runtime.Support as FCRS"
  sayLn "import qualified Prelude as P"
  sayLn "import qualified System.IO.Unsafe as SIU"
  sayLn "import Prelude ((.), ($), (>>=))"
  ln
  forM_ (interfaceExports interface) $ sayExport SayExportForeignImports
  forM_ (interfaceExports interface) $ sayExport SayExportDecls

getModuleName :: Interface -> String
getModuleName interface =
  let x:xs = interfaceName interface
      name = toUpper x : map toLower xs
  in "Foreign.Cppop.Generated." ++ name

toHsTypeName :: Constness -> ExtName -> String
toHsTypeName cst extName =
  (case cst of
      Const -> (++ "Const")
      Nonconst -> id) $
  case fromExtName extName of
    x:xs -> toUpper x:xs
    [] -> []

toHsClassName :: Constness -> Class -> String
toHsClassName cst cls = toHsTypeName cst (classExtName cls) ++ "Class"

toHsCastMethodName :: Constness -> Class -> String
toHsCastMethodName cst cls = "to" ++ toHsTypeName cst (classExtName cls)

toHsDataTypeName :: Constness -> Class -> String
toHsDataTypeName cst cls = toHsTypeName cst $ classExtName cls

toHsFnName :: ExtName -> String
toHsFnName extName = case fromExtName extName of
  x:xs -> toLower x:xs
  [] -> []

toArgName :: Int -> String
toArgName = ("arg" ++) . show

data SayExportMode = SayExportForeignImports | SayExportDecls

sayExport :: SayExportMode -> Export -> Generator ()
sayExport mode export = case export of
  ExportFn fn ->
    (sayExportFn mode <$> fnExtName <*> pure Nothing <*> fnPurity <*> fnParams <*> fnReturn) fn
  ExportClass cls -> sayExportClass mode cls

sayExportFn :: SayExportMode -> ExtName -> Maybe (Constness, Class) -> Purity -> [Type] -> Type -> Generator ()
sayExportFn mode name methodInfo purity paramTypes retType =
  let hsFnName = toHsFnName name
      hsFnImportedName = hsFnName ++ "'"
  in case mode of
    SayExportForeignImports -> do
      -- Print a "foreign import" statement.
      hsCType <-
        fromMaybeM
        (abort $ "Couldn't create Haskell C-side type signature for export \"" ++
         fromExtName name ++ "\".") $
        fnToHsType HsCSide methodInfo purity paramTypes retType
      saysLn ["foreign import ccall \"", externalNameToCpp name, "\" ", hsFnImportedName, " :: ",
              prettyPrint hsCType]

    SayExportDecls -> do
      -- Print the type signature.
      ln
      hsHsType <-
        fromMaybeM
        (abort $ "Couldn't create Haskell Haskell-side type signature for export \"" ++
         fromExtName name ++ "\".") $
        fnToHsType HsHsSide methodInfo purity paramTypes retType
      saysLn [hsFnName, " :: ", prettyPrint hsHsType]

      -- Print the function body.
      let argNames = map toArgName [1..length paramTypes]
          argNamesWithThis = (if isJust methodInfo then ("this":) else id) argNames
          convertedArgNames = map (++ "'") argNames
      -- Operators on this line must bind more weakly than operators used below,
      -- namely ($) and (>>=).  (So finish the line with ($).)
      saysLn $ hsFnName : map (' ':) argNamesWithThis ++ case purity of
        Nonpure -> [" ="]
        Pure -> [" = SIU.unsafePerformIO $"]
      indent $ do
        forM_ (zip3 paramTypes argNames convertedArgNames) $ \(t, argName, argName') ->
          sayArgumentEncode t argName argName'

        saysLn $
          hsFnImportedName :
          (case methodInfo of
             Just (cst, cls) -> " (" ++ toHsCastMethodName cst cls ++ " this)"
             Nothing -> "") :
          map (' ':) convertedArgNames ++
          [" >>="]

        sayReturnDecode retType

sayArgumentEncode :: Type -> String -> String -> Generator ()
sayArgumentEncode t argName argName' = case t of
  TVoid -> abort "sayArgumentEncode: TVoid unimplemented."
  TBool -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TChar -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TUChar -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TShort -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TUShort -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TInt -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TUInt -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TLong -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TULong -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TLLong -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TULLong -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TFloat -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TDouble -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TSize -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TSSize -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TArray {} -> abort "sayArgumentEncode: TArray unimplemented."
  TPtr (TObj cls) ->
    saysLn ["P.return (", toHsCastMethodName Nonconst cls, " ", argName, ") >>= \\", argName', " ->"]
  TPtr (TConst (TObj cls)) ->
    saysLn ["P.return (", toHsCastMethodName Const cls, " ", argName, ") >>= \\", argName', " ->"]
  TPtr _ -> saysLn ["P.return ", argName, " >>= \\", argName', " ->"]
  TRef {} -> abort "sayArgumentEncode: TRef unimplemented."
  TFn {} -> abort "sayArgumentEncode: TFn unimplemented."
  TObj cls -> case haskellEncodingEncoder <$> classHaskellType (classEncoding cls) of
    Just (HaskellEncoderWith withFn) ->
      saysLn ["(", withFn, ") ", argName, " $ \\", argName', " ->"]
    Just (HaskellEncoderConverter converterFn) ->
      saysLn ["P.return ((", converterFn, ") ", argName, ") >>= \\", argName', " ->"]
    Nothing -> abort $ "sayArgumentEncode: Can't encode class: " ++ show cls
  TOpaque {} -> abort "sayArgumentEncode: TOpaque unimplemented."
  TBlob {} -> abort "sayArgumentEncode: TBlob unimplemented."
  TConst t' -> sayArgumentEncode t' argName argName'

sayReturnDecode :: Type -> Generator ()
sayReturnDecode t = case t of
  TVoid -> sayLn "P.return"
  TBool -> sayLn "P.return"
  TChar -> sayLn "P.return"
  TUChar -> sayLn "P.return"
  TShort -> sayLn "P.return"
  TUShort -> sayLn "P.return"
  TInt -> sayLn "P.return"
  TUInt -> sayLn "P.return"
  TLong -> sayLn "P.return"
  TULong -> sayLn "P.return"
  TLLong -> sayLn "P.return"
  TULLong -> sayLn "P.return"
  TFloat -> sayLn "P.return"
  TDouble -> sayLn "P.return"
  TSize -> sayLn "P.return"
  TSSize -> sayLn "P.return"
  TArray {} -> abort "sayReturnDecode: TArray unimplemented."
  TPtr _ -> sayLn "P.return"
  TRef {} -> abort "sayReturnDecode: TRef unimplemented."
  TFn {} -> abort "sayReturnDecode: TFn unimplemented."
  TObj cls -> case haskellEncodingDecoder <$> classHaskellType (classEncoding cls) of
    Just decoderFn -> saysLn ["(", decoderFn, ")"]
    Nothing -> abort $ "sayReturnDecode: Can't decode class: " ++ show cls
  TOpaque {} -> abort "sayReturnDecode: TOpaque unimplemented."
  TBlob {} -> abort "sayReturnDecode: TBlob unimplemented."
  TConst t' -> sayReturnDecode t'

sayExportClass :: SayExportMode -> Class -> Generator ()
sayExportClass mode cls = case mode of
  SayExportForeignImports -> do
    sayExportClassHsCtors mode cls

    forM_ (classMethods cls) $ \method -> do
      let methodInfo = case methodApplicability method of
            MNormal -> Just (Nonconst, cls)
            MStatic -> Nothing
            MConst -> Just (Const, cls)
      (sayExportFn mode <$> methodExtName <*> pure methodInfo <*> methodPurity <*>
       methodParams <*> methodReturn) method

  SayExportDecls -> do
    sayExportClassHsClass cls Const
    sayExportClassHsClass cls Nonconst

    sayExportClassHsStaticMethods cls

    -- Create a newtype for referencing foreign objects with pointers.  The
    -- newtype is not used with encodings of value objects.
    sayExportClassHsType cls Const
    sayExportClassHsType cls Nonconst

    sayExportClassHsNull cls
    sayExportClassHsCtors mode cls

sayExportClassHsClass :: Class -> Constness -> Generator ()
sayExportClassHsClass cls cst = do
  let hsTypeName = toHsDataTypeName cst cls
      hsClassName = toHsClassName cst cls
      supers = classSuperclasses cls
      hsSupers =
        (\x -> if null x then ["FCRS.CppPtr"] else x) $
        case cst of
          Const -> map (toHsClassName Const) supers
          Nonconst -> toHsClassName Const cls : map (toHsClassName Nonconst) supers
      hsCastMethodName = toHsCastMethodName cst cls
  ln
  saysLn $
    "class (" :
    intersperse ", " (map (++ " this") hsSupers) ++
    [") => ", hsClassName, " this where"]
  indent $ do
    saysLn [hsCastMethodName, " :: this -> ", hsTypeName]
    saysLn [hsCastMethodName, " = ", hsTypeName, " . F.castPtr . FCRS.toPtr"]

    let methods = filter ((cst ==) . methodConst) $ classMethods cls
    forM_ methods $ \method ->
      when (methodStatic method == Nonstatic) $
      (sayExportFn SayExportDecls <$> methodExtName <*>
       pure (case methodApplicability method of
               MNormal -> Just (Nonconst, cls)
               MStatic -> error "sayExportClassHsClass: MStatic after Nonstatic, impossible case."
               MConst -> Just (Const, cls)) <*>
       methodPurity <*> methodParams <*> methodReturn) method

sayExportClassHsStaticMethods :: Class -> Generator ()
sayExportClassHsStaticMethods cls = do
  forM_ (classMethods cls) $ \method ->
    when (methodStatic method == Static) $
    (sayExportFn SayExportDecls <$> methodExtName <*> pure Nothing <*> methodPurity <*>
     methodParams <*> methodReturn) method

sayExportClassHsType :: Class -> Constness -> Generator ()
sayExportClassHsType cls cst = do
  let hsTypeName = toHsDataTypeName cst cls
  ln
  saysLn ["newtype ", hsTypeName, " = ", hsTypeName, " (F.Ptr ", hsTypeName, ")"]
  ln
  saysLn ["instance FCRS.CppPtr ", hsTypeName, " where"]
  saysLn ["  toPtr (", hsTypeName, " ptr) = ptr"]
  ln
  let neededInstances = flatten $ unfoldTree (id &&& classSuperclasses) cls
  forM_ neededInstances $ \cls' -> do
    saysLn ["instance ", toHsClassName Const cls', " ", hsTypeName]
    when (cst == Nonconst) $
      saysLn ["instance ", toHsClassName Nonconst cls', " ", hsTypeName]

sayExportClassHsNull :: Class -> Generator ()
sayExportClassHsNull cls = do
  let clsExtName = classExtName cls
      clsHsNullName = toHsFnName (classExtName cls) ++ "_null"
  ln
  saysLn [clsHsNullName, " :: ", toHsTypeName Nonconst clsExtName]
  saysLn [clsHsNullName, " = ", toHsTypeName Nonconst clsExtName, " F.nullPtr"]

sayExportClassHsCtors :: SayExportMode -> Class -> Generator ()
sayExportClassHsCtors mode cls =
  forM_ (classCtors cls) $ \ctor ->
  (sayExportFn mode <$> ctorExtName <*> pure Nothing <*>
   pure Nonpure <*> ctorParams <*> pure (TPtr $ TObj cls)) ctor

fnToHsType :: HsTypeSide -> Maybe (Constness, Class) -> Purity -> [Type] -> Type -> Maybe HsQualType
fnToHsType side methodInfo purity paramTypes returnType = do
  let params = map contextForParam $ zip [1..] paramTypes
      context = mapMaybe fst params :: HsContext
  hsParams <- fmap (case methodInfo of
                      Just (cst, cls) -> case side of
                        HsHsSide -> (HsTyVar (HsIdent "this") :)
                        HsCSide -> (HsTyVar (HsIdent $ toHsDataTypeName cst cls) :)
                      Nothing -> id) $
              sequence $ map snd params
  hsReturn <- fmap (case purity of
                       Pure -> id
                       Nonpure -> HsTyApp $ HsTyCon $ UnQual $ HsIdent "P.IO") $
              cppTypeToHsType side returnType
  return $ HsQualType context $ foldr HsTyFun hsReturn hsParams

  where contextForParam :: (Int, Type) -> (Maybe HsAsst, Maybe HsType)
        contextForParam (i, t) = case t of
          TPtr (TObj cls) -> case side of
            HsHsSide -> let t' = HsTyVar $ HsIdent $ toArgName i
                        in (Just (UnQual $ HsIdent $ toHsClassName Nonconst cls, [t']),
                            Just t')
            HsCSide -> (Nothing, Just $ HsTyVar $ HsIdent $ toHsDataTypeName Nonconst cls)
          TPtr (TConst (TObj cls)) -> case side of
            HsHsSide -> let t' = HsTyVar $ HsIdent $ toArgName i
                        in (Just (UnQual $ HsIdent $ toHsClassName Const cls, [t']),
                            Just t')
            HsCSide -> (Nothing, Just $ HsTyVar $ HsIdent $ toHsDataTypeName Const cls)
          TConst t' -> contextForParam (i, t')
          _ -> (Nothing, cppTypeToHsType side t)

cppTypeToHsType :: HsTypeSide -> Type -> Maybe HsType
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
  TArray {} -> Nothing
  --TPtr (TObj cls) -> Just $ HsTyApp (HsTyCon $ UnQual $ HsIdent "FP.Ptr") $ HsTyCon $ UnQual $ HsIdent $ toHsTypeName $ classExtName cls
  -- Oops, not a pointer to the pointer-newtype, the pointer-newtype itself!
  TPtr (TObj cls) ->
    Just $ HsTyCon $ UnQual $ HsIdent $ toHsTypeName Nonconst $ classExtName cls
  TPtr (TConst (TObj cls)) ->
    Just $ HsTyCon $ UnQual $ HsIdent $ toHsTypeName Const $ classExtName cls
  -- Do we even want this next instance?  If we want that functionality we
  -- should have our own poiner type, since Ptrs are meant to be valid in
  -- the Haskell process's address space.
  --TPtr t' -> fmap (HsTyApp (HsTyCon $ UnQual $ HsIdent "FP.Ptr")) $ cppTypeToHsType t'
  TPtr _ -> Nothing
  TRef {} -> Nothing
  TFn {} -> Nothing
  TObj cls -> fmap (encodingTypeForSide side) $ classHaskellType $ classEncoding cls
  TOpaque {} -> Nothing
  TBlob -> Nothing
  TConst t' -> cppTypeToHsType side t'

sayLn :: String -> Generator ()
sayLn x = tell [x]

saysLn :: [String] -> Generator ()
saysLn = sayLn . concat

ln :: Generator ()
ln = sayLn ""

indent :: Generator () -> Generator ()
indent = censor $ map $ \x -> ' ':' ':x
