module Foreign.Cppop.Generator.Language.Haskell (
  Generation,
  generate,
  generatedSource,
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow ((&&&), first)
import Control.Monad (forM_, when)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, censor, runWriterT, tell)
import Data.Char (toLower, toUpper)
import Data.List (intercalate, intersperse)
import Data.Tree (flatten, unfoldTree)
import Data.Tuple (swap)
import Data.Maybe (mapMaybe)
import Foreign.Cppop.Generator.Spec
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
  sayLn "import qualified Data.Binary.Get as DBG"
  sayLn "import qualified Data.Binary.Put as DBP"
  sayLn "import qualified Data.ByteString.Lazy as DBL"
  sayLn "import qualified Foreign.Cppop.Runtime.Binary as FCRB"
  sayLn "import qualified Foreign.Cppop.Runtime.Client as FCRC"
  sayLn "import qualified Foreign.C.Types as FCT"
  sayLn "import qualified Prelude as P"
  sayLn "import qualified System.IO.Unsafe as SIU"
  sayLn "import Prelude (($))"
  mapM_ sayExport $ interfaceExports interface

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

toHsFnName :: ExtName -> String
toHsFnName extName = case fromExtName extName of
  x:xs -> toLower x:xs
  [] -> []

toArgName :: Int -> String
toArgName = ("arg" ++) . show

sayExport :: Export -> Generator ()
sayExport export = case export of
  ExportFn fn ->
    (sayExportFn <$> fnExtName <*> pure False <*> fnPurity <*> fnParams <*> fnReturn) fn
  ExportClass cls -> sayExportClass cls

sayExportFn :: ExtName -> Bool -> Purity -> [Type] -> Type -> Generator ()
sayExportFn name isMethod purity paramTypes retType = do
  let hsFnName = toHsFnName name
  ln

  -- Print the type signature.
  hsType <-
    maybe (abort $ "Couldn't create Haskell type signature for export \"" ++
           fromExtName name ++ "\".")
          return $
    fnToHsType isMethod purity paramTypes retType
  saysLn [hsFnName, " :: ", prettyPrint hsType]

  -- Print the function body.
  let argNames = map (\x -> "arg" ++ show x) [1..length paramTypes]
      argNamesWithThis = (if isMethod then ("this":) else id) argNames
  saysLn $ hsFnName : " client" : map (' ':) argNamesWithThis ++ [" ="]
  indent $ do
    sayLn $
      (case purity of
          Nonpure -> id
          Pure -> ("SIU.unsafePerformIO $ " ++)) $
      "P.fmap (DBG.runGet FCRB.hget) $ FCRC.call client " ++ show (fromExtName name) ++
      if null argNamesWithThis
      then " $ DBL.empty"
      else " $ DBP.runPut $ do"
    indent $ forM_ argNamesWithThis $ \argName ->
        saysLn ["FCRB.hput ", argName]

sayExportClass :: Class -> Generator ()
sayExportClass cls = do

  sayExportClassHsClass cls Const
  sayExportClassHsClass cls Nonconst

  sayExportClassHsStaticMethods cls

  -- Create a newtype for referencing foreign objects with pointers.  The
  -- newtype is not used with encodings of value objects.
  sayExportClassHsType cls Const
  sayExportClassHsType cls Nonconst

  sayExportClassHsNull cls
  sayExportClassHsCtors cls

sayExportClassHsClass :: Class -> Constness -> Generator ()
sayExportClassHsClass cls cst = do
  let extName = classExtName cls
      hsTypeName = toHsTypeName cst extName
      hsClassName = toHsClassName cst cls
      supers = classSuperclasses cls
      hsSupers =
        (\x -> if null x then ["FCRB.Ptr"] else x) $
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
    saysLn [hsCastMethodName, " = FCRB.cast"]

    let methods = filter ((cst ==) . methodConst) $ classMethods cls
    forM_ methods $ \method ->
      when (methodStatic method == Nonstatic) $
      (sayExportFn <$> methodExtName <*> pure True <*> methodPurity <*>
       methodParams <*> methodReturn) method

sayExportClassHsStaticMethods :: Class -> Generator ()
sayExportClassHsStaticMethods cls = do
  forM_ (classMethods cls) $ \method ->
    when (methodStatic method == Static) $
    (sayExportFn <$> methodExtName <*> pure False <*> methodPurity <*>
     methodParams <*> methodReturn) method

sayExportClassHsType :: Class -> Constness -> Generator ()
sayExportClassHsType cls cst = do
  let hsTypeName = toHsTypeName cst $ classExtName cls
  ln
  saysLn ["newtype ", hsTypeName, " = ", hsTypeName, " FCT.CIntPtr"]
  ln
  saysLn ["instance FCRB.Ptr ", hsTypeName, " where"]
  saysLn ["  toPtr (", hsTypeName, " ptr) = ptr"]
  saysLn ["  fromPtr = ", hsTypeName]
  ln
  saysLn ["instance FCRB.HostBinary ", hsTypeName, " where"]
  saysLn ["  hget = P.fmap ", hsTypeName, " FCRB.hget"]
  saysLn ["  hput (", hsTypeName, " ptr) = FCRB.hput ptr"]
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
  saysLn [clsHsNullName, " = ", toHsTypeName Nonconst clsExtName, " 0"]

sayExportClassHsCtors :: Class -> Generator ()
sayExportClassHsCtors cls =
  forM_ (classCtors cls) $ \ctor ->
    (sayExportFn <$> ctorExtName <*> pure False <*> pure Nonpure <*>
     ctorParams <*> pure (TPtr $ TObj cls)) ctor

fnToHsType :: Bool -> Purity -> [Type] -> Type -> Maybe HsQualType
fnToHsType isMethod purity paramTypes returnType = do
  let params = map contextForParam $ zip [1..] paramTypes
      context = mapMaybe fst params :: HsContext
  hsParams <- fmap (\x -> (HsTyCon (UnQual $ HsIdent "FCRC.Client") :) $
                          (if isMethod then (HsTyVar (HsIdent "this") :) else id)
                          x) $
              sequence $ map snd params
  hsReturn <- fmap (case purity of
                       Pure -> id
                       Nonpure -> HsTyApp $ HsTyCon $ UnQual $ HsIdent "P.IO") $
              cppTypeToHsType returnType
  return $ HsQualType context $ foldr HsTyFun hsReturn hsParams

  where contextForParam :: (Int, Type) -> (Maybe HsAsst, Maybe HsType)  -- Either (HsAsst, HType) (Maybe HsType)
        contextForParam (i, t) = case t of
          TPtr (TObj cls) ->
            let t' = HsTyVar $ HsIdent $ toArgName i
            in (Just (UnQual $ HsIdent $ toHsClassName Nonconst cls, [t']),
                Just t')
          TPtr (TConst (TObj cls)) ->
            let t' = HsTyVar $ HsIdent $ toArgName i
            in (Just (UnQual $ HsIdent $ toHsClassName Const cls, [t']),
                Just t')
          TConst t' -> contextForParam (i, t')
          _ -> (Nothing, cppTypeToHsType t)

cppTypeToHsType :: Type -> Maybe HsType
cppTypeToHsType t = case t of
  TVoid -> Just $ HsTyCon $ Special HsUnitCon
  TBool -> Just $ HsTyCon $ UnQual $ HsIdent "P.Bool"
  TChar -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CChar"
  TUChar -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CUChar"
  TShort -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CShort"
  TUShort -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CUShort"
  TInt -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CInt"
  TUInt -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CUInt"
  TLong -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CLong"
  TULong -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CULong"
  TLLong -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CLLong"
  TULLong -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CULLong"
  TFloat -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CFloat"
  TDouble -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CDouble"
  TSize -> Just $ HsTyCon $ UnQual $ HsIdent "FCT.CSize"
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
  TObj cls -> fmap coderType $ classHaskellType $ classEncoding cls
  TOpaque {} -> Nothing
  TBlob -> Nothing
  TConst t' -> cppTypeToHsType t'

cppFnTypeToHsType :: Purity -> [Type] -> Type -> Maybe HsType
cppFnTypeToHsType purity params ret =
  foldr HsTyFun <$>
  ((case purity of
      Pure -> id
      Nonpure -> HsTyApp $ HsTyCon $ UnQual $ HsIdent "P.IO") <$>
    cppTypeToHsType ret) <*>
  mapM cppTypeToHsType params

sayLn :: String -> Generator ()
sayLn x = tell [x]

saysLn :: [String] -> Generator ()
saysLn = sayLn . concat

ln :: Generator ()
ln = sayLn ""

indent :: Generator () -> Generator ()
indent = censor $ map $ \x -> ' ':' ':x
