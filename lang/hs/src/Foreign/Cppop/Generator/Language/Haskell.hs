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
import Foreign.Cppop.Generator.Language.Cpp (classDeleteFnCppName, externalNameToCpp)
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
  sayLn "---------- GENERATED FILE, EDITS WILL BE LOST ----------"
  ln
  saysLn ["module ", getModuleName interface, " where"]
  ln
  sayLn "import qualified Foreign as F"
  sayLn "import qualified Foreign.C as FC"
  sayLn "import qualified Foreign.Cppop.Runtime.Support as FCRS"
  sayLn "import qualified Prelude as P"
  sayLn "import qualified System.IO.Unsafe as SIU"
  sayLn "import Prelude ((.), ($), (>>=))"
  ln
  sayLn "foreign import ccall \"wrapper\" newFreeHaskellFunPtrFunPtr"
  indent $ sayLn ":: (F.FunPtr (P.IO ()) -> P.IO ())"
  indent $ sayLn "-> P.IO (F.FunPtr (F.FunPtr (P.IO ()) -> P.IO ()))"
  ln
  sayLn "freeHaskellFunPtrFunPtr :: F.FunPtr (F.FunPtr (P.IO ()) -> P.IO ())"
  sayLn "{-# NOINLINE freeHaskellFunPtrFunPtr #-}"
  sayLn "freeHaskellFunPtrFunPtr ="
  indent $ sayLn "SIU.unsafePerformIO $ newFreeHaskellFunPtrFunPtr F.freeHaskellFunPtr"
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

toHsClassDeleteFnName :: Class -> String
toHsClassDeleteFnName cls = 'd':'e':'l':'e':'t':'e':'\'':toHsDataTypeName Nonconst cls

toHsCallbackCtorName :: Callback -> String
toHsCallbackCtorName = toHsFnName . callbackExtName

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
  ExportCallback cb -> sayExportCallback mode cb

sayExportFn :: SayExportMode -> ExtName -> Maybe (Constness, Class) -> Purity -> [Type] -> Type -> Generator ()
sayExportFn mode name methodInfo purity paramTypes retType =
  let hsFnName = toHsFnName name
      hsFnImportedName = hsFnName ++ "'"
  in case mode of
    SayExportForeignImports -> do
      -- Print a "foreign import" statement.
      hsCType <-
        fromMaybeM
        (abort $ "sayExportFn: Couldn't create Haskell C-side type signature for export \"" ++
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
          sayEncode t [" ", argName, " >>= \\", argName', " ->"]

        saysLn $
          hsFnImportedName :
          (case methodInfo of
             Just (cst, cls) -> " (" ++ toHsCastMethodName cst cls ++ " this)"
             Nothing -> "") :
          map (' ':) convertedArgNames ++
          [" >>="]

        sayDecode retType []

-- | Prints \"foreign import\" statements and an internal callback construction
-- function for a given 'Callback' specification.  For example, for a callback
-- of 'HsHsSide' type @Int -> String -> IO Int@, we will generate the following
-- bindings:
--
-- > foreign import ccall "wrapper" name'newFunPtr
-- >   :: (CInt -> TPtr TChar -> IO CInt)
-- >   -> IO (FunPtr (CInt -> TPtr TChar -> IO CInt))
-- >
-- > -- (This is an ad-hoc generated binding for C++ callback impl class constructor.)
-- > foreign import ccall "genpop__name_impl" name'newCallback
-- >   :: FunPtr (CInt -> TPtr TChar -> IO CInt)
-- >   -> FunPtr (FunPtr (IO ()) -> IO ())
-- >   -> Bool
-- >   -> IO (CCallback (CInt -> TPtr TChar -> IO CInt))
-- >
-- > name :: (CInt -> String -> IO CInt) -> IO (CCallback (CInt -> TPtr TChar -> IO CInt))
-- > name f = do
-- >   let cf arg1' arg2' = do
-- >         arg1 <- P.return arg1'
-- >         arg2 <- ...decode the string...
-- >         f arg1 arg2 >>= P.return
-- >   cfp <- name'newFunPtr cf
-- >   name'newCallback cfp freeHaskellFunPtrFunPtr False
--
-- Only the implementation of bindings that take a callback of this type will
-- make use of this @name@ binding; @name@ is not useful to users of bindings.
sayExportCallback :: SayExportMode -> Callback -> Generator ()
sayExportCallback mode cb = do
  let name = callbackExtName cb
      paramTypes = callbackParams cb
      retType = callbackReturn cb
      fnType = callbackToTFn cb
      hsFnName = toHsCallbackCtorName cb
      hsFnName'newCallback = hsFnName ++ "'newCallback"
      hsFnName'newFunPtr = hsFnName ++ "'newFunPtr"

  hsFnCType <-
    fromMaybeM
    (abort $ "sayExportCallback: Couldn't create a function C-side type for callback: " ++
     show cb) $
    cppTypeToHsType HsCSide fnType

  case mode of
    SayExportForeignImports -> do
      let hsFunPtrType = HsTyApp (HsTyCon $ UnQual $ HsIdent "F.FunPtr") hsFnCType
          hsFunPtrImportType =
            HsTyFun hsFnCType $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") hsFunPtrType
          hsCallbackCtorImportType =
            HsTyFun hsFunPtrType $
            HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "F.FunPtr") $
                     HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "F.FunPtr") $
                              HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") $
                              HsTyCon $ Special HsUnitCon) $
                     HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") $
                     HsTyCon $ Special HsUnitCon) $
            HsTyFun (HsTyCon $ UnQual $ HsIdent "P.Bool") $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "FCRS.CCallback") hsFnCType

      saysLn ["foreign import ccall \"wrapper\" ", hsFnName'newFunPtr, " :: ",
              prettyPrint hsFunPtrImportType]
      saysLn ["foreign import ccall \"", externalNameToCpp name, "\" ", hsFnName'newCallback, " :: ",
              prettyPrint hsCallbackCtorImportType]

    SayExportDecls -> do
      hsFnHsType <-
        fromMaybeM
        (abort $ "sayExportCallback: Couldn't create a function Haskell-side type for callback: " ++
         show cb) $
        cppTypeToHsType HsHsSide fnType

      let wholeFnType =
            HsTyFun hsFnHsType $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "FCRS.CCallback") hsFnCType
          paramCount = length paramTypes
          argNames = map toArgName [1..paramCount]
          argNames' = map (++ "'") argNames
      ln
      saysLn [hsFnName, " :: ", prettyPrint wholeFnType]
      saysLn [hsFnName, " f'hs = do"]
      indent $ do
        flip sayLet Nothing
          [do saysLn ["f'c ", intercalate " " argNames, " ="]
              indent $ do
                forM_ (zip3 paramTypes argNames argNames') $ \(t, argName, argName') ->
                  sayDecode t [" ", argName, " >>= \\", argName', " ->"]
                saysLn $ "f'hs" : map (' ':) argNames' ++ [" >>="]
                sayEncode retType []]
        saysLn ["f'p <- ", hsFnName'newFunPtr, " f'c"]
        saysLn [hsFnName'newCallback, " f'p freeHaskellFunPtrFunPtr P.False"]

sayEncode :: Type -> [String] -> Generator ()
sayEncode t suffix = case t of
  TVoid -> saysLn $ "P.return" : suffix
  TBool -> saysLn $ "P.return" : suffix
  TChar -> saysLn $ "P.return" : suffix
  TUChar -> saysLn $ "P.return" : suffix
  TShort -> saysLn $ "P.return" : suffix
  TUShort -> saysLn $ "P.return" : suffix
  TInt -> saysLn $ "P.return" : suffix
  TUInt -> saysLn $ "P.return" : suffix
  TLong -> saysLn $ "P.return" : suffix
  TULong -> saysLn $ "P.return" : suffix
  TLLong -> saysLn $ "P.return" : suffix
  TULLong -> saysLn $ "P.return" : suffix
  TFloat -> saysLn $ "P.return" : suffix
  TDouble -> saysLn $ "P.return" : suffix
  TSize -> saysLn $ "P.return" : suffix
  TSSize -> saysLn $ "P.return" : suffix
  TArray {} -> abort "sayEncode: TArray unimplemented."
  TPtr (TObj cls) ->
    saysLn $ "(P.return . " : toHsCastMethodName Nonconst cls : ")" : suffix
  TPtr (TConst (TObj cls)) ->
    saysLn $ "(P.return . " : toHsCastMethodName Const cls : ")" : suffix
  TPtr _ -> saysLn $ "P.return" : suffix
  TRef {} -> abort "sayEncode: TRef unimplemented."
  TFn {} -> abort "sayEncode: TFn unimplemented."
  TCallback cb -> saysLn $ toHsCallbackCtorName cb : suffix
  TObj cls -> case haskellEncodingEncoder <$> classHaskellType (classEncoding cls) of
    Just converterFn ->
      -- TODO Use the Encode class here?
      saysLn $ "(" : converterFn : ")" : suffix
    Nothing -> abort $ "sayEncode: Can't encode class: " ++ show cls
  TOpaque {} -> abort "sayEncode: TOpaque unimplemented."
  TBlob {} -> abort "sayEncode: TBlob unimplemented."
  TConst t' -> sayEncode t' suffix

sayDecode :: Type -> [String] -> Generator ()
sayDecode t suffix = case t of
  TVoid -> saysLn $ "P.return" : suffix
  TBool -> saysLn $ "P.return" : suffix
  TChar -> saysLn $ "P.return" : suffix
  TUChar -> saysLn $ "P.return" : suffix
  TShort -> saysLn $ "P.return" : suffix
  TUShort -> saysLn $ "P.return" : suffix
  TInt -> saysLn $ "P.return" : suffix
  TUInt -> saysLn $ "P.return" : suffix
  TLong -> saysLn $ "P.return" : suffix
  TULong -> saysLn $ "P.return" : suffix
  TLLong -> saysLn $ "P.return" : suffix
  TULLong -> saysLn $ "P.return" : suffix
  TFloat -> saysLn $ "P.return" : suffix
  TDouble -> saysLn $ "P.return" : suffix
  TSize -> saysLn $ "P.return" : suffix
  TSSize -> saysLn $ "P.return" : suffix
  TArray {} -> abort "sayDecode: TArray unimplemented."
  TPtr _ -> saysLn $ "P.return" : suffix
  TRef {} -> abort "sayDecode: TRef unimplemented."
  TFn {} -> abort "sayDecode: TFn unimplemented."
  TCallback {} -> abort "sayDecode: TCallback unimplemented."
  TObj cls -> case haskellEncodingDecoder <$> classHaskellType (classEncoding cls) of
    Just decoderFn -> saysLn $ "(" : decoderFn : ")" : suffix
    Nothing -> abort $ "sayDecode: Can't decode class: " ++ show cls
  TOpaque {} -> abort "sayDecode: TOpaque unimplemented."
  TBlob {} -> abort "sayDecode: TBlob unimplemented."
  TConst t' -> sayDecode t' suffix

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

    sayExportClassHsDeleteImports cls

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
  saysLn $ "  delete = " : toHsClassDeleteFnName cls : case cst of
    Const -> []
    Nonconst -> [" . ", toHsCastMethodName Const cls]
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

sayExportClassHsDeleteImports :: Class -> Generator ()
sayExportClassHsDeleteImports cls =
  saysLn ["foreign import ccall \"", classDeleteFnCppName cls, "\" ",
          toHsClassDeleteFnName cls, " :: ", toHsDataTypeName Const cls,
          " -> P.IO ()"]

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
  TPtr (TFn paramTypes retType) -> do
    paramHsTypes <- mapM (cppTypeToHsType side) paramTypes
    retHsType <- cppTypeToHsType side retType
    Just $
      (case side of
         HsCSide -> HsTyApp (HsTyCon $ UnQual $ HsIdent "F.FunPtr")
         HsHsSide -> id) $
      foldr HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") retHsType) paramHsTypes
  -- Do we even want this next instance?  If we want that functionality we
  -- should have our own poiner type, since Ptrs are meant to be valid in
  -- the Haskell process's address space.
  --TPtr t' -> fmap (HsTyApp (HsTyCon $ UnQual $ HsIdent "FP.Ptr")) $ cppTypeToHsType t'
  TPtr _ -> Nothing
  TRef {} -> Nothing
  TFn paramTypes retType -> do
    paramHsTypes <- mapM (cppTypeToHsType side) paramTypes
    retHsType <- cppTypeToHsType side retType
    Just $ foldr HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") retHsType) paramHsTypes
  TCallback cb ->
    (case side of
       HsCSide -> HsTyApp $ HsTyCon $ UnQual $ HsIdent "FCRS.CCallback"
       HsHsSide -> id) <$>
    cppTypeToHsType side (callbackToTFn cb)
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

indent :: Generator a -> Generator a
indent = censor $ map $ \x -> ' ':' ':x

sayLet :: [Generator ()] -> Maybe (Generator ()) -> Generator ()
sayLet bindings maybeBody = do
  sayLn "let"
  indent $ sequence_ bindings
  forM_ maybeBody $ \body ->
    -- Indent here in case we have a "let ... in ..." within a do block.
    indent $ do
      sayLn "in"
      indent body
