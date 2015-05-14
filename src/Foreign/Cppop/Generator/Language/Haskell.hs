module Foreign.Cppop.Generator.Language.Haskell (
  Generation,
  generate,
  generatedFiles,
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow ((&&&), second)
import Control.Monad (forM, when)
import Data.Foldable (forM_)
import Data.List (intersperse)
import Data.Tree (flatten, unfoldTree)
import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe)
import Foreign.Cppop.Common
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Language.Cpp.General (
  classDecodeFnCppName,
  classDeleteFnCppName,
  classEncodeFnCppName,
  externalNameToCpp,
  )
import Foreign.Cppop.Generator.Language.Haskell.General
import Language.Haskell.Syntax (
  HsAsst,
  HsContext,
  HsName (HsIdent),
  HsQName (Special, UnQual),
  HsQualType (HsQualType),
  HsSpecialCon (HsUnitCon),
  HsType (HsTyApp, HsTyCon, HsTyFun, HsTyVar),
  )
import System.FilePath ((<.>), pathSeparator)

data Generation = Generation
  { generatedFiles :: M.Map FilePath String
    -- ^ A map from paths of generated files to the contents of those files.
    -- The file paths are relative paths below the Haskell generation root.
  }

generate :: Interface -> Either String Generation
generate iface =
  fmap (Generation . M.fromList) $
  forM (M.elems $ interfaceModules iface) $ \m ->
  let moduleName = getModuleName iface m
  in (,) (listSubst '.' pathSeparator moduleName <.> "hs") . prependExtensions <$>
     execGenerator iface moduleName (generateSource m)

prependExtensions :: String -> String
prependExtensions =
  -- MultiParamTypeClasses are necessary for instances of Decodable and
  -- Encodable.  FlexiableInstances and TypeSynonymInstances are enabled to
  -- allow conversions to and from String, which is really [Char].
  ("{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}\n\n" ++)

generateSource :: Module -> Generator ()
generateSource m = do
  addQualifiedImports
  addImports
    [ "Control.Monad ((>=>))"
    , "Prelude ((.), ($), (>>=), (++))"
    ]
  forM_ (moduleExports m) $ sayExport SayExportForeignImports
  forM_ (moduleExports m) $ sayExport SayExportDecls

data SayExportMode = SayExportForeignImports | SayExportDecls

sayExport :: SayExportMode -> Export -> Generator ()
sayExport mode export = case export of
  ExportEnum enum -> sayExportEnum mode enum
  ExportFn fn ->
    (sayExportFn mode <$> fnExtName <*> pure Nothing <*> fnPurity <*> fnParams <*> fnReturn) fn
  ExportClass cls -> sayExportClass mode cls
  ExportCallback cb -> sayExportCallback mode cb

sayExportEnum :: SayExportMode -> CppEnum -> Generator ()
sayExportEnum mode enum = case mode of
  -- Nothing to import from the C++ side of an enum.
  SayExportForeignImports -> return ()
  SayExportDecls -> do
    let hsTypeName = toHsEnumTypeName enum
        values :: [(Int, String)]
        values = map (second $ toHsEnumCtorName enum) $ enumValueNames enum

    -- Print out the data declaration.
    ln
    saysLn ["data ", hsTypeName, " ="]
    indent $ do
      forM_ (zip (False:repeat True) values) $ \(cont, (_, hsCtorName)) ->
        saysLn [if cont then "| " else "", hsCtorName]
      sayLn "deriving (P.Bounded, P.Eq, P.Ord, P.Show)"

    -- Print out the Enum instance.
    ln
    saysLn ["instance P.Enum ", hsTypeName, " where"]
    indent $ do
      forM_ values $ \(num, hsCtorName) ->
        saysLn ["fromEnum ", hsCtorName, " = ", show num]
      ln
      forM_ values $ \(num, hsCtorName) ->
        saysLn ["toEnum ", show num, " = ", hsCtorName]
      -- TODO Fix the potential name collision of 'n'.
      saysLn ["toEnum n = P.error $ ",
              show (concat ["Unknown ", hsTypeName, " numeric value: "]),
              " ++ P.show n"]

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
         fromExtName name ++ "\".") =<<
        fnToHsTypeAndUse HsCSide methodInfo purity paramTypes retType
      saysLn ["foreign import ccall \"", externalNameToCpp name, "\" ", hsFnImportedName, " :: ",
              prettyPrint hsCType]

    SayExportDecls -> do
      -- Print the type signature.
      ln
      hsHsType <-
        fromMaybeM
        (abort $ "Couldn't create Haskell Haskell-side type signature for export \"" ++
         fromExtName name ++ "\".") =<<
        fnToHsTypeAndUse HsHsSide methodInfo purity paramTypes retType
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
     show cb) =<<
    cppTypeToHsTypeAndUse HsCSide fnType

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
         show cb) =<<
        cppTypeToHsTypeAndUse HsHsSide fnType

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
        sayLet
          [do saysLn ["f'c ", unwords argNames, " ="]
              indent $ do
                forM_ (zip3 paramTypes argNames argNames') $ \(t, argName, argName') ->
                  sayDecode t [" ", argName, " >>= \\", argName', " ->"]
                saysLn $ "f'hs" : map (' ':) argNames' ++ [" >>="]
                sayEncode retType []]
          Nothing
        saysLn ["f'p <- ", hsFnName'newFunPtr, " f'c"]
        saysLn [hsFnName'newCallback, " f'p FCRS.freeHaskellFunPtrFunPtr P.False"]

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
  -- TODO The coersion here is unnecssary if we replace the C numeric types with
  -- their Haskell ones across the board (e.g. CInt -> Int).
  TEnum _ -> saysLn $ "(P.return . FCRS.coerceIntegral . P.fromEnum)" : suffix
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
  -- TODO The coersion here is unnecssary if we replace the C numeric types with
  -- their Haskell ones across the board (e.g. CInt -> Int).
  TEnum _ -> saysLn $ "(P.return . P.toEnum . FCRS.coerceIntegral)" : suffix
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
sayExportClass mode cls = do
  case mode of
    SayExportForeignImports -> do
      -- It doesn't matter when we emit the imports the class requires, but we
      -- only need to do it once.
      forM_ (classHaskellType $ classEncoding cls) $ \encoding ->
        addImportSet $ haskellEncodingImports encoding
      mapM_ addImportsForSuperclasses $ classSuperclasses cls

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

  sayExportClassHsSpecialFns mode cls

  where addImportsForSuperclasses :: Class -> Generator ()
        addImportsForSuperclasses superclass = do
          addImportsForClass superclass
          mapM_ addImportsForSuperclasses $ classSuperclasses superclass

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
sayExportClassHsStaticMethods cls =
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
      clsHsNullName = toHsClassNullName cls
  ln
  saysLn [clsHsNullName, " :: ", toHsTypeName Nonconst clsExtName]
  saysLn [clsHsNullName, " = ", toHsTypeName Nonconst clsExtName, " F.nullPtr"]

sayExportClassHsCtors :: SayExportMode -> Class -> Generator ()
sayExportClassHsCtors mode cls =
  forM_ (classCtors cls) $ \ctor ->
  (sayExportFn mode <$> ctorExtName <*> pure Nothing <*>
   pure Nonpure <*> ctorParams <*> pure (TPtr $ TObj cls)) ctor

sayExportClassHsSpecialFns :: SayExportMode -> Class -> Generator ()
sayExportClassHsSpecialFns mode cls = do
  -- Say the delete function.
  case mode of
    SayExportForeignImports ->
      saysLn ["foreign import ccall \"", classDeleteFnCppName cls, "\" ",
              toHsClassDeleteFnName cls, " :: ", toHsDataTypeName Const cls,
              " -> P.IO ()"]
    -- The user interface to this is the generic 'delete' function, rendered
    -- elsewhere.
    SayExportDecls -> return ()

  -- Say Encodable and Decodable instances, if the class is encodable and
  -- decodable.
  forM_ (classHaskellType $ classEncoding cls) $ \encoding -> do
    let hsType = haskellEncodingType encoding
        hsTypeStr = concat ["(", prettyPrint hsType, ")"]
        cType = haskellEncodingCType encoding
        typeName = toHsDataTypeName Nonconst cls
        typeNameConst = toHsDataTypeName Const cls
    case mode of
      SayExportForeignImports -> do
        let hsPtrType = HsTyCon $ UnQual $ HsIdent typeName
            hsConstPtrType = HsTyCon $ UnQual $ HsIdent typeNameConst
        saysLn ["foreign import ccall \"", classEncodeFnCppName cls, "\" ",
                toHsClassEncodeFnName cls, " :: ", prettyPrint (fnInIO cType hsPtrType)]
        saysLn ["foreign import ccall \"", classDecodeFnCppName cls, "\" ",
                toHsClassDecodeFnName cls, " :: ", prettyPrint (fnInIO hsConstPtrType cType)]

      SayExportDecls -> do
        -- Say the Encodable instances.
        ln
        saysLn ["instance FCRS.Encodable ", typeName, " (", hsTypeStr, ") where"]
        indent $ do
          sayLn "encode ="
          indent $ sayEncode (TObj cls) [" >=> ", toHsClassEncodeFnName cls]
        ln
        saysLn ["instance FCRS.Encodable ", typeNameConst, " (", hsTypeStr, ") where"]
        indent $
          --saysLn ["encode = P.fmap (", toHsCastMethodName Const cls, " . (`P.asTypeOf` (P.undefined :: ", typeName, "))) . FCRS.encode"]
          saysLn ["encode = P.fmap (", toHsCastMethodName Const cls,
                  ") . FCRS.encodeAs (P.undefined :: ", typeName, ")"]

        -- Say the Decodable instances.
        ln
        saysLn ["instance FCRS.Decodable ", typeName, " (", hsTypeStr, ") where"]
        indent $
          saysLn ["decode = FCRS.decode . ", toHsCastMethodName Const cls]
        ln
        saysLn ["instance FCRS.Decodable ", typeNameConst, " (", hsTypeStr, ") where"]
        indent $ do
          saysLn ["decode = ", toHsClassDecodeFnName cls, " >=>"]
          indent $ sayDecode (TObj cls) []

  where fnInIO :: HsType -> HsType -> HsType
        fnInIO arg result =
          HsTyFun arg $ HsTyApp (HsTyCon $ UnQual $ HsIdent "P.IO") result

fnToHsTypeAndUse :: HsTypeSide
                 -> Maybe (Constness, Class)
                 -> Purity
                 -> [Type]
                 -> Type
                 -> Generator (Maybe HsQualType)
fnToHsTypeAndUse side methodInfo purity paramTypes returnType = do
  params <- zipWithM (curry contextForParam) [1..] paramTypes
  let context = mapMaybe fst params :: HsContext
      hsParamsMaybe =
        (case methodInfo of
            Just (cst, cls) -> case side of
              HsHsSide -> (HsTyVar (HsIdent "this") :)
              HsCSide -> (HsTyVar (HsIdent $ toHsDataTypeName cst cls) :)
            Nothing -> id) <$>
        mapM snd params
  case hsParamsMaybe of
    Nothing -> return Nothing
    Just hsParams -> do
      hsReturnMaybe <-
        fmap (case purity of
                 Pure -> id
                 Nonpure -> HsTyApp $ HsTyCon $ UnQual $ HsIdent "P.IO") <$>
        cppTypeToHsTypeAndUse side returnType
      case hsReturnMaybe of
        Nothing -> return Nothing
        Just hsReturn ->
          return $ Just $ HsQualType context $ foldr HsTyFun hsReturn hsParams

  where contextForParam :: (Int, Type) -> Generator (Maybe HsAsst, Maybe HsType)
        contextForParam (i, t) = case t of
          TPtr (TObj cls) -> do
            addImportsForClass cls
            return $ case side of
              HsHsSide -> let t' = HsTyVar $ HsIdent $ toArgName i
                          in (Just (UnQual $ HsIdent $ toHsClassName Nonconst cls, [t']),
                              Just t')
              HsCSide -> (Nothing, Just $ HsTyVar $ HsIdent $ toHsDataTypeName Nonconst cls)
          TPtr (TConst (TObj cls)) -> do
            addImportsForClass cls
            return $ case side of
              HsHsSide -> let t' = HsTyVar $ HsIdent $ toArgName i
                          in (Just (UnQual $ HsIdent $ toHsClassName Const cls, [t']),
                              Just t')
              HsCSide -> (Nothing, Just $ HsTyVar $ HsIdent $ toHsDataTypeName Const cls)
          TConst t' -> contextForParam (i, t')
          _ -> (,) Nothing <$> cppTypeToHsTypeAndUse side t
