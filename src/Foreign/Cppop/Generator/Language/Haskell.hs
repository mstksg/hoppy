module Foreign.Cppop.Generator.Language.Haskell (
  Generation,
  generate,
  generatedFiles,
  ) where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Arrow ((&&&), second)
import Control.Monad (forM, when)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (forM_)
import Data.Graph (SCC (AcyclicSCC, CyclicSCC), stronglyConnComp)
import Data.List (intersperse)
import Data.Tree (flatten, unfoldTree)
import qualified Data.Map as M
import Data.Maybe (isJust, mapMaybe)
import Data.Monoid (mconcat)
import qualified Data.Set as S
import Foreign.Cppop.Common
import Foreign.Cppop.Generator.Spec
import Foreign.Cppop.Generator.Language.Cpp.General (
  classDeleteFnCppName,
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
generate iface = do
  -- Build the partial generation of each module.
  modPartials <- forM (M.elems $ interfaceModules iface) $ \m ->
    (,) m <$> execGenerator iface (getModuleName iface m) (generateSource m)

  -- Compute the strongly connected components.  If there is a nontrivial SCC,
  -- then there is a module import cycle that we'll have to break with hs-boot
  -- files.
  let partialsByHsName :: M.Map HsModuleName Partial
      partialsByHsName = M.fromList $ map ((partialModuleHsName &&& id) . snd) modPartials

      sccInput :: [((Module, Partial), Partial, [Partial])]
      sccInput = flip map modPartials $ \x@(_, p) ->
        (x, p,
         mapMaybe (flip M.lookup partialsByHsName . hsImportModule) $
         M.keys $ getHsImportSet $ outputImports $ partialOutput p)

      sccs :: [SCC (Module, Partial)]
      sccs = stronglyConnComp sccInput

  fileContents <- execWriterT $ forM_ sccs $ \scc -> case scc of
    AcyclicSCC (_, p) -> tell [finishPartial p "hs"]
    CyclicSCC mps -> do
      let cycleModNames = S.fromList $ map (partialModuleHsName . snd) mps
      forM_ mps $ \(m, p) -> do
        -- Create a boot partial.
        pBoot <- lift $ execGenerator iface (partialModuleHsName p) (generateBootSource m)

        -- Change the source and boot partials so that all imports of modules in
        -- this cycle are {-# SOURCE #-} imports.
        let p' = setSourceImports cycleModNames p
            pBoot' = setSourceImports cycleModNames pBoot

        -- Emit the completed partials.
        tell [finishPartial p' "hs", finishPartial pBoot' "hs-boot"]

  return $ Generation $ M.fromList fileContents

  where finishPartial :: Partial -> String -> (FilePath, String)
        finishPartial p fileExt =
          (listSubst '.' pathSeparator (partialModuleHsName p) <.> fileExt,
           prependExtensions $ renderPartial p)

        setSourceImports :: S.Set HsModuleName -> Partial -> Partial
        setSourceImports modulesToSourceImport p =
          let output = partialOutput p
              imports = outputImports output
              imports' = makeHsImportSet $
                         M.mapWithKey (setSourceImportIfIn modulesToSourceImport) $
                         getHsImportSet imports
              output' = output { outputImports = imports' }
          in p { partialOutput = output' }

        setSourceImportIfIn :: S.Set HsModuleName -> HsImportKey -> HsImportSpecs -> HsImportSpecs
        setSourceImportIfIn modulesToSourceImport key specs =
          if hsImportModule key `S.member` modulesToSourceImport
          then specs { hsImportSource = True }
          else specs

prependExtensions :: String -> String
prependExtensions =
  -- MultiParamTypeClasses are necessary for instances of Decodable and
  -- Encodable.  FlexiableInstances and TypeSynonymInstances are enabled to
  -- allow conversions to and from String, which is really [Char].
  ("{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}\n\n" ++)

generateSource :: Module -> Generator ()
generateSource m = do
  forM_ (moduleExports m) $ sayExport SayExportForeignImports
  forM_ (moduleExports m) $ sayExport SayExportDecls

generateBootSource :: Module -> Generator ()
generateBootSource m =
  forM_ (moduleExports m) $ sayExport SayExportBoot

data SayExportMode = SayExportForeignImports | SayExportDecls | SayExportBoot

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
    addImports $ mconcat [hsImports "Prelude" ["($)", "(++)"], hsImportForPrelude]

    -- Print out the data declaration.
    ln
    saysLn ["data ", hsTypeName, " ="]
    indent $ do
      forM_ (zip (False:repeat True) values) $ \(cont, (_, hsCtorName)) ->
        saysLn [if cont then "| " else "", hsCtorName]
      sayLn "deriving (CppopP.Bounded, CppopP.Eq, CppopP.Ord, CppopP.Show)"

    -- Print out the Enum instance.
    ln
    saysLn ["instance CppopP.Enum ", hsTypeName, " where"]
    indent $ do
      forM_ values $ \(num, hsCtorName) ->
        saysLn ["fromEnum ", hsCtorName, " = ", show num]
      ln
      forM_ values $ \(num, hsCtorName) ->
        saysLn ["toEnum ", show num, " = ", hsCtorName]
      -- TODO Fix the potential name collision of 'n'.
      saysLn ["toEnum n = CppopP.error $ ",
              show (concat ["Unknown ", hsTypeName, " numeric value: "]),
              " ++ CppopP.show n"]

  SayExportBoot -> do
    let hsTypeName = toHsEnumTypeName enum
    addImports hsImportForPrelude
    ln
    saysLn ["data ", hsTypeName]
    saysLn ["instance CppopP.Bounded ", hsTypeName]
    saysLn ["instance CppopP.Enum ", hsTypeName]
    saysLn ["instance CppopP.Eq ", hsTypeName]
    saysLn ["instance CppopP.Ord ", hsTypeName]
    saysLn ["instance CppopP.Show ", hsTypeName]

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
      lineEnd <- case purity of
        Nonpure -> return [" ="]
        Pure -> do addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForUnsafeIO]
                   return [" = CppopSIU.unsafePerformIO $"]
      saysLn $ hsFnName : map (' ':) argNamesWithThis ++ lineEnd
      indent $ do
        forM_ (zip3 paramTypes argNames convertedArgNames) $ \(t, argName, argName') ->
          sayArgProcessing ToCpp t argName argName'

        sayCallAndProcessReturn ToCpp retType $
          saysLn $
          hsFnImportedName :
          (case methodInfo of
             Just (cst, cls) -> " (" ++ toHsCastMethodName cst cls ++ " this)"
             Nothing -> "") :
          map (' ':) convertedArgNames

    SayExportBoot ->
      -- Functions (methods included) cannot be referenced from other exports,
      -- so we don't need to emit anything.
      return ()

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
-- >         arg1 <- return arg1'
-- >         arg2 <- ...decode the string...
-- >         f arg1 arg2 >>= return
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
    (abort $ concat
     ["sayExportCallback: Couldn't create a function C-side type for ", show cb, "."]) =<<
    cppTypeToHsTypeAndUse HsCSide fnType

  hsFnHsType <-
    fromMaybeM
    (abort $ concat
     ["sayExportCallback: Couldn't create a function Haskell-side type for ", show cb, "."]) =<<
    cppTypeToHsTypeAndUse HsHsSide fnType

  let getWholeFnType = do
        addImports $ mconcat [hsImportForPrelude, hsImportForSupport]
        return $
          HsTyFun hsFnHsType $
          HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopP.IO") $
          HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopFCRS.CCallback") hsFnCType

  case mode of
    SayExportForeignImports -> do
      addImports $ mconcat [hsImportForForeign, hsImportForPrelude, hsImportForSupport]
      let hsFunPtrType = HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopF.FunPtr") hsFnCType
          hsFunPtrImportType =
            HsTyFun hsFnCType $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopP.IO") hsFunPtrType
          hsCallbackCtorImportType =
            HsTyFun hsFunPtrType $
            HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopF.FunPtr") $
                     HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopF.FunPtr") $
                              HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopP.IO") $
                              HsTyCon $ Special HsUnitCon) $
                     HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopP.IO") $
                     HsTyCon $ Special HsUnitCon) $
            HsTyFun (HsTyCon $ UnQual $ HsIdent "CppopP.Bool") $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopP.IO") $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopFCRS.CCallback") hsFnCType

      saysLn ["foreign import ccall \"wrapper\" ", hsFnName'newFunPtr, " :: ",
              prettyPrint hsFunPtrImportType]
      saysLn ["foreign import ccall \"", externalNameToCpp name, "\" ", hsFnName'newCallback, " :: ",
              prettyPrint hsCallbackCtorImportType]

    SayExportDecls -> do
      wholeFnType <- getWholeFnType
      let paramCount = length paramTypes
          argNames = map toArgName [1..paramCount]
          argNames' = map (++ "'") argNames
      ln
      saysLn [hsFnName, " :: ", prettyPrint wholeFnType]
      saysLn [hsFnName, " f'hs = do"]
      indent $ do
        sayLet
          [do saysLn ["f'c ", unwords argNames, " ="]
              indent $ do
                addImports $ hsImport1 "Prelude" "(>>=)"
                forM_ (zip3 paramTypes argNames argNames') $ \(t, argName, argName') ->
                  sayArgProcessing FromCpp t argName argName'
                sayCallAndProcessReturn FromCpp retType $
                  saysLn $ "f'hs" : map (' ':) argNames' ++ [" >>="]]
          Nothing
        saysLn ["f'p <- ", hsFnName'newFunPtr, " f'c"]
        saysLn [hsFnName'newCallback, " f'p CppopFCRS.freeHaskellFunPtrFunPtr CppopP.False"]

    SayExportBoot -> do
      wholeFnType <- getWholeFnType
      ln
      saysLn [hsFnName, " :: ", prettyPrint wholeFnType]

data CallDirection =
  ToCpp  -- ^ Haskell code is calling out to C++.
  | FromCpp  -- ^ C++ is invoking a callback.

sayArgProcessing :: CallDirection -> Type -> String -> String -> Generator ()
sayArgProcessing dir t fromVar toVar = case t of
  TVoid -> abort "sayArgProcessing: TVoid is not a valid argument type."
  TBool -> doPrimitive
  TChar -> doPrimitive
  TUChar -> doPrimitive
  TShort -> doPrimitive
  TUShort -> doPrimitive
  TInt -> doPrimitive
  TUInt -> doPrimitive
  TLong -> doPrimitive
  TULong -> doPrimitive
  TLLong -> doPrimitive
  TULLong -> doPrimitive
  TFloat -> doPrimitive
  TDouble -> doPrimitive
  TSize -> doPrimitive
  TSSize -> doPrimitive
  TEnum _ -> do
    addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForPrelude, hsImportForSupport]
    saysLn ["let ", toVar,
            case dir of
              ToCpp -> " = CppopFCRS.coerceIntegral $ CppopP.fromEnum "
              FromCpp -> " = CppopP.toEnum $ CppopFCRS.coerceIntegral ",
            fromVar, " in"]
  TPtr (TObj cls) -> do
    addImportForClass cls
    saysLn $ case dir of
      ToCpp -> ["let ", toVar, " = ", toHsCastMethodName Nonconst cls, " ", fromVar, " in"]
      FromCpp -> ["let ", toVar, " = ", fromVar, " in"]
  TPtr (TConst (TObj cls)) -> do
    addImportForClass cls
    saysLn $ case dir of
      ToCpp -> ["let ", toVar, " = ", toHsCastMethodName Const cls, " ", fromVar, " in"]
      FromCpp -> ["let ", toVar, " = ", fromVar, " in"]
  TPtr _ -> doPrimitive
  TRef t' -> sayArgProcessing dir (TPtr t') fromVar toVar
  TFn {} -> abort $ concat ["sayArgProcessing: TFn unimplemented, given ", show t, "."]
  TCallback cb -> case dir of
    ToCpp -> do
      importHsModuleForExtName $ callbackExtName cb
      saysLn [toHsCallbackCtorName cb, " ", fromVar, " >>= \\", toVar, " ->"]
    FromCpp ->
      abort $ concat ["sayArgProcessing: Can't receive a callback from C++, given ", show cb, "."]
  TObj _ -> case dir of
    ToCpp -> do
      addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForSupport]
      saysLn ["CppopFCRS.withCppObj ", fromVar, " $ \\", toVar, " ->"]
    FromCpp -> do
      addImports $ mconcat [hsImport1 "Prelude" "(>>=)", hsImportForSupport]
      saysLn ["CppopFCRS.decode ", fromVar, " >>= \\", toVar, " ->"]
  TConst t' -> sayArgProcessing dir t' fromVar toVar
  where doPrimitive = saysLn ["let ", toVar, " = ", fromVar, " in"]

sayCallAndProcessReturn :: CallDirection -> Type -> Generator () -> Generator ()
sayCallAndProcessReturn dir t sayCall = case t of
  TVoid -> sayCall
  TBool -> sayCall
  TChar -> sayCall
  TUChar -> sayCall
  TShort -> sayCall
  TUShort -> sayCall
  TInt -> sayCall
  TUInt -> sayCall
  TLong -> sayCall
  TULong -> sayCall
  TLLong -> sayCall
  TULLong -> sayCall
  TFloat -> sayCall
  TDouble -> sayCall
  TSize -> sayCall
  TSSize -> sayCall
  TEnum _ -> do
    addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForPrelude, hsImportForSupport]
    case dir of
      ToCpp -> saysLn ["CppopP.toEnum $ CppopFCRS.coerceIntegral"]
      FromCpp -> saysLn ["CppopFCRS.coerceIntegral $ CppopP.fromEnum"]
    sayCall
  TPtr _ -> sayCall
  TRef _ -> sayCall
  TFn {} -> abort $ concat ["sayCallAndProcessReturn: TFn unimplemented, given ", show t, "."]
  TCallback cb -> case dir of
    ToCpp ->
      abort $ concat ["sayCallAndProcessReturn: Can't receive a callback from C++, given ",
                      show cb, "."]
    FromCpp -> do
      importHsModuleForExtName $ callbackExtName cb
      saysLn [toHsCallbackCtorName cb, "=<<"]
      sayCall
  TObj _ -> do
    addImports $ mconcat [hsImport1 "Prelude" "(=<<)", hsImportForSupport]
    case dir of
      ToCpp -> sayLn "CppopFCRS.decodeAndDelete =<<"
      FromCpp -> sayLn "CppopFCRS.encode =<<"
    sayCall
  TConst t' -> sayCallAndProcessReturn dir t' sayCall

sayExportClass :: SayExportMode -> Class -> Generator ()
sayExportClass mode cls = do
  case mode of
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
      sayExportClassHsClass True cls Const
      sayExportClassHsClass True cls Nonconst

      sayExportClassHsStaticMethods cls

      -- Create a newtype for referencing foreign objects with pointers.  The
      -- newtype is not used with encodings of value objects.
      sayExportClassHsType True cls Const
      sayExportClassHsType True cls Nonconst

      sayExportClassHsNull cls
      sayExportClassHsCtors mode cls

    SayExportBoot -> do
      sayExportClassHsClass False cls Const
      sayExportClassHsClass False cls Nonconst

      sayExportClassHsType False cls Const
      sayExportClassHsType False cls Nonconst

  sayExportClassHsSpecialFns mode cls

sayExportClassHsClass :: Bool -> Class -> Constness -> Generator ()
sayExportClassHsClass doDecls cls cst = do
  let hsTypeName = toHsDataTypeName cst cls
      hsClassName = toHsClassName cst cls
      hsCastMethodName = toHsCastMethodName cst cls
      supers = classSuperclasses cls

  forM_ supers $ importHsModuleForExtName . classExtName
  hsSupers <-
    (\x -> if null x
           then do addImports hsImportForSupport
                   return ["CppopFCRS.CppPtr"]
           else return x) $
    case cst of
      Const -> map (toHsClassName Const) supers
      Nonconst -> toHsClassName Const cls : map (toHsClassName Nonconst) supers

  -- Print the class definition.
  ln
  saysLn $
    "class (" :
    intersperse ", " (map (++ " this") hsSupers) ++
    [") => ", hsClassName, " this"]

  -- Print the up-cast function.
  ln
  saysLn [hsCastMethodName, " :: ", hsClassName, " this => this -> ", hsTypeName]
  when doDecls $ do
    addImports $ mconcat [hsImport1 "Prelude" "(.)", hsImportForForeign, hsImportForSupport]
    saysLn [hsCastMethodName, " = ", hsTypeName, " . CppopF.castPtr . CppopFCRS.toPtr"]

  -- Print the non-static methods.
  when doDecls $ do
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

sayExportClassHsType :: Bool -> Class -> Constness -> Generator ()
sayExportClassHsType doDecls cls cst = do
  let hsTypeName = toHsDataTypeName cst cls
  addImports $ mconcat [hsImportForForeign, hsImportForSupport]
  ln
  saysLn ["newtype ", hsTypeName, " = ", hsTypeName, " (CppopF.Ptr ", hsTypeName, ")"]
  ln
  if doDecls
    then do saysLn ["instance CppopFCRS.CppPtr ", hsTypeName, " where"]
            saysLn ["  toPtr (", hsTypeName, " ptr) = ptr"]
            deleteTail <- case cst of
              Const -> return []
              Nonconst -> do addImports $ hsImport1 "Prelude" "(.)"
                             return [" . ", toHsCastMethodName Const cls]
            saysLn $ "  delete = " : toHsClassDeleteFnName cls : deleteTail
            ln
    else saysLn ["instance CppopFCRS.CppPtr ", hsTypeName]
  let neededInstances = flatten $ unfoldTree (id &&& classSuperclasses) cls
  forM_ neededInstances $ \cls' -> do
    addImportForClass cls'
    saysLn ["instance ", toHsClassName Const cls', " ", hsTypeName]
    when (cst == Nonconst) $
      saysLn ["instance ", toHsClassName Nonconst cls', " ", hsTypeName]

sayExportClassHsNull :: Class -> Generator ()
sayExportClassHsNull cls = do
  let clsExtName = classExtName cls
      clsHsNullName = toHsClassNullName cls
  addImports hsImportForForeign
  ln
  saysLn [clsHsNullName, " :: ", toHsTypeName Nonconst clsExtName]
  saysLn [clsHsNullName, " = ", toHsTypeName Nonconst clsExtName, " CppopF.nullPtr"]

sayExportClassHsCtors :: SayExportMode -> Class -> Generator ()
sayExportClassHsCtors mode cls =
  forM_ (classCtors cls) $ \ctor ->
  (sayExportFn mode <$> ctorExtName <*> pure Nothing <*>
   pure Nonpure <*> ctorParams <*> pure (TPtr $ TObj cls)) ctor

sayExportClassHsSpecialFns :: SayExportMode -> Class -> Generator ()
sayExportClassHsSpecialFns mode cls = do
  -- Say the delete function.
  case mode of
    SayExportForeignImports -> do
      addImports hsImportForPrelude
      saysLn ["foreign import ccall \"", classDeleteFnCppName cls, "\" ",
              toHsClassDeleteFnName cls, " :: ", toHsDataTypeName Const cls,
              " -> CppopP.IO ()"]
    -- The user interface to this is the generic 'delete' function, rendered
    -- elsewhere.
    SayExportDecls -> return ()
    SayExportBoot -> return ()

  -- Say Encodable and Decodable instances, if the class is encodable and
  -- decodable.
  forM_ (classHaskellConversion $ classConversions cls) $ \conv -> do
    let hsType = classHaskellConversionType conv
        hsTypeStr = concat ["(", prettyPrint hsType, ")"]
        typeName = toHsDataTypeName Nonconst cls
        typeNameConst = toHsDataTypeName Const cls
    case mode of
      SayExportForeignImports -> return ()

      SayExportDecls -> do
        addImports $ mconcat [hsImport1 "Control.Monad" "(>=>)",
                              hsImportForPrelude,
                              hsImportForSupport,
                              classHaskellConversionTypeImports conv,
                              classHaskellConversionToCppImports conv,
                              classHaskellConversionFromCppImports conv]

        -- Say the Encodable instances.
        ln
        saysLn ["instance CppopFCRS.Encodable ", typeName, " (", hsTypeStr, ") where"]
        indent $ do
          sayLn "encode ="
          indent $ mapM_ sayLn $ lines $ classHaskellConversionToCppFn conv
        ln
        saysLn ["instance CppopFCRS.Encodable ", typeNameConst, " (", hsTypeStr, ") where"]
        indent $
          saysLn ["encode = CppopP.fmap (", toHsCastMethodName Const cls,
                  ") . CppopFCRS.encodeAs (CppopP.undefined :: ", typeName, ")"]

        -- Say the Decodable instances.
        ln
        saysLn ["instance CppopFCRS.Decodable ", typeName, " (", hsTypeStr, ") where"]
        indent $
          saysLn ["decode = CppopFCRS.decode . ", toHsCastMethodName Const cls]
        ln
        saysLn ["instance CppopFCRS.Decodable ", typeNameConst, " (", hsTypeStr, ") where"]
        indent $ do
          sayLn "decode ="
          indent $ mapM_ sayLn $ lines $ classHaskellConversionFromCppFn conv

      SayExportBoot -> do
        addImports $ mconcat [hsImportForSupport,
                              classHaskellConversionTypeImports conv]
        ln
        saysLn ["instance CppopFCRS.Encodable ", typeName, " (", hsTypeStr, ")"]
        saysLn ["instance CppopFCRS.Encodable ", typeNameConst, " (", hsTypeStr, ")"]
        saysLn ["instance CppopFCRS.Decodable ", typeName, " (", hsTypeStr, ")"]
        saysLn ["instance CppopFCRS.Decodable ", typeNameConst, " (", hsTypeStr, ")"]

fnToHsTypeAndUse :: HsTypeSide
                 -> Maybe (Constness, Class)
                 -> Purity
                 -> [Type]
                 -> Type
                 -> Generator (Maybe HsQualType)
fnToHsTypeAndUse side methodInfo purity paramTypes returnType = do
  params <- mapM contextForParam $
            (case methodInfo of
                Just (cst, cls) -> [("this", case cst of
                                        Nonconst -> TPtr $ TObj cls
                                        Const -> TPtr $ TConst $ TObj cls)]
                Nothing -> []) ++
            zip (map toArgName [1..]) paramTypes
  let context = mapMaybe fst params :: HsContext
      hsParamsMaybe = mapM snd params
  case hsParamsMaybe of
    Nothing -> return Nothing
    Just hsParams -> do
      hsReturnMaybe <- cppTypeToHsTypeAndUse side returnType
      case hsReturnMaybe of
        Nothing -> return Nothing
        Just hsReturn -> do
          hsReturn' <- case purity of
            Pure -> return hsReturn
            Nonpure -> do
              addImports hsImportForPrelude
              return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "CppopP.IO") hsReturn
          return $ Just $ HsQualType context $ foldr HsTyFun hsReturn' hsParams

  where contextForParam :: (String, Type) -> Generator (Maybe HsAsst, Maybe HsType)
        contextForParam (s, t) = case t of
          TPtr (TObj cls) -> do
            addImportForClass cls
            return $ case side of
              HsHsSide -> let t' = HsTyVar $ HsIdent s
                          in (Just (UnQual $ HsIdent $ toHsClassName Nonconst cls, [t']),
                              Just t')
              HsCSide -> (Nothing, Just $ HsTyVar $ HsIdent $ toHsDataTypeName Nonconst cls)
          TPtr (TConst (TObj cls)) -> do
            addImportForClass cls
            return $ case side of
              HsHsSide -> let t' = HsTyVar $ HsIdent s
                          in (Just (UnQual $ HsIdent $ toHsClassName Const cls, [t']),
                              Just t')
              HsCSide -> (Nothing, Just $ HsTyVar $ HsIdent $ toHsDataTypeName Const cls)
          TConst t' -> contextForParam (s, t')
          _ -> (,) Nothing <$> cppTypeToHsTypeAndUse side t
