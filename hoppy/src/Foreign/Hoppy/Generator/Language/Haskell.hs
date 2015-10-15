{-# LANGUAGE CPP #-}

-- | Internal portion of the Haskell code generator.
module Foreign.Hoppy.Generator.Language.Haskell (
  Generation,
  generate,
  generatedFiles,
  ) where

-- Module structure:
--
-- The result of generating a Hoppy module is a single Haskell module that
-- contains bindings for everything exported from the Hoppy module.  While
-- generated C++ modules get their objects from #includes of underlying headers
-- and only depend on each other in the case of callbacks, Haskell modules
-- depend on each other any time something in one references something in
-- another (somewhat mirroring the dependency graph of the binding definitions),
-- so cycles are much more common (for example, when a C++ interface uses a
-- forward class declaration to break an #include cycle).  Fortunately, GHC
-- supports dependency cycles, so Hoppy automatically detects and breaks cycles
-- with the use of .hs-boot files.  The boot files contain everything that could
-- be used from another generated module, for example class casting functions
-- needed to coerce pointers to the right type for a foreign call, or enum data
-- declarations.

-- How object passing works:
--
-- All of the comments about argument passing in Cpp.hs apply here.  The
-- following types are used for passing arguments from Haskell to C++:
--
--  C++ type   | Pass over FFI | HsCSide  | HsHsSide
-- ------------+---------------+----------+-----------------
--  Foo        | Foo const*    | FooConst | FooValue a => a
--  Foo const& | Foo const*    | FooConst | FooValue a => a
--  Foo&       | Foo*          | Foo      | FooPtr a => a
--  Foo const* | Foo const*    | FooConst | FooValue a => a
--  Foo*       | Foo*          | Foo      | FooPtr a => a
--
-- FooPtr contains pointers to nonconst Foo (and all subclasses).  FooValue
-- contains pointers to const and nonconst Foo (and all subclasses), as well as
-- the convertible Haskell type, if there is one.  The rationale is that
-- FooValue is used where the callee will not modify the argument, so both a
-- const pointer to an existing object, and a fresh const pointer to a temporary
-- on the case of passing a Foo, is fine.  Because functions taking Foo& and
-- Foo* may modify their argument, we disallow passing a temporary converted
-- from a Haskell value implicitly; 'withCppObj' can be used for this.
--
-- For values returned from C++, and for arguments and return values in
-- callbacks, the HsCSide column above is the exposed type; polymorphism as in
-- the HsHsSide column is not provided.

import Control.Arrow ((&&&), second)
import Control.Monad (forM, unless, when)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except (throwError)
#else
import Control.Monad.Error (throwError)
#endif
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (forM_)
import Data.Graph (SCC (AcyclicSCC, CyclicSCC), stronglyConnComp)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, mapMaybe)
import qualified Data.Set as S
import Foreign.Hoppy.Common
import Foreign.Hoppy.Generator.Spec
import Foreign.Hoppy.Generator.Language.Cpp.General (
  classCastFnCppName,
  classDeleteFnCppName,
  externalNameToCpp,
  )
import Foreign.Hoppy.Generator.Language.Haskell.General
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

-- | The in-memory result of generating Haskell code for an interface.
data Generation = Generation
  { generatedFiles :: M.Map FilePath String
    -- ^ A map from paths of generated files to the contents of those files.
    -- The file paths are relative paths below the Haskell generation root.
  }

-- | Runs the C++ code generator against an interface.
generate :: Interface -> Either ErrorMsg Generation
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
prependExtensions = (prependExtensionsPrefix ++)

prependExtensionsPrefix :: String
prependExtensionsPrefix =
  -- MultiParamTypeClasses are necessary for instances of Decodable and
  -- Encodable.  FlexibleContexts is needed for the type signature of the
  -- function that wraps the actual callback function in callback creation
  -- functions.
  --
  -- FlexibleInstances and TypeSynonymInstances are enabled to allow conversions
  -- to and from String, which is really [Char].
  --
  -- UndecidableInstances is needed for instances of the form "SomeClassConstPtr
  -- a => SomeClassValue a", and overlapping instances are used for the overlap
  -- between these instances and instances of SomeClassValue for the class's
  -- native Haskell type, when it's convertible.
  --
  -- GeneralizedNewtypeDeriving is to enable automatic deriving of
  -- Data.Bits.Bits instances for bitspace newtypes.
  "{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, " ++
  "MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances #-}\n\n"

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
  ExportVariable v -> sayExportVar mode v
  ExportEnum enum -> sayExportEnum mode enum
  ExportBitspace bitspace -> sayExportBitspace mode bitspace
  ExportFn fn ->
    (sayExportFn mode <$> fnExtName <*> pure Nothing <*> fnPurity <*> fnParams <*> fnReturn) fn
  ExportClass cls -> sayExportClass mode cls
  ExportCallback cb -> sayExportCallback mode cb

sayExportVar :: SayExportMode -> Variable -> Generator ()
sayExportVar mode v = do
  withErrorContext ("generating variable " ++ show (varExtName v)) $ do
    let (isConst, deconstType) = case varType v of
          TConst t -> (True, t)
          t -> (False, t)
    sayExportFn mode (varGetterExtName v) Nothing Nonpure [] deconstType
    unless isConst $
      sayExportFn mode (varSetterExtName v) Nothing Nonpure [deconstType] TVoid

sayExportEnum :: SayExportMode -> CppEnum -> Generator ()
sayExportEnum mode enum =
  withErrorContext ("generating enum " ++ show (enumExtName enum)) $
  case mode of
    -- Nothing to import from the C++ side of an enum.
    SayExportForeignImports -> return ()

    SayExportDecls -> do
      let hsTypeName = toHsEnumTypeName enum
          values :: [(Int, String)]
          values = map (second $ toHsEnumCtorName enum) $ enumValueNames enum
      addImports $ mconcat [hsImports "Prelude" ["($)", "(++)"], hsImportForPrelude]

      -- Print out the data declaration.
      ln
      addExport' hsTypeName
      saysLn ["data ", hsTypeName, " ="]
      indent $ do
        forM_ (zip (False:repeat True) values) $ \(cont, (_, hsCtorName)) ->
          saysLn [if cont then "| " else "", hsCtorName]
        sayLn "deriving (HoppyP.Bounded, HoppyP.Eq, HoppyP.Ord, HoppyP.Show)"

      -- Print out the Enum instance.
      ln
      saysLn ["instance HoppyP.Enum ", hsTypeName, " where"]
      indent $ do
        forM_ values $ \(num, hsCtorName) ->
          saysLn ["fromEnum ", hsCtorName, " = ", show num]
        ln
        forM_ values $ \(num, hsCtorName) ->
          saysLn ["toEnum ", show num, " = ", hsCtorName]
        -- TODO Fix the potential name collision of 'n'.
        saysLn ["toEnum n = HoppyP.error $ ",
                show (concat ["Unknown ", hsTypeName, " numeric value: "]),
                " ++ HoppyP.show n"]

    SayExportBoot -> do
      let hsTypeName = toHsEnumTypeName enum
      addImports hsImportForPrelude
      addExport hsTypeName
      ln
      saysLn ["data ", hsTypeName]
      saysLn ["instance HoppyP.Bounded ", hsTypeName]
      saysLn ["instance HoppyP.Enum ", hsTypeName]
      saysLn ["instance HoppyP.Eq ", hsTypeName]
      saysLn ["instance HoppyP.Ord ", hsTypeName]
      saysLn ["instance HoppyP.Show ", hsTypeName]

sayExportBitspace :: SayExportMode -> Bitspace -> Generator ()
sayExportBitspace mode bitspace =
  withErrorContext ("generating bitspace " ++ show (bitspaceExtName bitspace)) $
  let hsTypeName = toHsBitspaceTypeName bitspace
      fromFnName = toHsBitspaceToNumName bitspace
      className = toHsBitspaceClassName bitspace
      toFnName = toHsBitspaceFromValueName bitspace
      hsType = HsTyCon $ UnQual $ HsIdent hsTypeName
  in case mode of
    -- Nothing to import from the C++ side of a bitspace.
    SayExportForeignImports -> return ()

    SayExportDecls -> do
      let values :: [(Int, String)]
          values = map (second $ toHsBitspaceValueName bitspace) $ bitspaceValueNames bitspace

      hsNumType <- cppTypeToHsTypeAndUse HsHsSide $ bitspaceType bitspace

      -- Print out the data declaration and conversion functions.
      addImports $ mconcat [hsImportForBits, hsImportForPrelude]
      addExport' hsTypeName
      addExport' className
      ln
      saysLn ["newtype ", hsTypeName, " = ", hsTypeName, " { ",
              fromFnName, " :: ", prettyPrint hsNumType, " }"]
      indent $ sayLn "deriving (HoppyDB.Bits, HoppyP.Bounded, HoppyP.Eq, HoppyP.Ord, HoppyP.Show)"
      ln
      saysLn ["class ", className, " a where"]
      indent $ do
        let tyVar = HsTyVar $ HsIdent "a"
        saysLn [toFnName, " :: ", prettyPrint $ HsTyFun tyVar hsType]
      ln
      saysLn ["instance ", className, " (", prettyPrint hsNumType, ") where"]
      indent $ saysLn [toFnName, " = ", hsTypeName]

      -- If the bitspace has an associated enum, then print out a conversion
      -- instance for it as well.
      forM_ (bitspaceEnum bitspace) $ \enum -> do
        let enumTypeName = toHsEnumTypeName enum
        importHsModuleForExtName $ enumExtName enum
        addImports $ mconcat [hsImport1 "Prelude" "(.)", hsImportForPrelude, hsImportForSupport]
        ln
        saysLn ["instance ", className, " ", enumTypeName, " where"]
        indent $
          saysLn [toFnName, " = ", hsTypeName, " . HoppyFHRS.coerceIntegral . HoppyP.fromEnum"]

      -- Print out the constants.
      ln
      forM_ values $ \(num, valueName) -> do
        addExport valueName
        saysLn [valueName, " = ", hsTypeName, " ", show num]

    SayExportBoot -> do
      hsNumType <- cppTypeToHsTypeAndUse HsHsSide $ bitspaceType bitspace

      addImports $ mconcat [hsImportForBits, hsImportForPrelude]
      addExport' hsTypeName
      addExport' className
      ln
      saysLn ["newtype ", hsTypeName, " = ", hsTypeName, " { ",
              fromFnName, " :: ", prettyPrint hsNumType, " }"]
      ln
      saysLn ["instance HoppyDB.Bits ", hsTypeName]
      saysLn ["instance HoppyP.Bounded ", hsTypeName]
      saysLn ["instance HoppyP.Eq ", hsTypeName]
      saysLn ["instance HoppyP.Ord ", hsTypeName]
      saysLn ["instance HoppyP.Show ", hsTypeName]
      ln
      saysLn ["class ", className, " a where"]
      indent $ do
        let tyVar = HsTyVar $ HsIdent "a"
        saysLn [toFnName, " :: ", prettyPrint $ HsTyFun tyVar hsType]
      ln
      saysLn ["instance ", className, " (", prettyPrint hsNumType, ")"]
      forM_ (bitspaceEnum bitspace) $ \enum -> do
        let enumTypeName = toHsEnumTypeName enum
        importHsModuleForExtName $ enumExtName enum
        saysLn ["instance ", className, " ", enumTypeName]

sayExportFn :: SayExportMode
            -> ExtName
            -> Maybe (Constness, Class)
            -> Purity
            -> [Type]
            -> Type
            -> Generator ()
sayExportFn mode name methodInfo purity paramTypes retType =
  let hsFnName = toHsFnName name
      hsFnImportedName = hsFnName ++ "'"
  in case mode of
    SayExportForeignImports ->
      withErrorContext ("generating imports for function " ++ show name) $ do
        -- Print a "foreign import" statement.
        hsCType <- fnToHsTypeAndUse HsCSide methodInfo purity paramTypes retType
        saysLn ["foreign import ccall \"", externalNameToCpp name, "\" ", hsFnImportedName, " :: ",
                prettyPrint hsCType]

    SayExportDecls -> withErrorContext ("generating function " ++ show name) $ do
      -- Print the type signature.
      ln
      addExport hsFnName
      hsHsType <- fnToHsTypeAndUse HsHsSide methodInfo purity paramTypes retType
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
                   return [" = HoppySIU.unsafePerformIO $"]
      saysLn $ hsFnName : map (' ':) argNamesWithThis ++ lineEnd
      indent $ do
        forM_ (zip3 paramTypes argNames convertedArgNames) $ \(t, argName, argName') ->
          sayArgProcessing ToCpp t argName argName'

        sayCallAndProcessReturn ToCpp retType $
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
sayExportCallback mode cb =
  withErrorContext ("generating callback " ++ show (callbackExtName cb)) $ do
    let name = callbackExtName cb
        paramTypes = callbackParams cb
        retType = callbackReturn cb
        fnType = callbackToTFn cb
        hsFnName = toHsCallbackCtorName cb
        hsFnName'newCallback = hsFnName ++ "'newCallback"
        hsFnName'newFunPtr = hsFnName ++ "'newFunPtr"

    hsFnCType <- cppTypeToHsTypeAndUse HsCSide fnType
    hsFnHsType <- cppTypeToHsTypeAndUse HsHsSide fnType

    let getWholeFnType = do
          addImports $ mconcat [hsImportForPrelude, hsImportForSupport]
          return $
            HsTyFun hsFnHsType $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
            HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyFHRS.CCallback") hsFnCType

    case mode of
      SayExportForeignImports -> do
        addImports $ mconcat [hsImportForForeign, hsImportForPrelude, hsImportForSupport]
        let hsFunPtrType = HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.FunPtr") hsFnCType
            hsFunPtrImportType =
              HsTyFun hsFnCType $
              HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") hsFunPtrType
            hsCallbackCtorImportType =
              HsTyFun hsFunPtrType $
              HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.FunPtr") $
                       HsTyFun (HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyF.FunPtr") $
                                HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
                                HsTyCon $ Special HsUnitCon) $
                       HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
                       HsTyCon $ Special HsUnitCon) $
              HsTyFun (HsTyCon $ UnQual $ HsIdent "HoppyP.Bool") $
              HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") $
              HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyFHRS.CCallback") hsFnCType

        saysLn ["foreign import ccall \"wrapper\" ", hsFnName'newFunPtr, " :: ",
                prettyPrint hsFunPtrImportType]
        saysLn ["foreign import ccall \"", externalNameToCpp name, "\" ", hsFnName'newCallback, " :: ",
                prettyPrint hsCallbackCtorImportType]

      SayExportDecls -> do
        addExport hsFnName
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
                  forM_ (zip3 paramTypes argNames argNames') $ \(t, argName, argName') ->
                    sayArgProcessing FromCpp t argName argName'
                  sayCallAndProcessReturn FromCpp retType $
                    "f'hs" : map (' ':) argNames']
            Nothing
          saysLn ["f'p <- ", hsFnName'newFunPtr, " f'c"]
          saysLn [hsFnName'newCallback, " f'p HoppyFHRS.freeHaskellFunPtrFunPtr HoppyP.False"]

      SayExportBoot -> do
        addExport hsFnName
        wholeFnType <- getWholeFnType
        ln
        saysLn [hsFnName, " :: ", prettyPrint wholeFnType]

data CallDirection =
  ToCpp  -- ^ Haskell code is calling out to C++.
  | FromCpp  -- ^ C++ is invoking a callback.

sayArgProcessing :: CallDirection -> Type -> String -> String -> Generator ()
sayArgProcessing dir t fromVar toVar =
  withErrorContext ("processing argument of type " ++ show t) $
  case t of
    TVar _ -> throwError $ freeVarErrorMsg Nothing t
    TVoid -> throwError $ "TVoid is not a valid argument type"
    TBool -> case dir of
      ToCpp -> saysLn ["let ", toVar, " = if ", fromVar, " then 1 else 0 in"]
      FromCpp -> do addImports $ hsImport1 "Prelude" "(/=)"
                    saysLn ["let ", toVar, " = ", fromVar, " /= 0 in"]
    TChar -> noConversion
    TUChar -> noConversion
    TShort -> noConversion
    TUShort -> noConversion
    TInt -> noConversion
    TUInt -> noConversion
    TLong -> noConversion
    TULong -> noConversion
    TLLong -> noConversion
    TULLong -> noConversion
    TFloat -> noConversion
    TDouble -> noConversion
    TPtrdiff -> noConversion
    TSize -> noConversion
    TSSize -> noConversion
    TEnum _ -> do
      addImports $ mconcat [hsImport1 "Prelude" "($)", hsImportForPrelude, hsImportForSupport]
      saysLn ["let ", toVar,
              -- TODO The coersion here is unnecssary if we replace the C numeric
              -- types with their Haskell ones across the board (e.g. CInt ->
              -- Int).
              case dir of
                ToCpp -> " = HoppyFHRS.coerceIntegral $ HoppyP.fromEnum "
                FromCpp -> " = HoppyP.toEnum $ HoppyFHRS.coerceIntegral ",
              fromVar, " in"]
    TBitspace b -> do
      importHsModuleForExtName $ bitspaceExtName b
      saysLn ["let ", toVar, " = ", bitspaceConvFn dir b, " ", fromVar, " in"]
    -- References and pointers are handled equivalently.
    TPtr (TObj cls) -> do
      addImportForClass cls
      saysLn $ case dir of
        ToCpp -> ["let ", toVar, " = ", toHsCastMethodName Nonconst cls, " ", fromVar, " in"]
        FromCpp -> ["let ", toVar, " = ", fromVar, " in"]
    TPtr (TConst (TObj cls)) -> case dir of
      ToCpp -> do
        addImports $ mconcat [hsImport1 "Prelude" "($)"]
        addImportForClass cls
        saysLn [toHsWithValuePtrName cls, " ", fromVar, " $ \\", toVar, " ->"]
      FromCpp ->
        saysLn ["let ", toVar, " = ", fromVar, " in"]
    TPtr _ -> noConversion
    TRef t' -> sayArgProcessing dir (TPtr t') fromVar toVar
    TFn {} -> throwError "TFn unimplemented"
    TCallback cb -> case dir of
      ToCpp -> do
        addImports $ hsImport1 "Prelude" "(>>=)"
        importHsModuleForExtName $ callbackExtName cb
        saysLn [toHsCallbackCtorName cb, " ", fromVar, " >>= \\", toVar, " ->"]
      FromCpp -> throwError "Can't receive a callback from C++"
    TObj cls -> case dir of
      ToCpp -> do
        addImports $ mconcat [hsImport1 "Prelude" "($)"]
        addImportForClass cls
        saysLn [toHsWithValuePtrName cls, " ", fromVar, " $ \\", toVar, " ->"]
      FromCpp -> do
        addImports $ mconcat [hsImport1 "Prelude" "(>>=)", hsImportForSupport]
        saysLn ["HoppyFHRS.decode ", fromVar, " >>= \\", toVar, " ->"]
    TObjToHeap cls -> case dir of
      ToCpp -> throwError $ tObjToHeapWrongDirectionErrorMsg Nothing cls
      FromCpp -> noConversion
    TConst t' -> sayArgProcessing dir t' fromVar toVar
  where noConversion = saysLn ["let ", toVar, " = ", fromVar, " in"]
        bitspaceConvFn dir = case dir of
          ToCpp -> toHsBitspaceToNumName
          FromCpp -> toHsBitspaceFromValueName

-- | Note that the 'CallDirection' is the direction of the call, not the
-- direction of the return.  'ToCpp' means we're returning to the foreign
-- language, 'FromCpp' means we're returning from it.
sayCallAndProcessReturn :: CallDirection -> Type -> [String] -> Generator ()
sayCallAndProcessReturn dir t callWords =
  withErrorContext ("processing return value of type " ++ show t) $
  case t of
    TVar _ -> throwError $ freeVarErrorMsg Nothing t
    TVoid -> sayCall
    TBool -> do
      case dir of
        ToCpp -> do addImports $ mconcat [hsImport1 "Prelude" "(/=)", hsImportForPrelude]
                    sayLn "HoppyP.fmap (/= 0)"
        FromCpp -> sayLn "(\\x -> if x then 1 else 0)"
      sayCall
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
    TPtrdiff -> sayCall
    TSize -> sayCall
    TSSize -> sayCall
    TEnum _ -> do
      addImports $ mconcat [hsImport1 "Prelude" "(.)", hsImportForPrelude, hsImportForSupport]
      case dir of
        -- TODO The coersion here is unnecssary if we replace the C numeric types
        -- with their Haskell ones across the board (e.g. CInt -> Int).
        ToCpp -> saysLn ["HoppyP.fmap (HoppyP.toEnum . HoppyFHRS.coerceIntegral)"]
        FromCpp -> saysLn ["HoppyP.fmap (HoppyFHRS.coerceIntegral . HoppyP.fromEnum)"]
      sayCall
    TBitspace b -> do
      addImports hsImportForPrelude
      importHsModuleForExtName $ bitspaceExtName b
      saysLn ["HoppyP.fmap ", bitspaceConvFn dir b]
      sayCall
    TPtr _ -> sayCall
    TRef _ -> sayCall
    TFn {} -> throwError "TFn unimplemented"
    TCallback cb -> case dir of
      ToCpp -> throwError "Can't receive a callback from C++"
      FromCpp -> do
        addImports $ hsImport1 "Prelude" "(=<<)"
        importHsModuleForExtName $ callbackExtName cb
        saysLn [toHsCallbackCtorName cb, "=<<"]
        sayCall
    TObj _ -> do
      addImports $ mconcat [hsImport1 "Prelude" "(=<<)", hsImportForSupport]
      case dir of
        ToCpp -> sayLn "HoppyFHRS.decodeAndDelete =<<"
        FromCpp -> sayLn "HoppyFHRS.encode =<<"
      sayCall
    TObjToHeap cls -> case dir of
      ToCpp -> sayCall
      FromCpp -> throwError $ tObjToHeapWrongDirectionErrorMsg Nothing cls
    TConst t' -> sayCallAndProcessReturn dir t' callWords
  where sayCall = saysLn $ "(" : callWords ++ [")"]
        bitspaceConvFn dir = case dir of
          ToCpp -> toHsBitspaceFromValueName
          FromCpp -> toHsBitspaceToNumName

sayExportClass :: SayExportMode -> Class -> Generator ()
sayExportClass mode cls = do
  case mode of
    SayExportForeignImports -> do
      sayExportClassHsCtors mode cls

      forM_ (classMethods cls) $ \method ->
        (sayExportFn mode <$> getClassyExtName cls <*> pure Nothing <*> methodPurity <*>
         pure (getMethodEffectiveParams cls method) <*> methodReturn) method

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

  sayExportClassCastPrimitives mode cls
  sayExportClassHsSpecialFns mode cls

sayExportClassHsClass :: Bool -> Class -> Constness -> Generator ()
sayExportClassHsClass doDecls cls cst = do
  let hsTypeName = toHsDataTypeName cst cls
      hsValueClassName = toHsValueClassName cls
      hsWithValuePtrName = toHsWithValuePtrName cls
      hsPtrClassName = toHsPtrClassName cst cls
      hsCastMethodName = toHsCastMethodName cst cls
      supers = classSuperclasses cls

  forM_ supers $ importHsModuleForExtName . classExtName
  hsSupers <-
    (\x -> if null x
           then do addImports hsImportForSupport
                   return ["HoppyFHRS.CppPtr"]
           else return x) $
    case cst of
      Const -> map (toHsPtrClassName Const) supers
      Nonconst -> toHsPtrClassName Const cls : map (toHsPtrClassName Nonconst) supers

  -- Print the value class definition.  There is only one of these, and it is
  -- spiritually closer to the const version of the pointers for this class, so
  -- we emit for the const case only.
  when (cst == Const) $ do
    addImports hsImportForPrelude
    addExport' hsValueClassName
    ln
    saysLn ["class ", hsValueClassName, " a where"]
    indent $
      saysLn [hsWithValuePtrName, " :: a -> (", hsTypeName, " -> HoppyP.IO b) -> HoppyP.IO b"]

    -- Generate instances for all pointer subtypes.
    ln
    saysLn ["instance {-# OVERLAPPABLE #-} ", hsPtrClassName, " a => ", hsValueClassName, " a",
            if doDecls then " where" else ""]
    when doDecls $ do
      addImports $ mconcat [hsImports "Prelude" ["($)", "(.)"],
                            hsImportForPrelude]
      indent $ saysLn [hsWithValuePtrName, " = HoppyP.flip ($) . ", hsCastMethodName]

    -- When the class has a native Haskell type, also print an instance for it.
    forM_ (classHaskellConversion $ classConversions cls) $ \conv -> do
      hsType <- classHaskellConversionType conv
      ln
      saysLn ["instance {-# OVERLAPPING #-} ", hsValueClassName, " (", prettyPrint hsType, ")",
              if doDecls then " where" else ""]
      when doDecls $ do
        addImports hsImportForSupport
        indent $ saysLn [hsWithValuePtrName, " = HoppyFHRS.withCppObj"]

  -- Print the pointer class definition.
  addExport' hsPtrClassName
  ln
  saysLn $
    "class (" :
    intersperse ", " (map (++ " this") hsSupers) ++
    [") => ", hsPtrClassName, " this where"]
  indent $ saysLn [hsCastMethodName, " :: this -> ", hsTypeName]

  -- Print the non-static methods.
  when doDecls $ do
    let methods = filter ((cst ==) . methodConst) $ classMethods cls
    forM_ methods $ \method ->
      when (methodStatic method == Nonstatic) $
      (sayExportFn SayExportDecls <$> getClassyExtName cls <*> pure Nothing <*>
       methodPurity <*> pure (getMethodEffectiveParams cls method) <*>
       methodReturn) method

sayExportClassHsStaticMethods :: Class -> Generator ()
sayExportClassHsStaticMethods cls =
  forM_ (classMethods cls) $ \method ->
    when (methodStatic method == Static) $
    (sayExportFn SayExportDecls <$> getClassyExtName cls <*> pure Nothing <*> methodPurity <*>
     methodParams <*> methodReturn) method

sayExportClassHsType :: Bool -> Class -> Constness -> Generator ()
sayExportClassHsType doDecls cls cst = do
  addImports $ mconcat [hsImportForForeign, hsImportForSupport]
  -- Unfortunately, we must export the data constructor, so that GHC can marshal
  -- it in foreign calls in other modules.
  addExport' hsTypeName
  ln
  saysLn ["newtype ", hsTypeName, " = ", hsTypeName, " (HoppyF.Ptr ", hsTypeName, ")"]

  -- Generate const_cast functions:
  --   castFooToConst :: Foo -> FooConst
  --   castFooToNonconst :: FooConst -> Foo
  ln
  let constCastFnName = toHsConstCastFn cst cls
  addExport constCastFnName
  saysLn [constCastFnName, " :: ", toHsDataTypeName (constNegate cst) cls, " -> ", hsTypeName]
  when doDecls $ do
    addImports $ hsImport1 "Prelude" "(.)"
    saysLn [constCastFnName, " = ", hsTypeName, " . HoppyF.castPtr . HoppyFHRS.toPtr"]

  -- Generate an instance of CppPtr.
  ln
  if doDecls
    then do saysLn ["instance HoppyFHRS.CppPtr ", hsTypeName, " where"]
            saysLn ["  toPtr (", hsTypeName, " ptr) = ptr"]
            deleteTail <- case cst of
              Const -> return []
              Nonconst -> do addImports $ hsImport1 "Prelude" "(.)"
                             return [" . ", toHsCastMethodName Const cls]
            saysLn $ "  delete = " : toHsClassDeleteFnName cls : deleteTail
    else saysLn ["instance HoppyFHRS.CppPtr ", hsTypeName]

  -- Generate instances for all superclasses' typeclasses.
  genInstances [] cls

  where hsTypeName :: String
        hsTypeName = toHsDataTypeName cst cls

        genInstances :: [Class] -> Class -> Generator ()
        genInstances path ancestorCls = do
          let path' = ancestorCls : path

          -- Unconditionally generate an instance of the const typeclass.
          when (null path) $ addImports $ hsImportForPrelude
          addImportForClass ancestorCls
          -- Use the fact that we've imported all of the classes from cls to
          -- ancestorCls.
          saysLn ["instance ", toHsPtrClassName Const ancestorCls, " ", hsTypeName,
                  if doDecls then " where" else ""]
          when doDecls $ indent $ do
            let convertToConst = cst == Nonconst
                castToSelf = null path
            fns <- (\x -> if null x
                          then do addImports $ hsImportForPrelude
                                  return ["HoppyP.id"]
                          else return x) $
                   catMaybes
                   [ if castToSelf then Nothing else Just $ toHsCastPrimitiveName cls ancestorCls
                   , if convertToConst then Just $ toHsConstCastFn Const cls else Nothing
                   ]
            case fns of
              _:_:_ -> addImports $ hsImport1 "Prelude" "(.)"
              _ -> return ()
            saysLn $ toHsCastMethodName Const ancestorCls : " = " :
                     intersperse " . " fns

          -- Generate an instance of the nonconst typeclass if we're working on
          -- a nonconst pointer type.
          when (cst == Nonconst) $ do
            saysLn ["instance ", toHsPtrClassName Nonconst ancestorCls, " ", hsTypeName,
                    if doDecls then " where" else ""]
            let name = toHsCastMethodName Nonconst ancestorCls
            when doDecls $ indent $
              if null path
              then do addImports hsImportForPrelude
                      saysLn [name, " = HoppyP.id"]
              else do addImports $ hsImport1 "Prelude" "(.)"
                      saysLn [name, " = ",
                              toHsConstCastFn Nonconst ancestorCls, " . ",
                              toHsCastPrimitiveName cls ancestorCls, " . ",
                              toHsConstCastFn Const cls]

          forM_ (classSuperclasses ancestorCls) $ genInstances path'

        toHsConstCastFn :: Constness -> Class -> String
        toHsConstCastFn cst' cls' =
          concat ["cast", toHsDataTypeName Nonconst cls',
                  case cst' of
                    Const -> "ToConst"
                    Nonconst -> "ToNonconst"]

sayExportClassHsNull :: Class -> Generator ()
sayExportClassHsNull cls = do
  let clsHsNullName = toHsClassNullName cls
  addImports hsImportForForeign
  addExport clsHsNullName
  ln
  saysLn [clsHsNullName, " :: ", toHsDataTypeName Nonconst cls]
  saysLn [clsHsNullName, " = ", toHsDataTypeName Nonconst cls, " HoppyF.nullPtr"]

sayExportClassHsCtors :: SayExportMode -> Class -> Generator ()
sayExportClassHsCtors mode cls =
  forM_ (classCtors cls) $ \ctor ->
  (sayExportFn mode <$> getClassyExtName cls <*> pure Nothing <*>
   pure Nonpure <*> ctorParams <*> pure (TPtr $ TObj cls)) ctor

sayExportClassHsSpecialFns :: SayExportMode -> Class -> Generator ()
sayExportClassHsSpecialFns mode cls = do
  -- Say the delete function.
  case mode of
    SayExportForeignImports -> do
      addImports hsImportForPrelude
      saysLn ["foreign import ccall \"", classDeleteFnCppName cls, "\" ",
              toHsClassDeleteFnName cls, " :: ", toHsDataTypeName Const cls,
              " -> HoppyP.IO ()"]
    -- The user interface to this is the generic 'delete' function, rendered
    -- elsewhere.
    SayExportDecls -> return ()
    SayExportBoot -> return ()

  -- Say Encodable and Decodable instances, if the class is encodable and
  -- decodable.
  forM_ (classHaskellConversion $ classConversions cls) $ \conv -> do
    hsType <- classHaskellConversionType conv
    let hsTypeStr = concat ["(", prettyPrint hsType, ")"]
        typeName = toHsDataTypeName Nonconst cls
        typeNameConst = toHsDataTypeName Const cls
    case mode of
      SayExportForeignImports -> return ()

      SayExportDecls -> do
        addImports $ mconcat [hsImportForPrelude, hsImportForSupport]

        -- Say the Encodable instances.
        ln
        saysLn ["instance HoppyFHRS.Encodable ", typeName, " (", hsTypeStr, ") where"]
        indent $ do
          sayLn "encode ="
          indent $ classHaskellConversionToCppFn conv
        ln
        saysLn ["instance HoppyFHRS.Encodable ", typeNameConst, " (", hsTypeStr, ") where"]
        indent $
          saysLn ["encode = HoppyP.fmap (", toHsCastMethodName Const cls,
                  ") . HoppyFHRS.encodeAs (HoppyP.undefined :: ", typeName, ")"]

        -- Say the Decodable instances.
        ln
        saysLn ["instance HoppyFHRS.Decodable ", typeName, " (", hsTypeStr, ") where"]
        indent $
          saysLn ["decode = HoppyFHRS.decode . ", toHsCastMethodName Const cls]
        ln
        saysLn ["instance HoppyFHRS.Decodable ", typeNameConst, " (", hsTypeStr, ") where"]
        indent $ do
          sayLn "decode ="
          indent $ classHaskellConversionFromCppFn conv

      SayExportBoot -> do
        addImports hsImportForSupport
        ln
        saysLn ["instance HoppyFHRS.Encodable ", typeName, " (", hsTypeStr, ")"]
        saysLn ["instance HoppyFHRS.Encodable ", typeNameConst, " (", hsTypeStr, ")"]
        saysLn ["instance HoppyFHRS.Decodable ", typeName, " (", hsTypeStr, ")"]
        saysLn ["instance HoppyFHRS.Decodable ", typeNameConst, " (", hsTypeStr, ")"]

sayExportClassCastPrimitives :: SayExportMode -> Class -> Generator ()
sayExportClassCastPrimitives mode cls = do
  let fromType = toHsDataTypeName Const cls
  case mode of
    SayExportForeignImports -> do
      forAncestors cls $ \super -> do
        let hsCastFnName = toHsCastPrimitiveName cls super
            toType = toHsDataTypeName Const super
        addExport hsCastFnName
        saysLn [ "foreign import ccall \"", classCastFnCppName cls super
               , "\" ", hsCastFnName, " :: ", fromType, " -> ", toType
               ]

    SayExportDecls -> return ()

    SayExportBoot -> do
      forAncestors cls $ \super -> do
        let hsCastFnName = toHsCastPrimitiveName cls super
            toType = toHsDataTypeName Const super
        addExport hsCastFnName
        saysLn [hsCastFnName, " :: ", fromType, " -> ", toType]

  where forAncestors :: Class -> (Class -> Generator ()) -> Generator ()
        forAncestors cls' f = forM_ (classSuperclasses cls') $ \super -> do
          f super
          forAncestors super f

fnToHsTypeAndUse :: HsTypeSide
                 -> Maybe (Constness, Class)
                 -> Purity
                 -> [Type]
                 -> Type
                 -> Generator HsQualType
fnToHsTypeAndUse side methodInfo purity paramTypes returnType = do
  params <- mapM contextForParam $
            (case methodInfo of
                Just (cst, cls) -> [("this", case cst of
                                        Nonconst -> TPtr $ TObj cls
                                        Const -> TPtr $ TConst $ TObj cls)]
                Nothing -> []) ++
            zip (map toArgName [1..]) paramTypes
  let context = mapMaybe fst params :: HsContext
      hsParams = map snd params
  hsReturn <- cppTypeToHsTypeAndUse side returnType
  hsReturnForPurity <- case (purity, side) of
    (Pure, HsHsSide) -> return hsReturn
    _ -> do
      addImports hsImportForPrelude
      return $ HsTyApp (HsTyCon $ UnQual $ HsIdent "HoppyP.IO") hsReturn
  return $ HsQualType context $ foldr HsTyFun hsReturnForPurity hsParams

  where contextForParam :: (String, Type) -> Generator (Maybe HsAsst, HsType)
        contextForParam (s, t) = case t of
          TPtr (TObj cls) -> receivePtr s cls Nonconst
          TPtr (TConst (TObj cls)) -> receiveValue s t cls
          TRef (TObj cls) -> receivePtr s cls Nonconst
          TRef (TConst (TObj cls)) -> receiveValue s t cls
          TObj cls -> receiveValue s t cls
          TConst t' -> contextForParam (s, t')
          _ -> handoff side t

        -- Use whatever type 'cppTypeToHsTypeAndUse' suggests, with no typeclass
        -- constraints.
        handoff :: HsTypeSide -> Type -> Generator (Maybe HsAsst, HsType)
        handoff side t = (,) Nothing <$> cppTypeToHsTypeAndUse side t

        -- Receive a @FooPtr this => this@.
        receivePtr :: String -> Class -> Constness -> Generator (Maybe HsAsst, HsType)
        receivePtr s cls cst = do
          addImportForClass cls
          return $ case side of
            HsHsSide -> let t' = HsTyVar $ HsIdent s
                        in (Just (UnQual $ HsIdent $ toHsPtrClassName cst cls, [t']),
                            t')
            HsCSide -> (Nothing, HsTyVar $ HsIdent $ toHsDataTypeName cst cls)

        -- Receive a @FooValue a => a@.
        receiveValue :: String -> Type -> Class -> Generator (Maybe HsAsst, HsType)
        receiveValue s t cls = case side of
          HsCSide -> handoff side t
          HsHsSide -> do
            addImports hsImportForSupport
            addImportForClass cls
            let t' = HsTyVar $ HsIdent s
            return (Just (UnQual $ HsIdent $ toHsValueClassName cls, [t']),
                    t')

getMethodEffectiveParams :: Class -> Method -> [Type]
getMethodEffectiveParams cls method =
  (case methodApplicability method of
     MNormal -> (TPtr (TObj cls):)
     MConst -> (TPtr (TConst $ TObj cls):)
     MStatic -> id) $
  methodParams method

-- | Imports bindings for the given class into the Haskell module.
addImportForClass :: Class -> Generator ()
addImportForClass = importHsModuleForExtName . classExtName
