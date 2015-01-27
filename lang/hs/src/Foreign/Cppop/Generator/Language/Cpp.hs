module Foreign.Cppop.Generator.Language.Cpp (
  Generation,
  generate,
  generatedHeader,
  generatedSource,
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (when)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Trans (lift)
import Data.Foldable (forM_)
import Data.List (intercalate, intersperse)
import Data.Tuple (swap)
import Foreign.Cppop.Generator.Spec

cppNameSeparator :: String
cppNameSeparator = "__"

makeCppName :: [String] -> String
makeCppName = intercalate cppNameSeparator

-- | "geniface" is used for functions that create C++ Interface objects.
externalInterfaceFnNamePrefix :: String
externalInterfaceFnNamePrefix = "geniface"

interfaceCppFnName :: Interface -> String
interfaceCppFnName interface = makeCppName [externalInterfaceFnNamePrefix, interfaceName interface]

-- | "genpop" is used for individually exported functions.
externalNamePrefix :: String
externalNamePrefix = "genpop"

externalNameToCpp :: ExtName -> String
externalNameToCpp extName =
  makeCppName [externalNamePrefix, fromExtName extName]

-- | A chunk is a string that contains an arbitrary portion of C++ code.  The
-- only requirement is that chunk boundaries are also C++ token boundaries,
-- because the generator monad automates the process of inserting whitespace
-- between chunk boundaries where necessary.
data Chunk = Chunk
  { chunkContents :: String
  }

type Generator = ReaderT Env (WriterT [Chunk] (Either String))

data Env = Env
  { envInterface :: Interface
  }

askInterface :: Generator Interface
askInterface = fmap envInterface ask

askIncludes :: Generator [Include]
askIncludes = fmap (interfaceIncludes . envInterface) ask

askExports :: Generator [Export]
askExports = fmap (interfaceExports . envInterface) ask

-- | Halts generation and returns the given error message.
abort :: String -> Generator a
abort = lift . lift . Left

runGenerator :: Interface -> Generator a -> Either String (String, a)
runGenerator interface =
  fmap (first combineChunks . swap) . runWriterT . flip runReaderT (Env interface)

-- | Flattens a list of chunks down into a single string.  Inserts spaces
-- between chunks where the ends of adjacent chunks would otherwise merge into a
-- single C++ token.
combineChunks :: [Chunk] -> String
combineChunks chunks =
  let strs = map chunkContents chunks
  in concat $ flip map (zip ("":strs) strs) $ \(prev, cur) ->
       let needsSpace =
             not (null prev) && not (null cur) &&
             (let a = last prev
                  b = head cur
              in -- "intconstx" should become "int const x"
                 isIdentifierChar a && isIdentifierChar b ||
                 -- Adjacent template parameter '>'s need spacing in old C++.
                 a == '>' && b == '>')
       in if needsSpace then ' ':cur else cur

-- | Emits a single 'Chunk'.
say :: String -> Generator ()
say = tell . (:[]) . Chunk

-- | Emits a 'Chunk' for each string in a list.
says :: [String] -> Generator ()
says = tell . map Chunk

sayIdentifier :: Identifier -> Generator ()
sayIdentifier = say . idToString

sayInterfaceHeader :: Generator ()
sayInterfaceHeader = do
  interface <- askInterface
  sayCommonIncludes
  says ["\n::cppop::Interface const*", interfaceCppFnName interface, "();\n"]

sayInterfaceSource :: Generator ()
sayInterfaceSource = do
  sayCommonIncludes
  mapM_ (say . includeToString) =<< askIncludes
  mapM_ sayExport =<< askExports
  sayInterfaceFunction

sayCommonIncludes :: Generator ()
sayCommonIncludes = do
  say "#include <map>\n"
  say "#include <string>\n"
  say "#include \"buffers.h\"\n"
  say "#include \"interface.h\"\n"

sayInterfaceFunction :: Generator ()
sayInterfaceFunction = do
  interface <- askInterface
  let fnName = interfaceCppFnName interface
  sayFunction fnName [] (TFn [] $ TPtr $ TConst $ TOpaque "::cppop::Interface") $ do
    says ["::cppop::Interface* i = new ::cppop::Interface(\"", interfaceName interface, "\");\n"]

    exports <- askExports
    forM_ exports $ \export -> case export of
      ExportFn fn -> sayDefinition $ fnExtName fn
      ExportClass cls -> do
        forM_ (classCtors cls) $ sayDefinition . ctorExtName
        forM_ (classMethods cls) $ sayDefinition . methodExtName

    says [ "i->finish();\n"  -- TODO Check the return value.
         , "return i;\n"
         ]

  where sayDefinition :: ExtName -> Generator ()
        sayDefinition extName =
          says [ "i->define(\""
               , fromExtName extName
               , "\", "
               , externalNameToCpp extName
               , ");\n"
               ]

sayFunction :: String -> [String] -> Type -> Generator () -> Generator ()
sayFunction name paramNames t body = do
  case t of
    TFn {} -> return ()
    _ -> abort $ "sayFunction requires a function type, given: " ++ show t
  say "\n"  -- New top-level structure, leave a blank line.
  sayVar name (Just paramNames) t
  say " {\n"
  body  -- TODO Indent.
  say "}\n"

class HasExternalName a where
  getExternalName :: a -> ExtName

instance HasExternalName Function where
  getExternalName = fnExtName

-- TODO Fixme, this is most likely backwards, it should be a finite set of
-- non-identifier chars.
isIdentifierChar :: Char -> Bool
isIdentifierChar = (`elem` identifierChars)

identifierChars :: String
identifierChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "_"

data Generation = Generation
  { generatedHeader :: String
  , generatedSource :: String
  }

generate :: Interface -> Either String Generation
generate interface = do
  header <- fst <$> runGenerator interface sayInterfaceHeader
  source <- fst <$> runGenerator interface sayInterfaceSource
  return Generation
    { generatedHeader = header
    , generatedSource = source
    }

sayExport :: Export -> Generator ()
sayExport export = case export of
  ExportFn fn ->
    -- Export a single function.
    sayExportFn (fnExtName fn)
                (sayIdentifier $ fnIdentifier fn)
                Nothing
                (fnParams fn)
                (fnReturn fn)
  ExportClass cls -> do
    let clsPtr = TPtr $ TObj cls
        justClsPtr = Just clsPtr
    -- Export each of the class's constructors.
    forM_ (classCtors cls) $ \ctor ->
      sayExportFn (ctorExtName ctor)
                  (say "new" >> sayIdentifier (classIdentifier cls))
                  Nothing
                  (ctorParams ctor)
                  clsPtr
    -- Export each of the class's methods.
    forM_ (classMethods cls) $ \method -> do
      let static = methodStatic method == Static
      sayExportFn (methodExtName method)
                  (do when static $ do
                        sayIdentifier (classIdentifier cls)
                        say "::"
                      say $ methodCName method)
                  (if static then Nothing else justClsPtr)
                  (methodParams method)
                  (methodReturn method)

sayExportFn :: ExtName -> Generator () -> Maybe Type -> [Type] -> Type -> Generator ()
sayExportFn extName sayCppName maybeThisType paramTypes retType = do
  let paramCount = length paramTypes
  sayFunction (externalNameToCpp extName)
              ["args", "out"]
              (TFn [ TPtr $ TOpaque "::cppop::BufferReader"
                   , TPtr $ TOpaque "::cppop::WritableBuffer"
                   ]
                   TVoid) $ do
    -- Maybe extract a this-pointer for a method call.
    forM_ maybeThisType $ \thisType ->
      sayArgRead extName "self" thisType
    -- Extract arguments from the binary RPC data.
    forM_ (zip [0..] paramTypes) $ \(paramIdx, paramType) -> do
      sayArgRead extName ("arg" ++ show paramIdx) paramType
    -- Call the exported function or method.
    let returnsData = retType /= TVoid
    when returnsData $ do
      sayVar "result" Nothing retType
      say " = "
    forM_ maybeThisType $ const $ say "self->"
    sayCppName
    say "("
    sayArgNames paramCount
    say ");\n"
    -- If there is a return value, send it back to the caller.
    when returnsData $ case retType of
      TObj cls -> case classCppEncoder $ classEncoding cls of
        Nothing -> abort $ "No encoder for class " ++ idToString (classIdentifier cls) ++
                   " while generating export " ++ fromExtName extName ++ "."
        Just encoderId -> do
          sayIdentifier encoderId
          say "(result, out);\n"
      _ -> do
        primTypeName <- maybe (abort $ "Can't encode return value of type " ++ show retType ++
                               " for export \"" ++ fromExtName extName ++ "\".")
                              return $
                        primitiveTypeName retType
        says ["*(", primTypeName, "*)out->alloc(sizeof(", primTypeName, ")) = result;\n"]

sayArgRead :: ExtName -> String -> Type -> Generator ()
sayArgRead extName paramVarName paramType = case paramType of
  TObj cls -> case classCppDecoder $ classEncoding cls of
    Nothing -> abort $ "No decoder for class " ++ idToString (classIdentifier cls) ++
               " while generating export \"" ++ fromExtName extName ++ "\"."
    Just decoderId -> do
      sayVar paramVarName Nothing paramType
      say "("
      sayIdentifier decoderId
      say "(args));\n"
  _ -> do
    primTypeName <- maybe (abort $ "Can't decode argument of type " ++ show paramType ++
                           " for export \"" ++ fromExtName extName ++ "\".")
                          return $
                    primitiveTypeName paramType
    sayVar paramVarName Nothing paramType
    says [" = *(", primTypeName, "*)args->read(sizeof(", primTypeName, "));\n"]

sayArgNames :: Int -> Generator ()
sayArgNames count =
  says $ intersperse ", " $ map (("arg" ++) . show) [0..count - 1]

sayVar :: String -> Maybe [String] -> Type -> Generator ()
sayVar name maybeParamNames t = sayType' t maybeParamNames topPrecedence $ say name

sayType' :: Type -> Maybe [String] -> Int -> Generator () -> Generator ()
sayType' t maybeParamNames outerPrec unwrappedOuter =
  let prec = typePrecedence t
      outer = if prec <= outerPrec
              then unwrappedOuter
              else say "(" >> unwrappedOuter >> say ")"
  in case t of
    TVoid -> say "void" >> outer
    TBool -> say "bool" >> outer
    TChar -> say "char" >> outer
    TUChar -> say "unsigned char" >> outer
    TShort -> say "short" >> outer
    TUShort -> say "unsigned short" >> outer
    TInt -> say "int" >> outer
    TUInt -> say "unsigned int" >> outer
    TLong -> say "long" >> outer
    TULong -> say "unsigned long" >> outer
    TLLong -> say "long long" >> outer
    TULLong -> say "unsigned long long" >> outer
    TFloat -> say "float" >> outer
    TDouble -> say "double" >> outer
    TSize -> say "size_t" >> outer
    TSSize -> say "ssize_t" >> outer
    TArray maybeSize t' -> sayType' t' Nothing prec $ do
      outer
      say $ maybe "[]" (\n -> '[' : show n ++ "]") maybeSize
      -- int[]             Array of int.
      -- int*[]            Array of pointers to ints.
      -- int*(*var[x])[y]  Array(x) of pointers to arrays(y) of pointers to ints.  (Ptr to array(y) must be sized.)
      -- int(*[])()        Array of pointers to functions returning ints.
      -- int(**[])()       Array of pointers to pointers to functions returning ints.
    TPtr t' -> sayType' t' Nothing prec $ say "*" >> outer
      -- int*              Pointer to an int.
      -- int(*)[]          Pointer to an array of ints.  (C requires the array be sized.)
      -- int(*)()          Pointer to a function returning an int.
    TRef t' -> sayType' t' Nothing prec $ say "&" >> outer
      -- int&              Reference to an int.
      -- int(&)[]          Reference to an array of ints.  (C requires the array be sized.)
      -- int(&)()          Reference to a function returning an int.
    TFn paramTypes retType -> sayType' retType Nothing prec $ do
      outer
      say "("
      --sequence_ $ intersperse (tell [", "]) $ map sayType paramTypes
      sequence_ $ intersperse (say ", ") $
        flip map (zip paramTypes $ maybe (repeat Nothing) (map Just) maybeParamNames) $ \(ptype, pname) ->
        sayType' ptype Nothing topPrecedence $ forM_ pname $ say
      say ")"
      -- int(*)()          Pointer to a function returning an int.
      -- int(*)()[]        Pointer to a function returning an array of ints.  (Must be sized...)
      -- int*(*)()         Pointer to a function returning a pointer to an int.
      -- int(*(*var)())[]  Pointer to a function returning a pointer to an array of ints.  (Must be sized...)
      -- A function can't return an array.
    TObj cls -> sayIdentifier (classIdentifier cls) >> outer
    TOpaque s -> say s >> outer
    TBlob -> say "void*" >> outer
    TConst t' -> sayType' t' maybeParamNames outerPrec $ say "const" >> unwrappedOuter  -- TODO Is using the outer stuff correctly here?

topPrecedence :: Int
topPrecedence = 11

typePrecedence :: Type -> Int
typePrecedence t = case t of
  TFn {} -> 10
  TArray {} -> 9
  TPtr {} -> 8
  TRef {} -> 8
  _ -> 7

primitiveTypeName :: Type -> Maybe String
primitiveTypeName = primitiveTypeName' False

primitiveTypeName' :: Bool -> Type -> Maybe String
primitiveTypeName' nonPrimitiveOkay t = case t of
  TVoid -> Just "void"
  TBool -> Just "bool"
  TChar -> Just "char"
  TUChar -> Just "unsigned char"
  TShort -> Just "short"
  TUShort -> Just "unsigned short"
  TInt -> Just "int"
  TUInt -> Just "unsigned int"
  TLong -> Just "long"
  TULong -> Just "unsigned long"
  TLLong -> Just "long long"
  TULLong -> Just "unsigned long long"
  TFloat -> Just "float"
  TDouble -> Just "double"
  TSize -> Just "size_t"
  TSSize -> Just "ssize_t"
  TArray {} -> Nothing  -- TODO Hmm.
  TPtr t' -> (++ "*") <$> primitiveTypeName' True t'
  TRef {} -> Nothing  -- TODO Hmm.
  TFn {} -> Nothing  -- TODO Hmm.
  TObj cls | nonPrimitiveOkay -> Just $ idToString $ classIdentifier cls
  TObj _ -> Nothing
  TOpaque str | nonPrimitiveOkay -> Just str
  TOpaque _  -> Nothing
  TBlob -> Nothing  -- TODO Hmm.
  TConst t' -> ("const " ++) <$> primitiveTypeName' nonPrimitiveOkay t'
