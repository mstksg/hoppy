module Foreign.Cppop.Generator.Language.Haskell.General (
  Generator,
  runGenerator,
  execGenerator,
  abort,
  sayQualifiedImports,
  sayLn,
  saysLn,
  ln,
  indent,
  sayLet,
  toHsTypeName,
  toHsClassName,
  toHsCastMethodName,
  toHsDataTypeName,
  toHsClassDeleteFnName,
  toHsCallbackCtorName,
  toHsFnName,
  toArgName,
  HsTypeSide (..),
  encodingTypeForSide,
  cppTypeToHsType,
  prettyPrint,
  ) where

import Control.Applicative ((<$>), (<*))
import Control.Arrow (first)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT, censor, runWriterT, tell)
import Data.Char (toLower, toUpper)
import Data.Foldable (forM_)
import Data.List (intercalate)
import Data.Tuple (swap)
import Foreign.Cppop.Generator.Spec
import qualified Language.Haskell.Pretty as P
import Language.Haskell.Syntax (
  HsName (HsIdent),
  HsQName (Special, UnQual),
  HsSpecialCon (HsUnitCon),
  HsType (HsTyApp, HsTyCon, HsTyFun),
  )

type Generator = WriterT [String] (Either String)

runGenerator :: Generator a -> Either String (String, a)
runGenerator = fmap (first (intercalate "\n") . swap) . runWriterT . (<* ln)

execGenerator :: Generator a -> Either String String
execGenerator = fmap fst . runGenerator

-- | Halts generation and returns the given error message.
abort :: String -> Generator a
abort = lift . Left

sayQualifiedImports :: Generator ()
sayQualifiedImports = do
  sayLn "import qualified Foreign as F"
  sayLn "import qualified Foreign.C as FC"
  sayLn "import qualified Foreign.Cppop.Runtime.Support as FCRS"
  sayLn "import qualified Prelude as P"
  sayLn "import qualified System.IO.Unsafe as SIU"

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
toArgName = ("arg'" ++) . show

data HsTypeSide = HsCSide | HsHsSide

encodingTypeForSide :: HsTypeSide -> HaskellEncoding -> HsType
encodingTypeForSide HsCSide = haskellEncodingCType
encodingTypeForSide HsHsSide = haskellEncodingType

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

-- | Prints a value like 'P.prettyPrint', but removes newlines so that they
-- don't cause problems with this module's textual generation.  Should be mainly
-- used for printing types; stripping newlines from definitions for example
-- could go badly.
prettyPrint :: P.Pretty a => a -> String
prettyPrint = filter (/= '\n') . P.prettyPrint
