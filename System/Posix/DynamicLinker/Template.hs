{-# LANGUAGE TemplateHaskell, CPP #-}

module System.Posix.DynamicLinker.Template (
  makeDynamicLinker, Callconv(..), DL, id, FunPtr
  ) where

import Language.Haskell.TH.Syntax
import Control.Monad (liftM, when, unless, liftM2, void, join)
import Data.List (nub)
import Data.Functor ( (<$>), fmap )

import System.Posix.DynamicLinker
import Foreign.Ptr
import Foreign.C.String
import Data.Traversable(traverse)
import Data.Map (fromList,lookup)
import Data.Maybe (fromMaybe, isNothing, fromJust,Maybe)

-- | Generate dynamic linking FFI methods for each field in the first parameter
makeDynamicLinker :: Name -- ^ Name of the data type
  -> Callconv             -- ^ Calling convention: CCall or StdCall
  -> Name                 -- ^ Name of the function used to transform symbol names
  -> Q [Dec]
makeDynamicLinker t callconv symMod = do
  info <- reify t
  reified <- case info of
                TyConI dec -> return dec
                _ -> fail $ errmsg t

  (name,cons) <- case reified of
               DataD [] _ [] [RecC name cons'] _ -> return (name,cons')
               NewtypeD [] _ [] (RecC name cons') _ -> return (name,cons')
               _ -> fail $ errmsg t

  -- Check for a field named "libHandle"
  unless (any (\(Name name _,_,_) -> occString name == "libHandle")  cons) $ qReport True (nolibhandlemsg t)

  -- Exclude "libHandle" from the symbol list
  let symbols = filter (\(Name name _,_,_) -> occString name /= "libHandle") cons

  maybeType <- [t| Data.Maybe.Maybe |]
  funptr <- [t| Foreign.Ptr.FunPtr |]

  -- Get symbol names and optionality
  let names = map (\ (n,_,_) -> n) symbols
  let (optionals,realTypes) = unzip $ map (\(_,_,t) -> isMaybe maybeType t) symbols
  let symbolsE = ListE $ map (\ (Name occ _) -> LitE $ StringL $ occString occ) names

  -- Generate names for foreign import functions
  makes <- mapM (\((Name occ _),_,_) -> newName $ "make_" ++ occString occ) symbols

  -- Generate foreign calls
  foreigns <- mapM (\ (n,t,mk) -> makeForeign funptr n t mk) (zip3 names realTypes makes)
  
  -- Show a warning if no foreign call has been generated
  when (null foreigns) $ qReport False (nodefmsg t)

  -- Generate loader
  loader <- makeLoader name names optionals makes symbolsE

  return (foreigns ++ loader)

  where
    -- | Indicate if a type is surrounded by Maybe and return real type
    isMaybe :: Type -> Type -> (Bool,Type)
    isMaybe maybeType (AppT mb t) | mb == maybeType = (True,t)
    isMaybe _ t = (False,t)

    loadName = transformNameLocal ("load" ++) t

    -- | Transform a name using the given function
    transformName :: (String -> String) -> Name -> Name
    transformName namer (Name occ f) = Name newName f
      where newName = mkOccName $ namer $ occString occ

    -- | Transform a name using the given function and make it local
    transformNameLocal :: (String -> String) -> Name -> Name
    transformNameLocal namer n = Name occ NameS
      where
        Name occ _ = transformName namer n
        
    -- | Generate a foreign declaration
    makeForeign :: Type -> Name -> Type -> Name -> Q Dec
    makeForeign fptr name typ mk = do
      return . ForeignD $ ImportF callconv Safe "dynamic" mk (AppT (AppT ArrowT (AppT fptr typ)) typ)

    -- | Generate module loader function
    makeLoader :: Name -> [Name] -> [Bool] -> [Name] -> Exp -> Q [Dec]
    makeLoader t names optionals makes symbolsE = do
      body <- [| \lib flags -> do
            -- Load the library
            dl <- dlopen lib flags

            -- Symbol list
            let symbls = $(return symbolsE)

            -- Transform symbol names
            let modSymbols = fmap $(return $ VarE symMod) symbls

            -- Load symbols
            let mydlsym s = withCAString s $ c_dlsym (packDL dl)
            ptrs <- traverse mydlsym modSymbols

            -- Associative Map: Modified symbol name -> Ptr () (may be null)
            let symPtrs = fromList $ modSymbols `zip` ptrs

            let fromFunPtr a = if a == nullFunPtr then Nothing else Just a

            -- Modified symbol name -> Maybe (Ptr a) 
            let pick a = join $ fmap (fromFunPtr . castFunPtr) $ Data.Map.lookup a symPtrs

            void $ traverse (\(name,opt) -> do
                when (not opt && isNothing (pick name)) $ error ("Mandatory symbol \"" ++ name ++ "\" was not found in " ++ lib)
              ) (zip modSymbols $(lift $ optionals))

            -- Fill the structure
            return $( do
                hdl <- [| dl |]
                let handleField = (Name (mkOccName "libHandle") NameS, hdl)
                pick <- [| pick |]
                fm <- [| Data.Functor.fmap |]
                fds <- traverse (\(sym,isOpt,mk) -> makeField mk isOpt pick fm sym) (zip3 names optionals makes)
                return $ RecConE t (handleField:fds)
              ) 
        |]
      sigType <- [t| FilePath -> [RTLDFlags] -> IO $(return $ ConT $ transformNameLocal id t) |]
      let load = FunD loadName [Clause [] (NormalB body) []]
      return [SigD loadName sigType,load]

    literalize (Name occ _) = LitE $ StringL $ occString occ -- Get a string literal from a name

    -- | Create a record field for a symbol
    makeField :: Name -> Bool -> Exp -> Exp -> Name -> Q FieldExp
    makeField mk isOptional pick fm name = do
      op <- if isOptional then [| id |] else [| fromJust |]

      return (name, AppE op $ AppE (AppE fm (VarE mk)) $ AppE pick $ AppE (VarE symMod) $ literalize name)


    nodefmsg t = "Warning: No dynamic linker method generated from the name " ++ show t

    nolibhandlemsg t = "You must add a field 'libHandle :: System.Posix.DynamicLinker.Prim.DL' in your data " ++ show t

    errmsg t = "Cannot derive dynamic linker methods for name " ++ show t ++ " because"
           ++ "\n it is not a type declared with 'data' or 'newtype'"
           ++ "\n Did you remember to double-tick the type as in"
           ++ "\n $(makeDynamicLinker ''TheType)?"
