{-# LANGUAGE TemplateHaskell, CPP #-}

module System.Posix.DynamicLinker.Template (
  makeDynamicLinker, Callconv(..)
  ) where

import Language.Haskell.TH.Syntax
import Control.Monad (liftM, when, unless, liftM2)
import Data.List (nub)
import Data.Functor ( (<$>), fmap )

import System.Posix.DynamicLinker
import Foreign.Ptr
import Foreign.C.String
import Data.Traversable(traverse)
import Data.Map (fromList,lookup)
import Data.Maybe (fromMaybe, isNothing, fromJust,Maybe)

makeDynamicLinker :: Name -> Callconv -> Name -> Q [Dec]
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

  -- Generate names for foreign import functions
  makes <- mapM (\((Name occ _),_,_) -> newName $ "make_" ++ occString occ) symbols

  -- Generate foreign calls
  foreigns <- mapM (\ (n,t,mk) -> makeForeign funptr n t mk) (zip3 names realTypes makes)
  
  -- Show a warning if no foreign call has been generated
  when (null foreigns) $ qReport False (nodefmsg t)

  -- Generate loader
  loader <- makeLoader name names optionals makes

  return (foreigns ++ loader)

  where
    -- | Indicate if a type is surrounded by Maybe and return real type
    isMaybe :: Type -> Type -> (Bool,Type)
    isMaybe maybeType (AppT mb t) | mb == maybeType = (True,t)
    isMaybe _ t = (False,t)

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
    makeLoader :: Name -> [Name] -> [Bool] -> [Name] -> Q [Dec]
    makeLoader t ss optionals makes = do
      body <- [| \lib flags -> do
            -- Load the library
            dl <- dlopen lib flags

            -- Symbol list
            let symbls = $(return symbols)

            -- Modify a given symbol name using the provided function (if any)
            let modify a = case $(return $ VarE symMod) of {
                Nothing -> a;
                Just f -> f a
              }
            
            -- Transform symbol names
            let modSymbols = fmap modify symbls

            -- Load symbols
            let mydlsym s = withCAString s $ c_dlsym (packDL dl)
            ptrs <- traverse mydlsym modSymbols

            -- Associative Map: Modified symbol name -> Ptr () (may be null)
            let symPtrs = fromList $ modSymbols `zip` ptrs

            -- Modified symbol name -> Maybe (Ptr a)
            let pick a = fmap castFunPtr $ Data.Map.lookup a symPtrs

            -- Modified symbol name -> Ptr a, or error if symbol is not found
            let mandatory s = fromMaybe $ error ("Mandatory symbol \"" ++ s ++ "\" not found in " ++ lib)

            -- Fill the structure
            return $( do
                hdl <- [| dl |]
                let handleField = (Name (mkOccName "libHandle") NameS, hdl)
                mand <- [| mandatory |]
                pick <- [| pick |]
                modif <- [| modify |]
                fm <- [| Data.Functor.fmap |]
                fds <- traverse (\(sym,isOpt,mk) -> makeField mk isOpt mand pick modif fm sym) (zip3 ss optionals makes)
                return $ RecConE t (handleField:fds)
              ) 
        |]
      let load = FunD loadName [Clause [] (NormalB body) []]
      return [load]

      where
        symbols = ListE $ map (\ (Name occ _) -> LitE $ StringL $ occString occ) ss
        loadName = transformNameLocal ("load" ++) t

    -- | Create a record field for a symbol
    makeField :: Name -> Bool -> Exp -> Exp -> Exp -> Exp -> Name -> Q FieldExp
    makeField mk isOptional mand pick modify fm name = do
      let literalize (Name occ _) = LitE $ StringL $ occString occ -- Get a string literal from a name
      let mandatory s a = AppE (AppE mand s) a -- Mandatory check call

      let litName = literalize name
      let op = if isOptional then id else mandatory litName

      return (name, op $ AppE (AppE fm (VarE mk)) $ AppE pick $ AppE modify $ litName)
    


    nodefmsg t = "Warning: No dynamic linker method generated from the name " ++ show t

    nolibhandlemsg t = "You must add a field 'libHandle :: System.Posix.DynamicLinker.Prim.DL' in your data " ++ show t

    errmsg t = "Cannot derive dynamic linker methods for name " ++ show t ++ " because"
           ++ "\n it is not a type declared with 'data' or 'newtype'"
           ++ "\n Did you remember to double-tick the type as in"
           ++ "\n $(makeDynamicLinker ''TheType)?"
