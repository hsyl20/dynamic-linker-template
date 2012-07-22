{-# LANGUAGE TemplateHaskell, CPP #-}

module System.Posix.DynamicLinker.Template (
  makeDynamicLinker, Callconv(..)
  ) where

import Language.Haskell.TH.Syntax
import Control.Monad (liftM, when, unless, liftM2)
import Data.List (nub)
import Data.Functor ( (<$>) )

import System.Posix.DynamicLinker
import Foreign.Ptr
import Foreign.C.String
import Data.Traversable(traverse)
import Data.Map (fromList,lookup)
import Data.Maybe (fromMaybe, isNothing)

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

  -- Generate foreign calls
  foreigns <- mapM (\ (n,_,ftype) -> makeForeign n ftype) symbols
  
  -- Show a warning if no foreign call has been generated
  when (null foreigns) $ qReport False (nodefmsg t)

  -- Generate loader
  loader <- makeLoader name $ map (\ (n,_,_) -> n) symbols

  return (loader ++ foreigns)

  where
    -- | Transform a name using the given function
    transformName :: (String -> String) -> Name -> Name
    transformName namer (Name occ f) = Name newName f
      where newName = mkOccName $ namer $ occString occ

    -- | Transform a name using the given function and make it local
    transformNameLocal :: (String -> String) -> Name -> Name
    transformNameLocal namer n = Name occ NameS
      where
        Name occ _ = transformName namer n
        
    -- | Name transformer for foreign functions
    foreignNameTransformer :: Name -> Name
    foreignNameTransformer = transformNameLocal ("make_" ++) 

    -- | Generate a foreign declaration
    makeForeign :: Name -> Type -> Q Dec
    makeForeign name typ = do
      let n = foreignNameTransformer name
      let fptr = mkName "Foreign.Ptr.FunPtr"
      return . ForeignD $ ImportF callconv Safe "dynamic" n (AppT (AppT ArrowT (AppT (ConT fptr) typ)) typ)

    -- | Generate module loader function
    makeLoader :: Name -> [Name] -> Q [Dec]
    makeLoader t ss = do
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

            -- Modified symbol name -> Ptr a (may be null)
            let unsafePick a = fromMaybe nullFunPtr $ pick a

            -- Error displayed when a mandatory symbol is not found
            let notFound a = error ("Mandatory symbol \"" ++ a ++ "\" not found in " ++ lib)

            -- Modified symbol name -> Ptr a, or error if symbol is not found
            let mandatory a = if isNothing (pick a) then notFound a else unsafePick a

            -- Fill the structure
            return $( do
                hdl <- [| dl |]
                let handleField = (Name (mkOccName "libHandle") NameS, hdl)
                mand <- [| mandatory |]
                opt <- [| pick |]
                modif <- [| modify |]
                fds <- traverse (makeField True mand opt modif) ss
                return $ RecConE t (handleField:fds)
              ) 
        |]
      let load = FunD loadName [Clause [] (NormalB body) []]
      return [load]

      where
        symbols = ListE $ map (\ (Name occ _) -> LitE $ StringL $ occString occ) ss
        makes = map foreignNameTransformer ss
        loadName = transformNameLocal ("load" ++) t

    -- | Create a record field for a symbol
    makeField :: Bool -> Exp -> Exp -> Exp -> Name -> Q FieldExp
    makeField isMandatory mand opt modify name = do
      let literalize (Name occ _) = LitE $ StringL $ occString occ -- Get a string literal from a name
      let mk = AppE (VarE $ foreignNameTransformer name) -- Foreign function call
      let mandatory = AppE mand -- Mandatory check call
      let optional = AppE opt -- Optional check call

      let op = if isMandatory then mandatory else optional

      return $ (name, mk $ op $ AppE modify $ literalize name)
    


    nodefmsg t = "Warning: No dynamic linker method generated from the name " ++ show t

    nolibhandlemsg t = "You must add a field 'libHandle :: System.Posix.DynamicLinker.Prim.DL' in your data " ++ show t

    errmsg t = "Cannot derive dynamic linker methods for name " ++ show t ++ " because"
           ++ "\n it is not a type declared with 'data' or 'newtype'"
           ++ "\n Did you remember to double-tick the type as in"
           ++ "\n $(makeDynamicLinker ''TheType)?"
