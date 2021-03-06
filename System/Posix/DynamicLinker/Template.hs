{-# LANGUAGE TemplateHaskell #-}

module System.Posix.DynamicLinker.Template
   ( makeDynamicLinker
   , Callconv(..)
   , DL
   , FunPtr
  )
where

import System.Posix.DynamicLinker

import Language.Haskell.TH.Syntax

import Control.Monad (when, unless, join)
import Foreign.Ptr
import Foreign.C.String
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import Data.Map (fromList, lookup)
import Data.Maybe (isNothing, fromJust, Maybe)
import Data.Functor (fmap)

-- | Generate dynamic linking FFI methods for each field in the first parameter
makeDynamicLinker :: Name -- ^ Name of the data type
  -> Callconv             -- ^ Calling convention: CCall or StdCall
  -> Name                 -- ^ Name of the function used to transform symbol names
  -> Q [Dec]
makeDynamicLinker dt callconv symMod = do

  -- reify and check that the name refers to a data type
  info <- reify dt
  reified <- case info of
     TyConI dec -> return dec
     _          -> fail $ errmsg dt

  -- check that there is a single constructor
  (name,cons) <- case reified of
     DataD [] _ [] _ [RecC name cons'] _    -> return (name,cons')
     NewtypeD [] _ [] _ (RecC name cons') _ -> return (name,cons')
     _                                      -> fail $ errmsg dt

  -- Check for a field named "libHandle"
  unless (any ((==) "libHandle" . extractName) cons) $
     qReport True (nolibhandlemsg dt)

  -- Exclude "libHandle" from the symbol list
  let symbols = filter ((/=) "libHandle" . extractName) cons

  maybeType <- [t| Data.Maybe.Maybe |]
  funptr    <- [t| Foreign.Ptr.FunPtr |]

  -- Get symbol names and optionality
  let names                 = map (\ (n,_,_) -> n) symbols
  let (optionals,realTypes) = unzip $ map (\(_,_,t) -> isMaybe maybeType t) symbols
  let symbolsE              = ListE $ map (\ (Name occ _) -> LitE $ StringL $ occString occ) names

  -- Generate names for foreign import functions
  makes <- mapM (newName . ("make_" ++) . extractName) symbols

  -- Generate foreign calls
  foreigns <- mapM (\(t,mk) -> makeForeign funptr t mk) (realTypes `zip` makes)
  
  -- Show a warning if no foreign call has been generated
  when (null foreigns) $ qReport False (nodefmsg dt)

  -- Generate loader
  loader <- makeLoader name names optionals makes symbolsE

  return (foreigns ++ loader)

  where
    -- | Indicate if a type is surrounded by Maybe and return real type
    isMaybe :: Type -> Type -> (Bool,Type)
    isMaybe maybeType (AppT mb t) | mb == maybeType = (True,t)
    isMaybe _ t = (False,t)

    loadName = transformNameLocal ("load" ++) dt

    extractName (Name name _, _, _) = occString name

    -- | Transform a name using the given function
    transformName :: (String -> String) -> Name -> Name
    transformName namer (Name occ f) = Name newNam f
      where newNam = mkOccName $ namer $ occString occ

    -- | Transform a name using the given function and make it local
    transformNameLocal :: (String -> String) -> Name -> Name
    transformNameLocal namer n = Name occ NameS
      where
        Name occ _ = transformName namer n
        
    -- | Generate a foreign declaration
    makeForeign :: Type -> Type -> Name -> Q Dec
    makeForeign fptr typ mk = do
      let importTyp = AppT (AppT ArrowT (AppT fptr typ)) typ
      return (ForeignD (ImportF callconv Safe "dynamic" mk importTyp))

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

            let 
               fromFunPtr a = if a == nullFunPtr then Nothing else Just a
               pick a = join $ fmap (fromFunPtr . castFunPtr) $ Data.Map.lookup a symPtrs
               missingmsg name lib' = "Mandatory symbol \"" ++ name ++ "\" was not found in " ++ lib'
               checkSym (name,opt) = when (not opt && isNothing (pick name)) $ error (missingmsg name lib)

            -- Check that the mandatory symbols are present
            traverse_ checkSym (zip modSymbols $(lift $ optionals))

            -- Fill the structure
            return $( do
                hdl <- [| dl |]
                let handleField = (Name (mkOccName "libHandle") NameS, hdl)
                pick' <- [| pick |]
                fm <- [| Data.Functor.fmap |]
                fds <- traverse (\(sym,isOpt,mk) -> makeField mk isOpt pick' fm sym) (zip3 names optionals makes)
                return $ RecConE t (handleField:fds)
              ) 
        |]
      
      -- loadXXX signature
      sigType <- [t| FilePath -> [RTLDFlags] -> IO $(return $ ConT $ transformNameLocal id t) |]
      -- loadXXX body
      let load = FunD loadName [Clause [] (NormalB body) []]
      -- return declarations
      return [SigD loadName sigType,load]
 
    -- Get a string literal from a name
    literalize (Name occ _) = LitE $ StringL $ occString occ

    -- | Create a record field for a symbol
    makeField :: Name -> Bool -> Exp -> Exp -> Name -> Q FieldExp
    makeField mk isOptional pick fm name = do
      op <- if isOptional then [| id |] else [| fromJust |]

      return (name, AppE op . AppE (AppE fm (VarE mk)) . AppE pick . AppE (VarE symMod) $ literalize name)

    nodefmsg t = "Warning: No dynamic linker method generated from the name " ++ show t

    nolibhandlemsg t = "You must add a field 'libHandle :: System.Posix.DynamicLinker.Prim.DL' in your data " ++ show t

    errmsg t = "Cannot derive dynamic linker methods for name " ++ show t ++ " because"
           ++ "\n it is not a type declared with 'data' or 'newtype'"
           ++ "\n or because it has more than one constructor"
           ++ "\n Did you remember to double-tick the type as in"
           ++ "\n $(makeDynamicLinker ''TheType)?"

