{-# LANGUAGE TemplateHaskell, CPP #-}

module System.Posix.DynamicLinker.Template (
  makeDynamicLinker
  ) where

import Language.Haskell.TH.Syntax
import Control.Monad (liftM, when)
import Data.List (nub)

import System.Posix.DynamicLinker
import Foreign.Ptr
import Foreign.C.String
import Data.Traversable(traverse)
import Data.Map (fromList,lookup)
import Data.Maybe (fromMaybe)

makeDynamicLinker :: Name -> Q [Dec]
makeDynamicLinker t = do
  info <- reify t
  reified <- case info of
                  TyConI dec -> return dec
                  _ -> fail $ errmsg t
  decMakeDynamicLinker t reified

decMakeDynamicLinker :: Name -> Dec -> Q [Dec]
decMakeDynamicLinker t dec = do
  (name,cons) <- case dec of
               DataD [] _ [] [RecC name cons'] _ -> return (name,cons')
               NewtypeD [] _ [] (RecC name cons') _ -> return (name,cons')
               _ -> fail $ errmsg t
  when (not $ any (\(Name name _,_,_) -> occString name == "libHandle")  cons) $ qReport True (nolibhandlemsg t)
  let cons2 = filter (\(Name name _,_,_) -> occString name /= "libHandle") cons
  decs <- makeDL name cons2
  when (null decs) $ qReport False (nodefmsg t)
  return decs


makeDL :: Name -> [VarStrictType] -> Q [Dec]
makeDL s vars = do
  foreigns <- mapM (\ (name,_,ftype) -> makeForeign name ftype) vars
  loader <- makeLoader s $ map (\ (n,_,_) -> n) vars
  return $ concat [foreigns,loader]

transformName :: (String -> String) -> Name -> Name
transformName namer (Name occ f) = Name newName f
  where newName = mkOccName $ namer $ occString occ
    
nameMake :: Name -> Name
nameMake = transformName ("make_" ++) 

makeForeign :: Name -> Type -> Q Dec
makeForeign name typ = do
  --FIXME: make callconv parametric 
  let n = nameMake name
  let fptr = mkName "Foreign.Ptr.FunPtr"
  return . ForeignD $ ImportF CCall Safe "dynamic" n (AppT (AppT ArrowT (AppT (ConT fptr) typ)) typ)

makeLoader :: Name -> [Name] -> Q [Dec]
makeLoader t ss = do
  body <- [|
      \lib -> do
        dl <- dlopen lib [RTLD_NOW,RTLD_LOCAL]
        let symbls = $(return symbols)
        let mydlsym = \s -> withCAString s $ c_dlsym (packDL dl)
        symPtrs <- traverse mydlsym symbls
        let syms = fromList $ symbls `zip` symPtrs
        let pick = \a -> fmap castFunPtr $ Data.Map.lookup a syms
        let unsafePick  = \a -> fromMaybe nullFunPtr $ pick a
        let notFound = \a -> error ("Mandatory symbol \"" ++ a ++ "\" not found in " ++ lib)
        let mandatory = \a -> if (pick a == Nothing) then notFound a else unsafePick a
        return $ $(fmap libHandle [| dl |])

    |]
  let load = FunD loadName [Clause [] (NormalB body) []]
  return [load]

  where
    symbols = ListE $ map (LitE . StringL) (map (\ (Name occ _) -> occString occ) ss)
    makes = map nameMake ss
    loadName = transformName ("load" ++) t
    mand = VarE $ Name (mkOccName "mandatory") NameS
    fields = map (\(field@(Name occ _),mk) -> (field, AppE (VarE mk) (AppE mand (LitE $ StringL $ occString occ)))) $ zip ss makes
    libHandle x =  RecConE t ((Name (mkOccName "libHandle") NameS, x) : fields)


nodefmsg t = "Warning: No dynamic linker method generated from the name " ++ show t

nolibhandlemsg t = "You must add a field 'libHandle :: System.Posix.DynamicLinker.Prim.DL' in your data " ++ show t

errmsg t = "Cannot derive dynamic linker methods for name " ++ show t ++ " because"
       ++ "\n it is not a type declared with 'data' or 'newtype'"
       ++ "\n Did you remember to double-tick the type as in"
       ++ "\n $(makeDynamicLinker ''TheType)?"
