{-# LANGUAGE DeriveGeneric #-}
module Cryptol.ModuleSystem.Name where

import GHC.Generics (Generic)
import Control.DeepSeq

import qualified Data.Text as Text

type Ident = Text.Text

pack :: String -> Ident
pack = Text.pack

unpack :: Ident -> String
unpack = Text.unpack

-- | Module names are just namespaces.
--
-- INVARIANT: the list of strings should never be empty in a valid module name.
newtype ModName = ModName [Ident]
                  deriving (Eq,Ord,Show,Generic)

instance NFData ModName


data Name     = Name Ident
              | NewName Pass Int
               deriving (Eq,Ord,Show,Generic)

instance NFData Name

data QName    = QName (Maybe ModName) Name
               deriving (Eq,Ord,Show,Generic)

instance NFData QName

unModName :: ModName -> [String]
unModName (ModName ns) = map unpack ns

mkModName :: [String] -> ModName
mkModName ns = ModName (map pack ns)

mkName :: String -> Name
mkName n = Name (pack n)

-- XXX It would be nice to also mark this as a name that doesn't need to be
-- resolved, if it's going to be created before renaming.
mkPrim :: String -> QName
mkPrim n = mkQual (ModName [pack "Cryptol"]) (Name (pack n))

asPrim :: QName -> Maybe String
asPrim (QName (Just (ModName [m])) (Name n))
  | m == pack "Cryptol" = Just (unpack n)
asPrim _ = Nothing

mkQual :: ModName -> Name -> QName
mkQual  = QName . Just

mkUnqual :: Name -> QName
mkUnqual  = QName Nothing

unqual :: QName -> Name
unqual (QName _ n) = n


data Pass = NoPat | MonoValues
  deriving (Eq,Ord,Show,Generic)

instance NFData Pass
