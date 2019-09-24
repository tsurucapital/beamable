{-# LANGUAGE TemplateHaskell, LambdaCase, ViewPatterns #-}
module Data.Beamable.TH ( makeBeamable ) where

import Data.Beamable.Internal

import Control.Monad
import Data.Digest.Murmur64
import Data.List (foldl')
import Data.Word
import Language.Haskell.TH
import qualified Data.Set as Set
import Unsafe.Coerce
import Data.Proxy


-- | This module generates 'Beamable' instances using TemplateHaskell, for
-- datatypes with single constructor 'serialize'/'deserialize' should be
-- compatible with oned derived with Generics, for multi constructor datatypes
-- those instances are not compatible. 'typeSignR' won't be compatible as well.

makeBeamable :: Name -> Q [Dec]
makeBeamable name = [d|
        instance Beamable $(conT name) where
            serialize = $(serializeTH name)
            deserialize = $(deserializeTH name)
            typeSignR _ = $(typeSignTH name)
    |]


usage :: String
usage = "usage: deriveBeamable ''Foo, Foo must be a datatype of some sort"

typeSignTH :: Name -> ExpQ
typeSignTH tname = do
    s <- newName "s"
    reify tname >>= \(TyConI dec) -> case dec of
        DataD _ name _ _ cons _      -> comb s tname name (map (signCon s) cons)
        DataInstD _ _ _ _ cons _  -> comb s tname s (map (signCon s) cons)
        NewtypeD _ name _ _ con _    -> comb s tname name ([signCon s con])
        NewtypeInstD _ _ _ _ con _-> comb s tname s ([signCon s con])
        _ -> error usage

comb :: Name -> Name -> Name -> [ExpQ] -> ExpQ
comb s tname name hashes = do
    let n = asWord64 . hash64 $ show name
        h = asWord64 . hash64 $ show tname
    [| \ $(varP s) -> if Set.member (unsafeCoerce (h :: Word64)) $(varE s)
                then id
                else $( foldl' (\acc x -> [| $acc . $x |]) [| hash64Add (n :: Word64) |] hashes )
     |]

signCon :: Name -> Con -> ExpQ
signCon s con = do
    let types = conTypes con
        name = asWord64 . hash64 . show $ conName con
        xs = map (\t -> [| typeSignR (Proxy :: Proxy $(return t)) |]) types
    foldl' (\acc x -> [| $acc . $x $(varE s) |] ) [| hash64Add (name :: Word64) |] xs

serializeTH :: Name -> ExpQ
serializeTH name = reify name >>= \(TyConI dec) -> lamCaseE $ case dec of
    DataD _ _ _ _ [con] _      -> (:[]) $ serializeCon Nothing con
    DataInstD _ _ _ _ [con] _  -> (:[]) $ serializeCon Nothing con
    NewtypeD _ _ _ _ con _     -> (:[]) $ serializeCon Nothing con
    NewtypeInstD _ _ _ _ con _ -> (:[]) $ serializeCon Nothing con
    DataD _ _ _ _ cons _       -> zipWith serializeCon (map Just [0..]) cons
    DataInstD _ _ _ _ cons _   -> zipWith serializeCon (map Just [0..]) cons
    _ -> error usage

deserializeTH :: Name -> ExpQ
deserializeTH tname = reify tname >>= \(TyConI dec) -> case dec of
    DataD _ _ _ _ cons _       -> deserializeCons cons
    DataInstD _ _ _ _ cons _   -> deserializeCons cons
    NewtypeD _ _ _ _ con _     -> deserializeCons [con]
    NewtypeInstD _ _ _ _ con _ -> deserializeCons [con]
    _ -> error usage

conTypes :: Con -> [Type]
conTypes (NormalC _ sts) = map snd sts
conTypes (RecC _ vsts) = map (\(_, _, t) -> t) vsts
conTypes (InfixC a _ b) = [snd a, snd b]
conTypes (ForallC _ _ con) = conTypes con
conTypes _ = error "Existential constructor? I'm sorry, Dave. I'm afraid I can't do that."

conName :: Con -> Name
conName (NormalC name _) = name
conName (RecC name _) = name
conName (InfixC _ name _) = name
conName _ = error "Existential constructor? I'm sorry, Dave. I'm afraid I can't do that."

serializeCon :: Maybe Word -> Con -> MatchQ
serializeCon m'prefix con = do
        vars <- replicateM (length $ conTypes con) (newName "a")
        let pat = conP (conName con) (map varP vars)
            prefix = maybe [] (\w -> [ [| word w |] ] ) m'prefix
            allVals = prefix ++ map put vars
            body = if null allVals
                       then [| mempty |]
                       else foldl1 (\e1 e2 -> [| $e1 <> $e2 |]) allVals
        match pat (NormalB <$> body) []
    where
        put v = [| serialize $( varE v) |]

deserializeCons :: [Con] -> ExpQ
deserializeCons cons = do
        case cons of
             [con] -> getCon con
             _ -> [| getWord >>= $(lamCaseE ((zipWith pickCon [0 :: Int ..] cons) ++ [kaput])) |]
    where
        kaput = Match WildP <$> (NormalB <$> [| error "unknown constructor" |]) <*> pure []
        pickCon idx con = do
            let pat = LitP . IntegerL . fromIntegral $ idx
            (Match pat) <$> (NormalB <$> getCon con) <*> pure []
        getCon con = let c = [| pure $( conE $ conName con ) |]
                      in iterate (\acc -> [| $acc <*> deserialize |] ) c !! (length $ conTypes con)
