{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Introspect field names and other attributes of a "Record".
module Record.Introspection  where

import           BasePrelude         hiding (Proxy)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
import           Data.Proxy
#endif
import           Foreign.Storable    (Storable (sizeOf))
import           GHC.TypeLits
import           Language.Haskell.TH
import           Record.Types

-- | List all field names in a record.
class HasFields a where
    fieldNames :: a -> [String]

return $ flip map [1..24] $ \arity ->
    let typeName = mkName $ "Record" <> show arity
        recordType = foldl (\a i -> AppT (AppT a (SigT (VarT (mkName ("n" <> show i))) (ConT ''Symbol)))
                                         (VarT (mkName ("v" <> show i))))
                           (ConT typeName)
                           [1 .. arity]
        nVals = map (\i -> "n" <> show i) [1..arity]
        nTVals = map (VarT . mkName) nVals
        fieldNames' = ListE $ flip map nTVals
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
                    (\n -> AppE (VarE (mkName "symbolVal"))
                           (SigE (ConE (mkName "Proxy")) (AppT (ConT (mkName "Proxy")) n)))
        context = map (\n -> ClassP (mkName "KnownSymbol") [n]) nTVals
#else
                    (\n -> AppE (VarE (mkName "fromSing"))
                           (SigE (VarE (mkName "sing")) (AppT (ConT (mkName "Proxy")) n)))
        context = map (\n -> ClassP (mkName "SingI") [n]) nTVals
#endif
        fieldNamesFun = FunD (mkName "fieldNames")
                             [Clause [WildP] (NormalB fieldNames') []]
    in InstanceD context
                 (AppT (ConT (mkName "HasFields")) recordType)
                 [fieldNamesFun]

-- | Compute the dimensionality of each field in a record. This is
-- primarily useful for things like the small finite vector types
-- provided by "Linear".
class HasFieldDims a where
    fieldDims :: a -> [Int]

return $ flip map [1..24] $ \arity ->
    let typeName = mkName $ "Record" <> show arity
        recordType = foldl (\a i -> AppT (AppT a (SigT (VarT (mkName ("n" <> show i))) (ConT ''Symbol)))
                                         (AppT (VarT (mkName ("l" <> show i))) (VarT (mkName ("v" <> show i)))))
                           (ConT typeName)
                           [1 .. arity]
        vVals = map (\i -> "v" <> show i) [1..arity]
        vTVals = map (VarT . mkName) vVals
        lVals =  map (\i -> "l" <> show i) [1..arity]
        lTVals = map (VarT . mkName) lVals
        vectorTVals = zipWith AppT lTVals vTVals
        numCxt = map (\i -> ClassP (mkName "Num") [i]) vectorTVals
        foldCxt = map (\i -> ClassP (mkName "Foldable") [i]) lTVals
        context = foldCxt ++ numCxt
        nameE = VarE . mkName
        fieldDims' = ListE $ flip map vectorTVals
                   (\v -> nameE "getSum" `AppE`
                         (nameE "foldMap" `AppE`
                          (nameE "const" `AppE`
                           (ConE (mkName "Sum") `AppE` (LitE (IntegerL 1)))) `AppE`
                         (SigE (LitE (IntegerL 0)) v)))
        fieldDimsFun = FunD (mkName "fieldDims")
                            [Clause [WildP]
                            (NormalB fieldDims') []]
    in InstanceD context
                 (AppT (ConT (mkName "HasFieldDims")) recordType)
                 [fieldDimsFun]

-- | Compute the size in bytes of of each field in a record.
class HasFieldSizes a where
  fieldSizes :: a -> [Int]

return $ flip map [1..24] $ \arity ->
    let typeName = mkName $ "Record" <> show arity
        recordType = foldl (\a i -> AppT (AppT a (SigT (VarT (mkName ("n" <> show i))) (ConT ''Symbol)))
                                         (VarT (mkName ("v" <> show i))))
                           (ConT typeName)
                           [1 .. arity]
        vVals = map (\i -> "v" <> show i) [1..arity]
        vTVals = map (VarT . mkName) vVals
        context = map (\v -> ClassP (mkName "Storable") [v]) vTVals
        fieldSizes' = ListE $ flip map vTVals
                    (\v -> AppE (VarE (mkName "sizeOf"))
                           (SigE (VarE (mkName "undefined")) v))
        fieldSizesFun = FunD (mkName "fieldSizes")
                             [Clause [WildP] (NormalB fieldSizes') []]
    in InstanceD context
                 (AppT (ConT (mkName "HasFieldSizes")) recordType)
                 [fieldSizesFun]
