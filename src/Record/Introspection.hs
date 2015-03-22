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
class HasFieldNames a where
    fieldNames :: a -> [String]

return $ flip map [1..24] $ \arity ->
    let typeName = mkName $ "Record" <> show arity
        recordType = foldl (\a i -> AppT (AppT a (SigT (VarT (mkName ("n" <> show i))) (ConT ''Symbol)))
                                         (VarT (mkName ("v" <> show i))))
                           (ConT typeName)
                           [1 .. arity]
        nVals = map (\i -> "n" <> show i) [1..arity]
        nTVals = map (VarT . mkName) nVals
#if MIN_VERSION_template_haskell(2,10,0)
        mkContext c t = AppT (ConT (mkName c)) t
#else
        mkContext c t = ClassP (mkName c) [t]
#endif
        fieldNames' = ListE $ flip map nTVals
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
                    (\n -> AppE (VarE (mkName "symbolVal"))
                           (SigE (ConE (mkName "Proxy")) (AppT (ConT (mkName "Proxy")) n)))
        context = map (\n -> mkContext "KnownSymbol" n) nTVals
#else
                    (\n -> AppE (VarE (mkName "fromSing"))
                           (SigE (VarE (mkName "sing")) (AppT (ConT (mkName "Proxy")) n)))
        context = map (\n -> mkContext "SingI" n) nTVals
#endif
        fieldNamesFun = FunD (mkName "fieldNames")
                             [Clause [WildP] (NormalB fieldNames') []]
    in InstanceD context
                 (AppT (ConT (mkName "HasFieldNames")) recordType)
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
        numCxt = map (\i -> mkContext "Num" i) vectorTVals
        foldCxt = map (\i -> mkContext "Foldable" i) lTVals
        context = foldCxt ++ numCxt
        nameE = VarE . mkName
#if MIN_VERSION_template_haskell(2,10,0)
        mkContext c t = AppT (ConT (mkName c)) t
#else
        mkContext c t = ClassP (mkName c) [t]
#endif
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
#if MIN_VERSION_template_haskell(2,10,0)
        mkContext c t = AppT (ConT (mkName c)) t
#else
        mkContext c t = ClassP (mkName c) [t]
#endif
        context = map (\v -> mkContext "Storable" v) vTVals
        fieldSizes' = ListE $ flip map vTVals
                    (\v -> AppE (VarE (mkName "sizeOf"))
                           (SigE (VarE (mkName "undefined")) v))
        fieldSizesFun = FunD (mkName "fieldSizes")
                             [Clause [WildP] (NormalB fieldSizes') []]
    in InstanceD context
                 (AppT (ConT (mkName "HasFieldSizes")) recordType)
                 [fieldSizesFun]
