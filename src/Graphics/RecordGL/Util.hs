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

-- |

module Graphics.RecordGL.Util  where

import           BasePrelude         hiding (Proxy)
import           Data.Proxy
import           GHC.TypeLits
import           Language.Haskell.TH
import           Record.Types

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
        context = map (\n -> ClassP (mkName "KnownSymbol") [n]) nTVals
        fieldNames' = ListE $ flip map nTVals
                    (\n -> AppE (VarE (mkName "symbolVal"))
                           (SigE (ConE (mkName "Proxy")) (AppT (ConT (mkName "Proxy")) n)))
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
