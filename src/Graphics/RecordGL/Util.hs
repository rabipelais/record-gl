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
    overFields :: (forall b. b -> c) -> a -> [c]

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
        overFieldsFun = FunD (mkName "overFields")
                             [Clause [] (NormalB (VarE (mkName "undefined"))) []]
    in InstanceD context
                 (AppT (ConT (mkName "HasFields")) recordType)
                 [fieldNamesFun, overFieldsFun]
