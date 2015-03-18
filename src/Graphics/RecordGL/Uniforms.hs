{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Graphics.RecordGL.Uniforms where

import           BasePrelude               hiding (Proxy)
import           GHC.TypeLits              (Symbol)
import           Graphics.GLUtil           (AsUniform (..),
                                            HasVariableType (..),
                                            ShaderProgram (..))
import qualified Graphics.Rendering.OpenGL as GL
import           Language.Haskell.TH
import           Record
import           Record.Types

-- | Provide the 'GL.VariableType' of each field in a 'Record'. The list
-- of types has the same order as the fields of the 'Record'.
class HasFieldGLTypes a where
    fieldGLTypes :: a -> [GL.VariableType]

-- Instances for the HasFieldGLTypes class
return $ flip map [1..24] $ \arity ->
    let typeName = mkName $ "Record" <> show arity
        recordType = foldl (\a i -> AppT (AppT a (VarT (mkName ("n" <> show i))))
                                         (VarT (mkName ("v" <> show i))))
                           (ConT typeName)
                           [1 .. arity]
        typePattern = ConP typeName (map (\i -> VarP (mkName ("v" <> show i))) [1..arity])
        vVals = map (\i -> "v" <> show i) [1..arity]
        vTVals = map (VarT . mkName) vVals
        context = map (\v -> ClassP (mkName "HasVariableType") [v]) vTVals
        typesList = ListE $ flip map vTVals
                    (\v -> AppE (VarE (mkName "variableType"))
                                (SigE (VarE (mkName "undefined")) v))
        fieldGLTypesFun = FunD (mkName "fieldGLTypes")
                               [Clause [WildP] (NormalB typesList) []]
    in InstanceD context
                 (AppT (ConT (mkName "HasFieldGLTypes")) recordType)
                 [fieldGLTypesFun]



type UniformFields a = Eq a

-- | Set GLSL uniform parameters form a `Record` representing
-- a subset of all uniform parameters used by a program.
setUniforms :: forall rs. UniformFields rs => IO rs
setUniforms = undefined
