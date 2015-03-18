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
import           Graphics.Rendering.OpenGL (UniformLocation)
import qualified Graphics.Rendering.OpenGL as GL
import           Language.Haskell.TH
import           Record
import           Record.Types

-- | Provide the 'GL.VariableType' of each field in a 'Record'. The list
-- of types has the same order as the fields of the 'Record'.
class HasFieldGLTypes a where
    fieldGLTypes :: a -> [GL.VariableType]

-- | Zips up lists of 'UniformLocation's and a 'Record' setting
-- uniform parameters using the record fields.
class SetUniformFields a where
  setUniformFields :: [Maybe UniformLocation] -> a -> IO ()

-- I would rather have written this by hand and some Emacs macros...
-- Instances for the HasFieldGLTypes class
-- Example implementation:
-- fieldGLTypes (RecordX v1...vn) = [variableType v1,...,variableType v2]
return $ flip map [1..24] $ \arity ->
  let typeName = mkName $ "Record" <> show arity
      recordType = foldl (\a i -> AppT (AppT a (VarT (mkName ("n" <> show i))))
                                       (VarT (mkName ("v" <> show i))))
                         (ConT typeName)
                         [1 .. arity]
      vVals = map (\i -> "v" <> show i) [1..arity]
      vTVals = map (VarT . mkName) vVals
      context = map (\v -> ClassP (mkName "HasVariableType") [v]) vTVals
      typesList = ListE $ map (AppE (VarE (mkName "variableType")) .
                                    SigE (VarE (mkName "undefined")))
                          vTVals
      fieldGLTypesFun = FunD (mkName "fieldGLTypes")
                             [Clause [WildP] (NormalB typesList) []]
  in InstanceD context
               (AppT (ConT (mkName "HasFieldGLTypes")) recordType)
               [fieldGLTypesFun]

-- Instances for the SetUniformFields class
-- Example implementation:
-- setUniformFields (u1 : ...) (RecordX v1...) = traverse_ (asUniform v1) u1 >> ...
return $ flip map [1..24] $ \arity ->
    let typeName = mkName $ "Record" <> show arity
        recordType = foldl (\a i -> AppT (AppT a (VarT (mkName ("n" <> show i))))
                                         (VarT (mkName ("v" <> show i))))
                           (ConT typeName)
                           [1 .. arity]
        typePattern = ConP typeName (map (\i -> VarP (mkName ("v" <> show i))) [1..arity])
        vNames = map (\i -> mkName $ "v" <> show i) [1..arity]
        vTVals = map VarT vNames
        context = map (\v -> ClassP (mkName "AsUniform") [v]) vTVals
        nameE = VarE . mkName
        uniformNames = map (\i -> mkName ("u" <> show i)) [1..arity + 1]
        uniformPat = AsP (mkName "us") $ foldr1 (\i a -> InfixP i (mkName ":") a) $ map VarP uniformNames
        asUniforms = foldr1 (\i a -> InfixE (Just i) (nameE ">>") (Just a)) $ zipWith go us vs
          where
            us = map VarE uniformNames
            go u v = AppE (AppE (nameE "traverse_")
                                (AppE (nameE "asUniform") v))
                          u
            vs = map VarE vNames
        wrongSizes = NormalG $ InfixE (Just (AppE (nameE "length")
                                                  (nameE "us")))
                                      (VarE (mkName "<"))
                                      (Just (LitE (IntegerL arity))) -- I wish this could be done with types
        contrarily = NormalG $ nameE "otherwise"
        setUniformFieldsFun =
            FunD (mkName "setUniformFields")
                 [Clause [uniformPat, typePattern]
                  (GuardedB [(wrongSizes, AppE (nameE "error")
                                               (LitE (StringL "Not enough UniformLocations :(")))
                            ,(contrarily, asUniforms)]) []]
    in InstanceD context
                 (AppT (ConT (mkName "SetUniformFields")) recordType)
                 [setUniformFieldsFun]

type UniformFields a = (HasFieldGLTypes a, SetUniformFields a)

-- | Set GLSL uniform parameters form a `Record` representing
-- a subset of all uniform parameters used by a program.
setUniforms :: forall rs. UniformFields rs => IO rs
setUniforms = undefined
