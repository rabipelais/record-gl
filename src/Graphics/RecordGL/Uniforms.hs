{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Graphics.RecordGL.Uniforms where

import           BasePrelude               hiding (Proxy)
import qualified Data.Map                  as M
import           Graphics.GLUtil           (AsUniform (..),
                                            HasVariableType (..),
                                            ShaderProgram (..))
import           Graphics.Rendering.OpenGL (UniformLocation)
import qualified Graphics.Rendering.OpenGL as GL
import           Language.Haskell.TH

import qualified Data.Set                  as S
import           Graphics.RecordGL.Util
import           Record.Types

-- | Provide the 'GL.VariableType' of each field in a 'Record'. The list
-- of types has the same order as the fields of the 'Record'.
class HasFieldGLTypes a where
    fieldGLTypes :: a -> [GL.VariableType]

-- | Zips up lists of 'UniformLocation's and a 'Record' setting
-- uniform parameters using the record fields.
class SetUniformFields a where
  setUniformFields :: [Maybe UniformLocation] -> a -> IO ()

type UniformFields a = (HasFields a, HasFieldGLTypes a, SetUniformFields a)

-- | Set GLSL uniform parameters from a 'Record'. A check is
-- performed to verify that /all/ uniforms used by a program are
-- represented by the record type. In other words, the record is a
-- superset of the parameters used by the program.
setAllUniforms :: forall record. UniformFields record
            => ShaderProgram -> record -> IO ()
setAllUniforms s r = case checks of
                      Left msg -> error msg
                      Right _ -> setUniformFields locs r
  where
    fnames = fieldNames r
    checks = do
      namesCheck "record" (M.keys $ uniforms s) fnames
      typesCheck True (snd <$> uniforms s) fieldTypes
    fieldTypes = M.fromList $ zip fnames (fieldGLTypes r)
    locs = map (fmap fst . (`M.lookup` uniforms s)) fnames

-- | Set GLSL uniform parameters form a `Record` representing
-- a subset of all uniform parameters used by a program.
setUniforms :: forall record. UniformFields record => ShaderProgram -> record -> IO ()
setUniforms s r = case checks of
                   Left msg -> error msg
                   Right _ -> setUniformFields locs r
  where
    fnames = fieldNames r
    checks = do
      namesCheck "GLSL programme" fnames (M.keys $ uniforms s)
      typesCheck False fieldTypes (snd <$> uniforms s)
    fieldTypes = M.fromList $ zip fnames (fieldGLTypes r)
    locs = map (fmap fst . (`M.lookup` uniforms s)) fnames

-- | Set GLSL uniform parameters from those fields of a 'PlainRec'
-- whose names correspond to uniform parameters used by a program.
setSomeUniforms :: forall r. UniformFields r
                => ShaderProgram -> r -> IO ()
setSomeUniforms s r = case typesCheck' True (snd <$> uniforms s) fieldTypes of
                       Left msg -> error msg
                       Right _ -> setUniformFields locs r
  where
    fnames = fieldNames r
    fieldTypes = M.fromList . zip fnames $ fieldGLTypes r
    locs = map (fmap fst . (`M.lookup` uniforms s)) fnames

-- | @namesCheck culprit little big@ checks that each name in @little@ is
-- an element of @big@.
namesCheck :: String -> [String] -> [String] -> Either String ()
namesCheck culprit little big = mapM_ go little
  where
    big' = S.fromList big
    go x | x `S.member` big' = Right ()
         | otherwise = Left $ "Field " ++ x ++ " not found in " ++ culprit

-- | @typesChecks blame little big@ checks that each (name,type) pair
-- in @little@ is a member of @big@.
typesCheck :: Bool
           -> M.Map String GL.VariableType -> M.Map String GL.VariableType
           -> Either String ()
typesCheck blame little big = mapM_ go $ M.toList little
  where
    go (n, t) | (glTypeEquiv t <$> M.lookup n big) == Just True = return ()
              | otherwise = Left $ msg n (show t) (maybe "" show (M.lookup n big))
    msg n t t' = let (expected, actual) = if blame then (t, t') else (t', t)
                 in "Record and GLSL types disagree on field " ++ n ++
                    ": GLSL expected " ++ expected ++
                    ", record disappointed with " ++ actual

-- | @typesCheck' blame little big@ checks that each (name,type) pair
-- in the intersection of @little@ and @big@ is consistent.
typesCheck' :: Bool
           -> M.Map String GL.VariableType -> M.Map String GL.VariableType
           -> Either String ()
typesCheck' blame little big = mapM_ go $ M.toList little
  where
    go (n, t) | fromMaybe True (glTypeEquiv t <$> M.lookup n big) = return ()
              | otherwise = Left $ msg n (show t) (maybe "" show (M.lookup n big))
    msg n t t' = let (expected, actual) = if blame then (t, t') else (t', t)
                 in "Record and GLSL types disagree on field " ++ n ++
                    ": GLSL expected " ++ expected ++
                    ", record disappointed with " ++ actual

-- We define our own equivalence relation on types because we don't
-- have unique Haskell representations for every GL type. For example,
-- the GLSL sampler types (e.g. Sampler2D) are just GLint in Haskell.
glTypeEquiv :: GL.VariableType -> GL.VariableType -> Bool
glTypeEquiv x y = glTypeEquiv' x y || glTypeEquiv' y x

-- The equivalence on 'GL.VariableType's we need identifies Samplers
-- with Ints because this is how GLSL represents samplers.
glTypeEquiv' :: GL.VariableType -> GL.VariableType -> Bool
glTypeEquiv' GL.Sampler1D GL.Int' = True
glTypeEquiv' GL.Sampler2D GL.Int' = True
glTypeEquiv' GL.Sampler3D GL.Int' = True
glTypeEquiv' x y = x == y


-- Template Haskell instances. Here be dragons, beware... -----------------------

-- I would rather have written this by hand and some Emacs macros...
-- Instances for the HasFieldGLTypes class
-- Example implementation:
-- fieldGLTypes _ = [variableType (undefined::v1),...,variableType (undefined::vn)]
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
