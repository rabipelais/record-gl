{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Utilities for working with vertex buffer objects (VBOs) filled
-- with vertices represented as `Records`.
module Graphics.RecordGL.Vertex (bufferVertices, bindVertices, reloadVertices
                                , deleteVertices, enableVertices, enableVertices'
                                , enableVertexFields, fieldToVAD
                                , ViableVertex, BufferedVertices(..)) where

import           Graphics.RecordGL.Uniforms
import           Record.Introspection
import           Record.Types

import           BasePrelude
import qualified Data.Map                   as M
import           Data.Proxy
import qualified Data.Vector.Storable       as V
import           Foreign.Ptr                (plusPtr)
import           Foreign.Storable
import           GHC.TypeLits
import           Graphics.GLUtil            hiding (Elem, throwError)
import           Graphics.Rendering.OpenGL  (BufferTarget (..),
                                             VertexArrayDescriptor (..),
                                             bindBuffer, ($=))
import qualified Graphics.Rendering.OpenGL  as GL

-- | Representation of a VBO whose type describes the vertices.
data BufferedVertices a  =
  BufferedVertices {getVertexBuffer :: GL.BufferObject}

-- | Load vertex data into a GPU-accessible buffer.
bufferVertices :: (Storable rs, BufferSource (v rs)) => v rs -> IO (BufferedVertices rs)
bufferVertices = fmap BufferedVertices . fromSource ArrayBuffer

-- | Reload 'BufferedVertices' with a 'V.Vector' of new vertex data.
reloadVertices :: Storable rs
               => BufferedVertices rs
               -> V.Vector rs
               -> IO ()
reloadVertices b v = do
  bindBuffer ArrayBuffer $= Just (getVertexBuffer b)
  replaceVector ArrayBuffer v

-- | Delete the object name associated with 'BufferedVertices'
deleteVertices :: BufferedVertices a -> IO ()
deleteVertices = GL.deleteObjectNames . (: []) . getVertexBuffer

-- | Bind previously-buffered vertex data.
bindVertices :: BufferedVertices a -> IO ()
bindVertices = (bindBuffer ArrayBuffer $=) . Just . getVertexBuffer

-- | Constraint alias capturing the requirements of a vertex type.
type ViableVertex t = (HasFieldNames t, HasFieldSizes t, HasFieldDims t,
                       HasFieldGLTypes t, Storable t)

-- | Line up a shader's attribute inputs with a vertex record. This
-- maps vertex fields to GLSL attributes on the basis of record field names
-- on the Haskell side, and variable names on the GLSL side.
enableVertices :: forall f r. ViableVertex r
               => ShaderProgram -> f r -> IO (Maybe String)
enableVertices s _ = enableAttribs s (Proxy :: Proxy r)

-- | Behaves like 'enableVertices', but raises an exception if the
-- supplied vertex record does not include a field required by the
-- shader.
enableVertices' :: forall f r. ViableVertex r
               => ShaderProgram -> f r -> IO ()
enableVertices' s _ = enableAttribs s (Proxy::Proxy r) >>=
                      maybe (return ()) error

data FieldDescriptor = FieldDescriptor { fieldName   :: String
                                       , fieldOffset :: Int
                                       , fieldDim    :: Int
                                       , fieldType   :: GL.VariableType }
                       deriving Show

fieldDescriptors :: ViableVertex t => t -> [FieldDescriptor]
fieldDescriptors x = getZipList $
                     FieldDescriptor <$> zl (fieldNames x)
                                     <*> zl (scanl (+) 0 $ fieldSizes x)
                                     <*> zl (fieldDims x)
                                     <*> zl (fieldGLTypes x)
  where zl = ZipList

-- | Bind some of a shader's attribute inputs to a vertex record. This
-- is useful when the inputs of a shader are split across multiple
-- arrays.
enableVertexFields :: forall p r. ViableVertex r
                   => ShaderProgram -> p r -> IO ()
enableVertexFields s _ = enableSomeAttribs s p >>= maybe (return ()) error
  where
    p = Proxy::Proxy r

-- | Do not raise an error is some of a shader's inputs are not bound
-- by a vertex record.
enableSomeAttribs :: forall v. ViableVertex v
                  => ShaderProgram -> Proxy v -> IO (Maybe String)
enableSomeAttribs s p = go $ fieldDescriptors (undefined::v)
  where go [] = return Nothing
        go (fd:fds) =
          let n = fieldName fd
              shaderAttribs = attribs s
          in case M.lookup n shaderAttribs of
               Nothing -> return (Just $ "Unexpected attribute " ++ n)
               Just (_,t)
                 | fieldType fd == t -> do enableAttrib s n
                                           setAttrib s n GL.ToFloat $
                                             descriptorVAD p fd
                                           go fds
                 | otherwise -> return . Just $ "Type mismatch in " ++ n

enableAttribs :: forall v. ViableVertex v
              => ShaderProgram -> Proxy v -> IO (Maybe String)
enableAttribs s p = go (map (second snd) $ M.assocs (attribs s))
  where
    go [] = return Nothing
    go ((l, t):as) = case find ((== l) . fieldName) fs of
                       Nothing -> return (Just $ "GLSL expecting " ++ l)
                       Just fd
                         | fieldType fd == t -> do
                           enableAttrib s l
                           setAttrib s l GL.ToFloat $
                             descriptorVAD p fd
                           go as
                         | otherwise -> return . Just $ "Type mismatch in " ++ l
    fs = fieldDescriptors (undefined::v)

descriptorVAD :: forall t a. Storable t
              => Proxy t -> FieldDescriptor -> VertexArrayDescriptor a
descriptorVAD _ fd = VertexArrayDescriptor (fromIntegral $ fieldDim fd)
                                           (variableDataType $ fieldType fd)
                                           (fromIntegral $
                                            sizeOf (undefined::t))
                                           (offset0 `plusPtr` fieldOffset fd)

namesAndOffsets :: (HasFieldNames t, HasFieldSizes t) => t -> [(String, Int)]
namesAndOffsets x = zip (fieldNames x) (scanl (+) 0 (fieldSizes x))

-- | Produce a 'GL.VertexArrayDescriptor' for a particular field of a
-- vertex record.
-- fieldToVAD :: forall r v a sy proxy.
--               (
--                Foldable v)
--            => String -> proxy r -> GL.VertexArrayDescriptor a
fieldToVAD :: forall sy r v a proxy.
              (Field' sy r (v a), HasFieldNames r, HasFieldSizes r, HasGLType a, Storable r, Num (v a),
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
               KnownSymbol sy,
#else
               SingI sy,
#endif
               Foldable v)
           => FieldName sy -> r -> GL.VertexArrayDescriptor a
fieldToVAD _ _ = GL.VertexArrayDescriptor dim
                                          (glType (undefined::a))
                                          (fromIntegral sz)
                                          (offset0 `plusPtr` offset)
  where
    sz = sizeOf (undefined::r)
    dim = getSum $ foldMap (const (Sum 1)) (0::v a)
    Just offset = lookup n $ namesAndOffsets (undefined::r)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 707
    n = symbolVal (Proxy::Proxy sy)
#else
    n = fromSing (sing::Sing sy)
#endif
