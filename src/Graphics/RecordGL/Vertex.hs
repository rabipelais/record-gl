{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- |

module Graphics.RecordGL.Vertex (bufferVertices, BufferedVertices(..)) where

import           Foreign.Storable          (Storable)
import           GHC.TypeLits
import           Graphics.GLUtil           hiding (Elem, throwError)
import           Graphics.RecordGL.Util
import           Graphics.Rendering.OpenGL (BufferTarget (..),
                                            VertexArrayDescriptor (..),
                                            bindBuffer, ($=))
import qualified Graphics.Rendering.OpenGL as GL

-- | Representation of a VBO whose type describes the vertices.
data BufferedVertices a  =
  BufferedVertices {getVertexBuffer :: GL.BufferObject}

bufferVertices :: (Storable rs, BufferSource (v rs)) => v rs -> IO (BufferedVertices rs)
bufferVertices = fmap BufferedVertices . fromSource ArrayBuffer
