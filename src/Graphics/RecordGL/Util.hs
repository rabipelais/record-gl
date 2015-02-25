{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- |

module Graphics.RecordGL.Util  where

import           Control.Monad
import           Foreign.Ptr      (castPtr, plusPtr)
import           Foreign.Storable
import           Record.Types

instance Storable v => Storable (Record1 n v) where
  sizeOf _ = sizeOf (undefined :: v)
  alignment _ = alignment (undefined :: v)
  peek ptr = liftM Record1 (peek $ castPtr ptr)
  poke ptr (Record1 v) = poke (castPtr ptr) v

instance (Storable v1, Storable v2) => Storable (Record2 n1 v1 n2 v2) where
  sizeOf _ = sizeOf (undefined :: v1) + sizeOf (undefined :: v2)
  alignment _ = alignment (undefined :: v1)
  peek ptr = do
    !x1 <- peek (castPtr ptr)
    !x2 <- peek (ptr `plusPtr` sizeOf (undefined :: v1))
    return $ Record2 x1 x2
  poke ptr (Record2 v1 v2) = poke (castPtr ptr) v1 >> poke (ptr `plusPtr` sizeOf (undefined :: v1)) v2
