{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE UnboxedTuples  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds      #-}
module CStruct.UArray
    (
    ) where

import           Foundation.Primitive
import           Foundation.Array
import           Foundation.Array.Internal
import           CStruct.Types

newtype CUArray (k :: Element) a = CUArray (UArray a)

newtype CMUArray st (k :: Element) a = CMUArray (MUArray st a)

cuarrayIndex :: PrimType a
             => CUArray k a
             -> a
cuarrayIndex = undefined

cuarrayRead :: (PrimType a, PrimMonad prim)
            => CMUArray (PrimState prim) k a
            -> prim a
cuarrayRead = undefined

cuarrayWrite :: (PrimType a, PrimMonad prim)
             => CMUArray (PrimState prim) k a
             -> prim a
cuarrayWrite = undefined
