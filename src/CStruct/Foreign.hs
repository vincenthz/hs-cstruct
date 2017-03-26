{-# LANGUAGE MagicHash        #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE FlexibleContexts #-}
module CStruct.Foreign
    ( CPtr(..)
    , glimpse
    , peek
    , poke
    ) where

import           GHC.Prim
import qualified GHC.Ptr              as F (Ptr(..))
import           CStruct.Types
import           Foundation.Primitive
import           Foundation                (Offset(..))

-- | A CStruct pointer
--
-- For all practical purpose this is exactly the same as normal `Ptr`,
-- but the associated phantom type is of Element kind instead of '*'
data CPtr (k :: Element) = CPtr Addr#

-- | Create a CPtr from a normal pointer
toCPtr :: F.Ptr a -> CPtr k
toCPtr (F.Ptr x) = CPtr x

-- | Peek at the pointer without any synchronisation (IO/ST).
--
-- The pointer data should be consider available and immutable
-- at any point; when using this function you need to make sure
-- all sort of lazyness doesn't happens and that you hold
-- the pointer memory at all time.
glimpse :: PrimType (AssocType element)
        => CPtr element                 -- ^ CStruct Pointer to index from
        -> AssocType element            -- ^ Get the value associated
glimpse (CPtr addr) = primAddrIndex addr (Offset 0)

-- | Peek at the CPtr and return the element associated
peek :: PrimType (AssocType element)
     => CPtr element                 -- ^ CStruct Pointer to read from
     -> IO (AssocType element)       -- ^ Get the value associated
peek (CPtr addr) = primAddrRead addr (Offset 0)

-- | Poke the element value at the memory pointed by the CPtr
poke :: PrimType (AssocType element)
     => CPtr element                 -- ^ CStruct pointer to write to
     -> AssocType element            -- ^ the value to write
     -> IO ()
poke (CPtr addr) value = primAddrWrite addr (Offset 0) value
