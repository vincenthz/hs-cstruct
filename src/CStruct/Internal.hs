{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
module CStruct.Internal (LongSize) where

import GHC.TypeLits

#include "MachDeps.h"

#if WORD_SIZE_IN_BITS == 64
type LongSize = 8
#else
type LongSize = 4
#endif
