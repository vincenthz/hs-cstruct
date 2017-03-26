{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module CStruct.Types where

import GHC.TypeLits
import Data.Type.Bool
import Data.Proxy
import Data.Int
import Data.Word
import CStruct.Internal

data Element =
      FInt8
    | FWord8
    | FInt16
    | FWord16
    | FInt32
    | FWord32
    | FInt64
    | FWord64
    | FFloat
    | FDouble
    | FLong
    | FULong
    | FArray Nat Element
    | FStruct [(Symbol, Element)]
    | FUnion [(Symbol, Element)]

data Access = Field Symbol | Index Nat

-----------------------------------------

type family Size (t :: Element) where
    Size ('FInt8)       = 1
    Size ('FWord8)      = 1
    Size ('FInt16)      = 2
    Size ('FWord16)     = 2
    Size ('FInt32)      = 4
    Size ('FWord32)     = 4
    Size ('FInt64)      = 8
    Size ('FWord64)     = 8
    Size ('FFloat)      = 4
    Size ('FDouble)     = 8
    Size ('FLong)       = LongSize
    Size ('FULong)      = LongSize
    Size ('FArray n el) = n * Size el
    Size ('FStruct ls)  = StructSize ls
    Size ('FUnion ls)   = UnionSize ls

type family StructSize (ls :: [(Symbol, Element)]) where
    StructSize '[]            = 0
    StructSize ('(_,l) ': ls) = Size l + StructSize ls

type family UnionSize (ls :: [(Symbol, Element)]) where
    UnionSize '[] = 0
    UnionSize ('(_,l) ': ls) = If (Size l <=? UnionSize ls) (UnionSize ls) (Size l)

getSize :: forall el . KnownNat (Size el) => Integer
getSize = natVal (Proxy :: Proxy (Size el))



----------------------------------------

type family Offset (ofs :: Nat) (accessors :: [Access]) (t :: Element) where
    Offset ofs '[]    t                      = ofs
    Offset ofs ('Field f:fs) ('FStruct dict) = StructOffset ofs f fs dict
    Offset ofs ('Field f:fs) ('FUnion dict)  = UnionOffset ofs f fs dict
    Offset ofs ('Index i:fs) ('FArray n t)   = ArrayOffset ofs i fs n t

type family StructOffset (ofs :: Nat) (field :: Symbol) (rs :: [Access]) (dict :: [(Symbol, Element)]) where
    StructOffset ofs field rs '[]                = TypeError ('Text "offset: field " ':<>: 'ShowType field ':<>: 'Text " not found in structure")
    StructOffset ofs field rs ('(field, t) ': _) = Offset ofs rs t
    StructOffset ofs field rs ('(_    , v) ': r) = StructOffset (ofs + Size v) field rs r

type family UnionOffset (ofs :: Nat) (field :: Symbol) (rs :: [Access]) (dict :: [(Symbol, Element)]) where
    UnionOffset ofs field rs '[]                 = TypeError ('Text "offset: field " ':<>: 'ShowType field ':<>: 'Text " not found in union")
    UnionOffset ofs field rs ('(field, t) ': _)  = Offset ofs rs t
    UnionOffset ofs field rs (_            : r)  = UnionOffset ofs field rs r

type family ArrayOffset ofs (idx :: Nat) (rs :: [Access]) (n :: Nat) (t :: Element) where
    ArrayOffset ofs idx rs n t =
        If (n <=? idx)
            (TypeError ('Text "out of bounds : index is " ':<>: 'ShowType idx ':<>: 'Text " but array of size " ':<>: 'ShowType n))
            (Offset (ofs + (idx * Size t)) rs t)
    
getOffset :: forall el fields . (KnownNat (Offset 0 fields el)) => Integer
getOffset = natVal (Proxy :: Proxy (Offset 0 fields el))

----------------------------------------

type family Resolv (fields :: [Access]) (t :: Element) where
    Resolv '[]    t                      = t
    Resolv ('Field f:fs) ('FStruct dict) = StructResolv f fs dict
    Resolv ('Field f:fs) ('FUnion dict)  = UnionResolv f fs dict
    Resolv ('Index i:fs) ('FArray n t)   = ArrayResolv i fs n t

type family StructResolv (field :: Symbol) (rs :: [Access]) (dict :: [(Symbol, Element)]) where
    StructResolv field rs '[]                = TypeError ('Text "resolv: field " ':<>: 'ShowType field ':<>: 'Text " not found in structure")
    StructResolv field rs ('(field, t) ': _) = Resolv rs t
    StructResolv field rs ('(_    , v) ': r) = StructResolv field rs r

type family UnionResolv (field :: Symbol) (rs :: [Access]) (dict :: [(Symbol, Element)]) where
    UnionResolv field rs '[]                 = TypeError ('Text "resolv: field " ':<>: 'ShowType field ':<>: 'Text " not found in union")
    UnionResolv field rs ('(field, t) ': _)  = Resolv rs t
    UnionResolv field rs (_            : r)  = UnionResolv field rs r

type family ArrayResolv (idx :: Nat) (rs :: [Access]) (n :: Nat) (t :: Element) where
   ArrayResolv idx rs n t =
        If (n <=? idx)
            (TypeError ('Text "out of bounds : index is " ':<>: 'ShowType idx ':<>: 'Text " but array of size " ':<>: 'ShowType n))
            (Resolv rs t)

getResolv :: forall el fields e . (Resolv fields el ~ e) => Proxy (e :: Element) -> Int
getResolv = undefined

----------------------------------------

type family AssocType (t :: Element) where
    AssocType ('FInt8)   = Int8
    AssocType ('FWord8)  = Word8
    AssocType ('FInt16)  = Int16
    AssocType ('FWord16) = Word16
    AssocType ('FInt32)  = Int32
    AssocType ('FWord32) = Word32
    AssocType ('FInt64)  = Int64
    AssocType ('FWord64) = Word64
    AssocType ('FFloat)  = Float
    AssocType ('FDouble) = Double
    --AssocType ('FLong)   = CLong
    --AssocType ('FULong)  = CULong

----------------------------------------
