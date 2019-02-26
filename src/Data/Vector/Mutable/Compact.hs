{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- |
-- When you use normal mutable vectors, the garbage collector will traverse
-- them on every garbage collection, because they may point to data that is
-- newer than them. If you want to store your elements on a compact region
-- anyway, but also need to point to them from a mutable vector, you can
-- exploit the stability of the elements' addresses after adding them to the
-- compact region, to avoid these expensive traversals when collecting garbage.
--
-- This library offers a data type for mutable vectors where all elements are
-- stored in a compact region, using an unspeakable pointer casting hack.
module Data.Vector.Mutable.Compact
  ( -- * Basic operations
    CVector
  , replicate
  , read
  , write

    -- * Shape querying
  , length
  , null
  ) where

import Prelude hiding (length, null, read, replicate)

import Data.Primitive.Addr (Addr (..))
import Data.Compact (Compact)
import Data.Vector.Primitive.Mutable (MVector)
import GHC.IO (IO (..))
import GHC.Prim (RealWorld)

import qualified Data.Compact as Compact
import qualified Data.Vector.Primitive.Mutable as MVector
import qualified GHC.Prim as Prim

--------------------------------------------------------------------------------
-- Basic operations

-- |
-- Store the elements of the vector in a compact region, and use an unboxed
-- vector of pointers to the elements. Values in a compact region will not be
-- moved by the garbage collector, so their addresses are stable, and can hence
-- be stored in a mutable byte array. This avoids garbage collection overhead
-- if you store your elements in a compact region anyway, because the garbage
-- collector will not traverse byte arrays.
--
-- The elements must be able to be stored in a compact region. See
-- "Data.Compact" for a list of restrictions. The vector itself cannot itself
-- be stored in a compact region, because it is mutable.
--
-- Elements can never be bottom, because they are added to a compact region.
data CVector a =
  CVector
    !(Compact ())
    {-# UNPACK #-} !(MVector RealWorld Addr)

-- |
-- Create a new mutable vector of the given length, with each element set to
-- the given value. The given compact region will be used for all future
-- elements inserted into this vector.
replicate :: Compact () -> Int -> b -> IO (CVector b)
replicate c n x = do
  -- TODO: Should we add the element with sharing?
  xC <- Compact.getCompact <$> Compact.compactAdd c x
  xA <- IO $ \s -> let !(# s', xA #) = Prim.anyToAddr# xC s in
                   (# s', Addr xA #)
  v  <- MVector.replicate n xA
  pure $ CVector c v

-- |
-- Read the element at the given zero-based position. If the position is out of
-- bounds, crash.
read :: CVector a -> Int -> IO a
read (CVector _ v) i = do
  Addr xA <- MVector.read v i
  let (# xC #) = Prim.addrToAny# xA
  pure xC

-- |
-- Write an element at the given zero-based position. If the position is out of
-- bounds, crash.
--
-- The element will be added to the compact region.
write :: CVector a -> Int -> a -> IO ()
write (CVector c v) i x = do
  -- TODO: Should we add the element with sharing?
  xC <- Compact.getCompact <$> Compact.compactAdd c x
  xA <- IO $ \s -> let !(# s', xA #) = Prim.anyToAddr# xC s in
                   (# s', Addr xA #)
  MVector.write v i xA

--------------------------------------------------------------------------------
-- Shape querying

-- |
-- How many elements are in the vector?
{-# INLINE length #-}
length :: CVector a -> Int
length (CVector _ v) = MVector.length v

-- |
-- Is the vector empty?
{-# INLINE null #-}
null :: CVector a -> Bool
null (CVector _ v) = MVector.null v
