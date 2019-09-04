{-# language
        BangPatterns
      , FlexibleContexts
      , FlexibleInstances
      , DefaultSignatures
      , LambdaCase
      , MagicHash
      , RankNTypes
      , ScopedTypeVariables
      , DataKinds
      , GADTs
      , TypeOperators
      , TypeFamilies
      , TypeFamilyDependencies
      , UnboxedTuples
      , UndecidableInstances
  #-}

-- | The contiguous typeclass parameterises over a contiguous array type.
--   This allows us to have a common API to a number of contiguous
--   array types and their mutable counterparts.
module Data.Primitive.Contiguous.Sized
  where
  -- ( -- * Accessors
  --   -- ** Length Information
  --   size
  -- , sizeMutable
  -- , null
  --   -- ** Indexing
  -- , index
  -- , index#
  -- , read
  --   -- ** Monadic indexing
  -- , indexM

  --   -- * Construction
  --   -- ** Initialisation
  -- , empty
  -- , new
  -- , singleton
  -- , doubleton
  -- , tripleton
  -- , replicate
  -- , replicateMutable
  -- , generate
  -- , generateM
  -- , generateMutable
  -- , iterateN
  -- , iterateMutableN
  -- , write
  --   -- ** Monadic initialisation
  -- , replicateMutableM
  -- , generateMutableM
  -- , iterateMutableNM
  -- , create
  -- , createT
  --   -- ** Unfolding
  -- , unfoldr
  -- , unfoldrN
  -- , unfoldrMutable
  --   -- ** Enumeration
  -- , enumFromN
  -- , enumFromMutableN
  --   -- ** Concatenation
  -- , append
  --   -- * Modifying arrays
  --   -- ** Permutations
  -- , reverse
  -- , reverseMutable
  -- , reverseSlice

  --   -- ** Resizing
  -- , resize

  --   -- * Elementwise operations
  --   -- ** Mapping
  -- , map
  -- , map'
  -- , mapMutable
  -- , mapMutable'
  -- , imap
  -- , imap'
  -- , imapMutable
  -- , imapMutable'
  -- , modify
  -- , modify'
  -- , mapMaybe

  --   -- ** Zipping
  -- , zip
  -- , zipWith

  --   -- ** Specific elements
  -- , swap

  --   -- * Working with predicates
  --   -- ** Filtering
  -- , filter
  -- , ifilter
  -- , catMaybes
  -- , lefts
  -- , rights
  -- , partitionEithers
  --   -- ** Searching
  -- , find
  -- , elem
  -- , maximum
  -- , minimum
  -- , maximumBy
  -- , minimumBy
  --   -- ** Comparing for equality
  -- , equals
  -- , equalsMutable
  -- , same

  --   -- * Folds
  -- , foldl
  -- , foldl'
  -- , foldr
  -- , foldr'
  -- , foldMap
  -- , foldMap'
  -- , foldlMap'
  -- , ifoldl'
  -- , ifoldr'
  -- , ifoldlMap'
  -- , ifoldlMap1'
  -- , foldlM'
  -- , asum

  --   -- * Traversals
  -- , traverse
  -- , traverse_
  -- , itraverse
  -- , itraverse_
  -- , traverseP
  -- , mapM
  -- , forM
  -- , mapM_
  -- , forM_
  -- , for
  -- , for_
  -- , sequence
  -- , sequence_

  --   -- * Typeclass method defaults
  -- , (<$)
  -- , ap

  --   -- * Prefix sums (scans)
  -- , scanl
  -- , scanl'
  -- , iscanl
  -- , iscanl'
  -- , prescanl
  -- , prescanl'
  -- , iprescanl
  -- , iprescanl'
  -- --, postscanl
  -- --, ipostscanl

  --   -- * Conversions
  --   -- ** Lists
  -- , fromList
  -- , fromListN
  -- , fromListMutable
  -- , fromListMutableN
  -- , unsafeFromListN
  -- , unsafeFromListReverseN
  -- , unsafeFromListReverseMutableN
  -- , toList
  -- , toListMutable
  --   -- ** Other array types
  -- , convert
  -- , lift
  -- , unlift
  --   -- ** Between mutable and immutable variants
  -- , clone
  -- , cloneMutable
  -- , copy
  -- , copyMutable
  -- , freeze
  -- , thaw
  -- , unsafeFreeze

  --   -- * Hashing
  -- , liftHashWithSalt

  --   -- * Forcing an array and its contents
  -- , rnf

  --   -- * Classes
  -- , Contiguous(Mutable,Element)
  -- , Always

  --   -- * Re-Exports
  -- , Array
  -- , MutableArray
  -- , SmallArray
  -- , SmallMutableArray
  -- , PrimArray
  -- , MutablePrimArray
  -- , UnliftedArray
  -- , MutableUnliftedArray
  -- ) where

-- import Prelude hiding (map,foldr,foldMap,traverse,read,filter,replicate,null,reverse,foldl,foldr,zip,zipWith,scanl,(<$),elem,maximum,minimum,mapM,mapM_,sequence,sequence_)
-- import Control.Applicative (liftA2)
-- import Control.Monad (when)
-- import Control.Monad.ST (runST,ST)
-- import Data.Bits (xor)
-- import Data.Coerce (coerce)
-- import Data.Primitive hiding (fromList,fromListN)
-- import Data.Primitive.Unlifted.Array
-- import Data.Primitive.Unlifted.Class (PrimUnlifted)
-- import Data.Semigroup (Semigroup,(<>),First(..))
-- import Data.Word (Word8)
-- import GHC.Base (build)
-- import GHC.Exts (MutableArrayArray#,ArrayArray#,Constraint,sizeofByteArray#,sizeofArray#,sizeofArrayArray#,unsafeCoerce#,sameMutableArrayArray#,isTrue#,dataToTag#,Int(..))
-- import qualified Control.DeepSeq as DS
-- import qualified Control.Applicative as A
-- import qualified Prelude

import Prelude hiding (map,foldr,foldMap,traverse,read,filter,replicate,null,reverse,foldl,foldr,zip,zipWith,scanl,(<$),elem,maximum,minimum,mapM,mapM_,sequence,sequence_)
import Data.Kind (Type, Constraint)
import qualified Data.Foldable as Foldable
import Control.DeepSeq (NFData)
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad(..))

import GHC.TypeLits (type (+))
import qualified GHC.TypeLits as GHC
import Arithmetic.Unsafe (Nat(..))
import Arithmetic.Types (type (<=), Fin(..))
import qualified Arithmetic.Nat as Nat
import qualified Arithmetic.Lte as Lte
import qualified Arithmetic.Fin as Fin
import qualified Data.Primitive.Contiguous as Unsized

-- | The 'Contiguous' typeclass as an interface to a multitude of
--   contiguous structures.
class Unsized.Contiguous (Unsized arr) => Contiguous (arr :: GHC.Nat -> Type -> Type) where
  -- | The Mutable counterpart to the array.
  type family Mutable arr = (r :: Type -> GHC.Nat -> Type -> Type) | r -> arr
  -- | The Unsized counterpart to the array
  type family Unsized arr = (r :: Type -> Type) | r -> arr
  -- | The constraint needed to store elements in the array.
  -- This defaults to the unsized 'Element' type family.
  type family Element arr :: Type -> Constraint
  type Element arr = Unsized.Element (Unsized arr)
  -- | Downcast a sized array to an unsized array
  unsized :: arr size a -> Unsized arr a
  -- | Upcast an unsized array to a sized array assuming you can handle any possible size.
  sized :: Unsized arr a -> (forall size. arr size a -> x) -> x
  -- | The empty array.
  empty :: arr 0 a
  -- | Allocate a new mutable array of the given size.
  new :: (PrimMonad m, Element arr b) => Nat size -> m (Mutable arr (PrimState m) size b)
  -- | @'replicateMutable' n x@ is a mutable array of length @n@ with @x@ the value of every element.
  replicateMutable :: (PrimMonad m, Element arr b) => Nat size -> b -> m (Mutable arr (PrimState m) size b)
  -- | Index into an array at the given index.
  index :: Element arr b => arr size b -> Fin size -> b
  -- | Index into an array at the given index, yielding an unboxed one-tuple of the element.
  index# :: Element arr b => arr size b -> Fin size -> (# b #)
  -- | Indexing in a monad.
  --
  --   The monad allows operations to be strict in the array
  --   when necessary. Suppose array copying is implemented like this:
  --
  --   > copy mv v = ... write mv i (v ! i) ...
  --
  --   For lazy arrays, @v ! i@ would not be not be evaluated,
  --   which means that @mv@ would unnecessarily retain a reference
  --   to @v@ in each element written.
  --
  --   With 'indexM', copying can be implemented like this instead:
  --
  --   > copy mv v = ... do
  --   >   x <- indexM v i
  --   >   write mv i x
  --
  --   Here, no references to @v@ are retained because indexing
  --   (but /not/ the elements) is evaluated eagerly.
  indexM :: (Element arr b, Monad m) => arr size b -> Fin size -> m b
  -- | Read a mutable array at the given index.
  read :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) size b -> Fin size -> m b
  -- | Write to a mutable array at the given index.
  write :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) size b -> Fin size -> b -> m ()
  -- | Resize an array into one with the given size.
  resize
    :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) size1 b
    -> Nat size2
    -> size1 <= size2
    -> m (Mutable arr (PrimState m) size2 b)
  -- | The size of the array
  size :: Element arr b => arr size b -> Nat size
  -- | The size of the mutable array.
  sizeMutable :: (PrimMonad m, Element arr b) => Mutable arr (PrimState m) size b -> m (Nat size)
  -- | Turn a mutable array into an immutable one without copying.
  --   The mutable array should not be used after this conversion.
  unsafeFreeze :: PrimMonad m => Mutable arr (PrimState m) size b -> m (arr size b)
  -- | Turn a mutable array into an immutable one with copying, using a slice of the mutable array.
  freeze
    :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) size b
    -> Nat off
    -> Nat len
    -> off + len <= size
    -> m (arr len b)
  -- | Copy a slice of an immutable array into a new mutable array.
  thaw
    :: (PrimMonad m, Element arr b)
    => arr size b
    -> Nat off
    -> Nat len
    -> off + len <= size
    -> m (Mutable arr (PrimState m) len b)
  -- | Copy a slice of an array into a mutable array.
  copy :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) size1 b -- ^ destination array
    -> Nat off1 -- ^ offset into destination array
    -> arr size2 b -- ^ source array
    -> Nat off2 -- ^ offset into source array
    -> Nat len -- ^ number of elements to copy
    -> off1 + len <= size1
    -> off2 + len <= size2
    -> m ()
  -- | Copy a slice of a mutable array into another mutable array.
  --   In the case that the destination and source arrays are the
  --   same, the regions may overlap.
  copyMutable :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) size1 b -- ^ destination array
    -> Nat off1 -- ^ offset into destination array
    -> Mutable arr (PrimState m) size2 b -- ^ source array
    -> Nat off2 -- ^ offset into source array
    -> Nat len -- ^ number of elements to copy
    -> off1 + len <= size1
    -> off2 + len <= size2
    -> m ()
  -- | Clone a slice of an array.
  clone :: Element arr b
    => arr size b
    -> Nat off
    -> Nat len
    -> off + len <= size
    -> arr len b
  -- | Clone a slice of a mutable array.
  cloneMutable :: (PrimMonad m, Element arr b)
    => Mutable arr (PrimState m) size b
    -> Nat off
    -> Nat len
   -> off + len <= size
    -> m (Mutable arr (PrimState m) len b)
  -- | Test the two arrays for equality.
  equals :: (Element arr b, Eq b) => arr size1 b -> arr size2 b -> Bool
  -- | Test the two mutable arrays for pointer equality.
  --   Does not check equality of elements.
  equalsMutable :: Mutable arr s size1 a -> Mutable arr s size2 a -> Bool
  -- | Create a singleton array.
  singleton :: Element arr a => a -> arr 1 a
  -- | Create a doubleton array.
  doubleton :: Element arr a => a -> a -> arr 2 a
  -- | Create a tripleton array.
  tripleton :: Element arr a => a -> a -> a -> arr 3 a
  -- | Reduce the array and all of its elements to WHNF.
  rnf :: (NFData a, Element arr a) => arr size a -> ()

newtype Array :: GHC.Nat -> Type -> Type where
  UnsafeArray :: Unsized.Array a -> Array n a

newtype MutableArray :: Type -> GHC.Nat -> Type -> Type where
  UnsafeMutableArray :: Unsized.MutableArray s a -> MutableArray s size a

instance Contiguous Array where
  type Mutable Array = MutableArray
  type Unsized Array = Unsized.Array
  type Element Array = Unsized.Element Unsized.Array
  unsized (UnsafeArray arr) = arr
  sized arr cb = cb (UnsafeArray arr)
  empty = UnsafeArray Unsized.empty
  new n = UnsafeMutableArray <$> Unsized.new (Nat.demote n)
  replicateMutable n x = UnsafeMutableArray <$> Unsized.replicateMutable (Nat.demote n) x
  index (UnsafeArray arr) (Fin i _) = Unsized.index arr (Nat.demote i)
  index# (UnsafeArray arr) (Fin i _) = Unsized.index# arr (Nat.demote i)
  indexM (UnsafeArray arr) (Fin i _) = Unsized.indexM arr (Nat.demote i)
  read (UnsafeMutableArray marr) (Fin i _) = Unsized.read marr (Nat.demote i)
  write (UnsafeMutableArray marr) (Fin i _) x = Unsized.write marr (Nat.demote i) x
  resize (UnsafeMutableArray marr) newSize !_ = UnsafeMutableArray <$> Unsized.resize marr (Nat.demote newSize)
  size (UnsafeArray arr) = Nat (Unsized.size arr)
  sizeMutable (UnsafeMutableArray marr) = Nat <$> Unsized.sizeMutable marr
  unsafeFreeze (UnsafeMutableArray marr) = UnsafeArray <$> Unsized.unsafeFreeze marr
  freeze (UnsafeMutableArray marr) off len !_ = UnsafeArray <$> Unsized.freeze marr (Nat.demote off) (Nat.demote len)
  thaw  (UnsafeArray arr) off len !_ = UnsafeMutableArray <$> Unsized.thaw arr (Nat.demote off) (Nat.demote len)
  copy (UnsafeMutableArray dst) off1 (UnsafeArray src) off2 len !_ !_ =
    Unsized.copy dst (Nat.demote off1) src (Nat.demote off2) (Nat.demote len)
  copyMutable (UnsafeMutableArray dst) off1 (UnsafeMutableArray src) off2 len !_ !_ =
    Unsized.copyMutable dst (Nat.demote off1) src (Nat.demote off2) (Nat.demote len)
  clone (UnsafeArray arr) off len !_ = UnsafeArray (Unsized.clone arr (Nat.demote off) (Nat.demote len))
  cloneMutable (UnsafeMutableArray marr) off len !_
    = UnsafeMutableArray <$> Unsized.cloneMutable marr (Nat.demote off) (Nat.demote len)
  equals (UnsafeArray arr1) (UnsafeArray arr2) = Unsized.equals arr1 arr2
  equalsMutable (UnsafeMutableArray marr1) (UnsafeMutableArray marr2) = Unsized.equalsMutable marr1 marr2
  singleton x = UnsafeArray (Unsized.singleton x)
  doubleton x y = UnsafeArray (Unsized.doubleton x y)
  tripleton x y z = UnsafeArray (Unsized.tripleton x y z)
  rnf (UnsafeArray arr) = Unsized.rnf arr
  {-# inline empty #-}
  {-# inline new #-}
  {-# inline replicateMutable #-}
  {-# inline index #-}
  {-# inline index# #-}
  {-# inline indexM #-}
  {-# inline read #-}
  {-# inline write #-}
  {-# inline resize #-}
  {-# inline size #-}
  {-# inline sizeMutable #-}
  {-# inline unsafeFreeze #-}
  {-# inline freeze #-}
  {-# inline thaw #-}
  {-# inline copy #-}
  {-# inline copyMutable #-}
  {-# inline clone #-}
  {-# inline cloneMutable #-}
  {-# inline equals #-}
  {-# inline equalsMutable #-}
  {-# inline singleton #-}
  {-# inline doubleton #-}
  {-# inline tripleton #-}
  {-# inline rnf #-}

-- | Append two arrays.
append :: (Contiguous arr, Element arr a) => arr size1 a -> arr size2 a -> arr (size1 + size2) a
append !a !b = runST $ do
  let sizeA = size a
      sizeB = size b
  m <- new (sizeA `Nat.plus` sizeB)
  copy m Nat.zero a Nat.zero sizeA (Lte.weakenR Lte.reflexive) Lte.reflexive
  copy m sizeA b Nat.zero sizeB Lte.reflexive Lte.reflexive
  unsafeFreeze m
{-# inline append #-}

-- | Map over the elements of an array with the index.
imap
  :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c)
  => (Fin size -> b -> c)
  -> arr1 size b -> arr2 size c
imap f a = runST $ do
  let sizeA = size a
  mb <- new sizeA
  Foldable.forM_ (Fin.ascending sizeA) $ \i -> do
    x <- indexM a i
    write mb i (f i x)
  unsafeFreeze mb
{-# inline imap #-}

-- | Map strictly over the elements of an array with the index.
--
--   Note that because a new array must be created, the resulting
--   array type can be /different/ than the original.
imap'
  :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c)
  => (Fin size -> b -> c)
  -> arr1 size b
  -> arr2 size c
imap' f a = runST $ do
  let sizeA = size a
  mb <- new sizeA
  Foldable.forM_ (Fin.ascending sizeA) $ \i -> do
    x <- indexM a i
    let !b = f i x
    write mb i b
  unsafeFreeze mb
{-# inline imap' #-}

-- | Map over the elements of an array.
--
--   Note that because a new array must be created, the resulting
--   array type can be /different/ than the original.
map
  :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c)
  => (b -> c)
  -> arr1 size b
  -> arr2 size c
map f a = runST $ do
  let sizeA = size a
  mb <- new sizeA
  Foldable.forM_ (Fin.ascending sizeA) $ \i -> do
    x <- indexM a i
    write mb i (f x)
  unsafeFreeze mb
{-# inline map #-}

-- | Map strictly over the elements of an array.
--
--   Note that because a new array must be created, the resulting
--   array type can be /different/ than the original.
map' :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 c) => (b -> c) -> arr1 size b -> arr2 size c
map' f a = runST $ do
  let sizeA = size a
  mb <- new sizeA
  Foldable.forM_ (Fin.ascending sizeA) $ \i -> do
    x <- indexM a i
    let !b = f x
    write mb i b
  unsafeFreeze mb
{-# inline map' #-}

-- | Convert one type of array into another.
convert :: (Contiguous arr1, Element arr1 b, Contiguous arr2, Element arr2 b) => arr1 size b -> arr2 size b
convert a = map id a
{-# inline convert #-}

foldr :: (Contiguous arr, Element arr a) => (a -> b -> b) -> b -> arr size a -> b
foldr = foldr -- TODO
{-# inline foldr #-}

-- | @'replicateMutableM' n act@ performs the action n times, gathering the results.
replicateMutableM :: (PrimMonad m, Contiguous arr, Element arr a)
  => Nat size
  -> m a
  -> m (Mutable arr (PrimState m) size a)
replicateMutableM len act = do
  marr <- new len
  Foldable.forM_ (Fin.ascending len) $ \i -> do
    x <- act
    write marr i x
  pure marr
