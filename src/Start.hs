{-# LANGUAGE NoImplicitPrelude #-}
-- | Provide reexports from dependency packages which will be available to all
-- modules in the jump package, and will be reexported by the @Jump@ module for
-- end users to use.
module Start
    ( -- * Standard
      -- ** Operators
      (Prelude.$)
    , (Prelude.$!)
    , (Prelude.&&)
    , (Prelude.||)
    , (Prelude..)
      -- ** Functions
    , Prelude.not
    , Prelude.otherwise
    , Prelude.fst
    , Prelude.snd
    , Prelude.id
    , Prelude.maybe
    , Prelude.either
    , Prelude.flip
    , Prelude.const
    , Prelude.error
    , putStr
    , putStrLn
    , print
    , terror
    , Prelude.odd
    , Prelude.even
    , Prelude.uncurry
    , Prelude.curry
    , Data.Tuple.swap
    , Prelude.until
    , Prelude.asTypeOf
    , Prelude.undefined
    , Prelude.seq
      -- ** Type classes
    , Prelude.Ord (..)
    , Prelude.Eq (..)
    , Prelude.Bounded (..)
    , Prelude.Enum (..)
    , Prelude.Show (..)
    , Prelude.Read
    , Prelude.Functor (..)
    , map
    , Prelude.Monad (..)
    , Data.String.IsString (..)
      -- ** Numeric type classes
    , Prelude.Num (..)
    , Prelude.Real (..)
    , Prelude.Integral (..)
    , Prelude.Fractional (..)
    , Prelude.Floating (..)
    , Prelude.RealFrac (..)
    , Prelude.RealFloat(..)
      -- ** Data types
    , Prelude.Maybe (..)
    , Prelude.Ordering (..)
    , Prelude.Bool (..)
    , Prelude.Char
    , Prelude.IO
    , Prelude.Either (..)
      -- * Re-exports
      -- ** Packed reps
    , Data.ByteString.ByteString
    , LByteString
    , Data.Text.Text
    , LText
      -- ** Containers
    , Data.Map.Strict.Map
    , Data.HashMap.Strict.HashMap
    , Data.IntMap.Strict.IntMap
    , Data.Set.Set
    , Data.HashSet.HashSet
    , Data.IntSet.IntSet
    , Data.Sequence.Seq
    , Data.Vector.Vector
    , UVector
    , Data.Vector.Unboxed.Unbox
    , SVector
    , Data.Vector.Storable.Storable
      -- ** Numbers
    , Data.Word.Word
    , Data.Word.Word8
    , Data.Word.Word16
    , Data.Word.Word32
    , Data.Word.Word64
    , Data.Int.Int
    , Data.Int.Int8
    , Data.Int.Int16
    , Data.Int.Int32
    , Data.Int.Int64
    , Prelude.Integer
    , Prelude.Rational
    , Prelude.Float
    , Prelude.Double
      -- ** Numeric functions
    , (Prelude.^)
    , (Prelude.^^)
    , Prelude.subtract
    , Prelude.fromIntegral
    , Prelude.realToFrac
      -- ** Monoids
    , Data.Monoid.Monoid (..)
    , (Data.Monoid.<>)
      -- ** Foldable
    , Data.Foldable.Foldable
        ( fold
        , foldMap
        , foldr
        , foldr'
        , foldl
        , foldl'
        , toList
        , null
        , length
        , elem
        )
    , sum
    , product
    , Data.Foldable.foldrM
    , Data.Foldable.foldlM
    , Data.Foldable.traverse_
    , Data.Foldable.for_
    , Data.Foldable.sequenceA_
    , Data.Foldable.asum
    , mapM_
    , forM_
    , sequence_
    , msum
    , concat
    , concatMap
      -- ** Traversable
    , Data.Traversable.Traversable
        ( traverse
        , sequenceA
        )
    , mapM
    , sequence

    -- MSS: Personally, I think this is really bad naming, and we should have
    -- `for = flip map`
    , Data.Traversable.for
    , forM
    , Data.Traversable.mapAccumL
    , Data.Traversable.mapAccumR
    , Data.Traversable.fmapDefault
    , Data.Traversable.foldMapDefault
      -- ** arrow
    , Control.Arrow.first
    , Control.Arrow.second
    , (Control.Arrow.***)
    , (Control.Arrow.&&&)
      -- ** Maybe
    , Data.Maybe.mapMaybe
    , Data.Maybe.catMaybes
    , Data.Maybe.fromMaybe
    , Data.Maybe.isJust
    , Data.Maybe.isNothing
    , Data.Maybe.listToMaybe
    , Data.Maybe.maybeToList
      -- ** Either
    , Data.Either.partitionEithers
    , Data.Either.lefts
    , Data.Either.rights
      -- ** Ord
    , Data.Function.on
    , Data.Ord.comparing
    , equating
    , GHC.Exts.Down (..)
      -- ** Applicative
    , Control.Applicative.Applicative (..)
    , Control.Applicative.Alternative (..)
    , Control.Applicative.Const (..)
    , Control.Applicative.ZipList (..)
    , (Control.Applicative.<$>)
    , Control.Applicative.optional
    , Control.Applicative.liftA
    , Control.Applicative.liftA2
    , Control.Applicative.liftA3
      -- ** Monad
    , (Control.Monad.<=<)
    , (Control.Monad.>=>)
    , (Control.Monad.=<<)
    , Control.Monad.forever
    , Control.Monad.void
    , Control.Monad.join
    , Control.Monad.replicateM_
    , Control.Monad.guard
    , Control.Monad.when
    , Control.Monad.unless
    , (Control.Monad.<$!>)
      -- ** Transformers
    , Data.Functor.Identity.Identity (..)
    , Control.Monad.Reader.MonadTrans (..)
    , Control.Monad.Reader.MonadIO (..)
    , Control.Monad.Reader.MonadReader (..)
    , Control.Monad.Reader.ReaderT (..)
    , Control.Monad.Writer.MonadWriter (..)
    , Control.Monad.State.Strict.MonadState (..)
    , Control.Monad.State.Strict.StateT (..)
    , Control.Monad.State.Strict.execStateT
    , Control.Monad.State.Strict.evalStateT
      -- ** Exceptions
    , Control.Exception.Exception (..)
    , Data.Typeable.Typeable
    , Control.Exception.SomeException
    , Control.Exception.IOException
    , Control.Monad.Catch.MonadThrow (..)
    , Control.Monad.Catch.MonadCatch (..)
    , Control.Monad.Catch.MonadMask (..)
    {-
    , Control.Exception.Lifted.throwIO
    , Control.Exception.Lifted.try
    , Control.Exception.Lifted.tryJust
    , Control.Exception.Lifted.catch
    , Control.Exception.Lifted.catchJust
    , Control.Exception.Lifted.handle
    , Control.Exception.Lifted.handleJust
    , Control.Exception.Lifted.bracket
    , Control.Exception.Lifted.bracket_
    , Control.Exception.Lifted.bracketOnError
    , Control.Exception.Lifted.onException
    , Control.Exception.Lifted.finally
    , Control.Exception.Lifted.mask
    , Control.Exception.Lifted.mask_
    , Control.Exception.Lifted.uninterruptibleMask
    , Control.Exception.Lifted.uninterruptibleMask_
    -}
    , module System.IO.Error
      -- ** Files
    , System.FilePath.FilePath
    , (System.FilePath.</>)
    , (System.FilePath.<.>)
    , System.FilePath.splitExtension
    , System.FilePath.takeExtension
    , System.FilePath.replaceExtension
    , System.FilePath.dropExtension
    , System.FilePath.addExtension
    , System.FilePath.splitExtensions
    , System.FilePath.dropExtensions
    , System.FilePath.takeExtensions
    , System.FilePath.splitFileName
    , System.FilePath.takeFileName
    , System.FilePath.replaceFileName
    , System.FilePath.dropFileName
    , System.FilePath.takeBaseName
    , System.FilePath.replaceBaseName
    , System.FilePath.takeDirectory
    , System.FilePath.replaceDirectory
    , System.FilePath.splitPath
    , System.FilePath.joinPath
    , System.FilePath.splitDirectories
    , System.FilePath.hasTrailingPathSeparator
    , System.FilePath.addTrailingPathSeparator
    , System.FilePath.dropTrailingPathSeparator
      -- ** IO
    , System.IO.Handle
    , System.IO.withBinaryFile
    , System.IO.IOMode (..)
    , System.IO.hClose
    , System.IO.stdin
    , System.IO.stdout
    , System.IO.stderr
    , Data.ByteString.hPut
    , Data.ByteString.hGetSome
      -- ** Text
    , Data.Text.lines
    , Data.Text.words
    , Data.Text.unlines
    , Data.Text.unwords
    , Data.Text.Encoding.encodeUtf8
    , decodeUtf8
    , Data.Text.Encoding.decodeUtf8With
    , Data.Text.Encoding.Error.lenientDecode
      -- ** Strings
    , Prelude.String
      -- ** Hashing
    , Data.Hashable.Hashable (..)
    ) where

import qualified System.FilePath
import System.IO.Error
import qualified Control.Monad.Catch
import qualified Control.Exception
import qualified Control.Monad.Reader
import qualified Control.Monad.State.Strict
import qualified Control.Monad.Writer (MonadWriter (..)) -- don't want to grab the badly implemented WriterT
import qualified Control.Monad
import qualified Control.Applicative
import qualified Data.Typeable
import qualified Data.Foldable
import qualified Data.Traversable
import qualified Prelude
import qualified Data.Hashable
import qualified GHC.Exts
import qualified Data.Text
import qualified Data.Text.IO
import qualified Data.Function
import qualified Data.Ord
import qualified Data.Either
import qualified Data.Maybe
import qualified Control.Arrow
import qualified Data.Monoid
import qualified Data.Int
import qualified Data.Word
import qualified Data.Vector
import qualified Data.Vector.Storable
import qualified Data.Vector.Unboxed
import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.IntSet
import qualified Data.Set
import qualified Data.HashSet
import qualified Data.IntMap.Strict
import qualified Data.HashMap.Strict
import qualified Data.Map.Strict
import qualified Data.Tuple
import qualified Data.Sequence
import qualified Data.String
import qualified Data.ByteString
import qualified System.IO
import qualified Data.Functor.Identity

type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString
type UVector = Data.Vector.Unboxed.Vector
type SVector = Data.Vector.Storable.Vector

equating :: Prelude.Eq a => (b -> a) -> b -> b -> Prelude.Bool
equating = Data.Function.on (Prelude.==)

putStr :: Control.Monad.Reader.MonadIO m => Data.Text.Text -> m ()
putStr = Control.Monad.Reader.liftIO Prelude.. Data.Text.IO.putStr

putStrLn :: Control.Monad.Reader.MonadIO m => Data.Text.Text -> m ()
putStrLn = Control.Monad.Reader.liftIO Prelude.. Data.Text.IO.putStrLn

print :: (Control.Monad.Reader.MonadIO m, Prelude.Show a) => a -> m ()
print = Control.Monad.Reader.liftIO Prelude.. Prelude.print

terror :: Data.Text.Text -> a
terror = Prelude.error Prelude.. Data.Text.unpack

mapM :: (Data.Traversable.Traversable t, Control.Applicative.Applicative f)
     => (a -> f b) -> t a -> f (t b)
mapM = Data.Traversable.traverse

sequence :: (Data.Traversable.Traversable t, Control.Applicative.Applicative f)
         => t (f a) -> f (t a)
sequence = Data.Traversable.sequenceA

forM :: (Data.Traversable.Traversable t, Control.Applicative.Applicative f)
     => t a -> (a -> f b) -> f (t b)
forM = Data.Traversable.for

mapM_ :: (Data.Foldable.Foldable t, Control.Applicative.Applicative m) => (a -> m b) -> t a -> m ()
mapM_ = Data.Foldable.traverse_

forM_ :: (Data.Foldable.Foldable t, Control.Applicative.Applicative m) => t a -> (a -> m b) -> m ()
forM_ = Data.Foldable.for_

sequence_ :: (Data.Foldable.Foldable t, Control.Applicative.Applicative m) => t (m a) -> m ()
sequence_ = Data.Foldable.sequenceA_

msum :: (Data.Foldable.Foldable t, Control.Applicative.Alternative m) => t (m a) -> m a
msum = Data.Foldable.asum

concat :: (Data.Foldable.Foldable t, Data.Monoid.Monoid m) => t m -> m
concat = Data.Foldable.fold

concatMap :: (Data.Foldable.Foldable t, Data.Monoid.Monoid m) => (a -> m) -> t a -> m
concatMap = Data.Foldable.foldMap

map :: Prelude.Functor f => (a -> b) -> f a -> f b
map = Prelude.fmap

sum :: (Prelude.Num a, Data.Foldable.Foldable f) => f a -> a
sum = Data.Foldable.foldl' (Prelude.+) 0

product :: (Prelude.Num a, Data.Foldable.Foldable f) => f a -> a
product = Data.Foldable.foldl' (Prelude.*) 1

decodeUtf8 :: Data.ByteString.ByteString -> Data.Text.Text
decodeUtf8 = Data.Text.Encoding.decodeUtf8With
             Data.Text.Encoding.Error.lenientDecode
