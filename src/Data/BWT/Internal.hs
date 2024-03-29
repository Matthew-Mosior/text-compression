{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}


-- |
-- Module      :  Data.BWT.Internal
-- Copyright   :  (c) Matthew Mosior 2022
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this library are expected to track development
-- closely.
--
-- All credit goes to the author(s)/maintainer(s) of the
-- [containers](https://hackage.haskell.org/package/containers) library
-- for the above warning text.
--
-- = Description
--
-- Various data structures and custom data types to describe the Burrows-Wheeler Transform (BWT)
-- and the Inverse BWT.
--
-- The implementation of the BWT relies upon 'DS.Seq' provided
-- by the [containers](https://hackage.haskell.org/package/containers).
--
-- The internal 'BWTMatrix' data type relies upon the 'DS.Seq' as well.


module Data.BWT.Internal ( -- * Base BWT types
                           Suffix(..),
                           SuffixArray,
                           BWT(..),
                           BWTMatrix(..),
                           -- * To BWT functions
                           saToBWT,
                           createSuffixArray,
                           -- * From BWT functions
                           sortTB,
                           STBWTCounter,
                           magicInverseBWT,
                           -- * Create BWT Matrix function
                           createBWTMatrix
                         ) where

import Control.Monad as CM
import Control.Monad.ST as CMST
import Data.Maybe as DMaybe (fromJust,isNothing)
import Data.Sequence as DS (Seq(..),empty,findIndexL,fromList,length,index,inits,null,tails,unstableSortBy,unstableSortOn,zip,(><),(|>),(<|))
import Data.STRef as DSTR
import GHC.Generics
import Prelude as P


{-Base level types.-}

-- | Basic suffix data type.  Used to describe
-- the core data inside of the 'SuffixArray' data type.
data Suffix a = Suffix { suffixindex    :: Int
                       , suffixstartpos :: Int
                       , suffix         :: Maybe (Seq a)
                       }
  deriving (Eq,Ord,Show,Read,Generic)

-- | The SuffixArray data type.
-- Uses 'DS.Seq' internally.
type SuffixArray a = Seq (Suffix a)

-- | The BWT data type.
-- Uses 'DS.Seq' internally.
newtype BWT a = BWT (Seq (Maybe a))
  deriving (Eq,Ord,Show,Read,Generic)

-- | The BWTMatrix data type.
-- Uses a 'DMA.Array' internally.
newtype BWTMatrix a = BWTMatrix (Seq (Seq (Maybe a)))
  deriving (Eq,Ord,Show,Read,Generic)

{-------------------}


{-toBWT functions.-}

-- | Computes the Burrows-Wheeler Transform (BWT) using the suffix array
-- and the original string (represented as a 'DS.Seq' for performance).
saToBWT :: SuffixArray a -> Seq a -> Seq (Maybe a)
saToBWT DS.Empty _ = DS.Empty
saToBWT (y DS.:<| ys) t =
  if | suffixstartpos y /= 1
     -> (Just $ DS.index t (suffixstartpos y - 1 - 1))
        DS.<| (saToBWT ys t)
     | otherwise
     -> Nothing
        DS.<| (saToBWT ys t)

-- | Computes the corresponding 'SuffixArray' of a given string. Please see [suffix array](https://en.wikipedia.org/wiki/Suffix_array)
-- for more information.
createSuffixArray :: Ord a
                  => Seq a
                  -> SuffixArray a
createSuffixArray xs =
  fmap (\(a,b,c) -> if | not $ DS.null c
                       -> Suffix { suffixindex    = a
                                 , suffixstartpos = b
                                 , suffix         = Just c
                                 }
                       | otherwise
                       -> Suffix { suffixindex    = a
                                 , suffixstartpos = b
                                 , suffix         = Nothing
                                 }
       )
  xsssuffixesfff
    where
      xsssuffixes         = DS.tails xs
      xsssuffixesf        = DS.zip (DS.fromList [1..(DS.length xsssuffixes)])
                                   xsssuffixes
      xsssuffixesffsorted = DS.unstableSortOn snd xsssuffixesf
      xsssuffixesfff      = (\(a,(b,c)) -> (a,b,c))
                            <$>
                            DS.zip (DS.fromList [1..(DS.length xsssuffixesffsorted)])
                                   xsssuffixesffsorted

{------------------}


{-fromBWT functions.-}

-- | Hierarchical sorting scheme that compares fst first then snd.
-- Necessary for the setting up the BWT in order to correctly
-- invert it using the [Magic](https://www.youtube.com/watch?v=QwSsppKrCj4) algorithm.
sortTB :: (Ord a1,Ord a2)
       => (a1, a2)
       -> (a1, a2)
       -> Ordering
sortTB (c1,i1) (c2,i2) = compare c1 c2 <>
                         compare i1 i2

-- | State function to push BWTString data into stack.
pushSTBWTSeq :: STRef s (Seq a)
             -> a
             -> ST s ()
pushSTBWTSeq s e = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> e)

-- | Abstract BWTCounter and associated state type.
type STBWTCounter s a = STRef s Int

-- | "Magic" Inverse BWT function.
magicInverseBWT :: Seq (Maybe a,Int)
                -> Seq a
magicInverseBWT DS.Empty = CMST.runST $ do
  bwtseqstackempty  <- newSTRef DS.empty
  bwtseqstackemptyr <- readSTRef bwtseqstackempty
  return bwtseqstackemptyr
magicInverseBWT xs       = CMST.runST $ do
  bwtseqstack      <- newSTRef DS.empty
  bwtcounterstackf <- newSTRef (-1)
  bwtcounterstacke <- newSTRef (-1)
  case (DS.findIndexL (\x -> isNothing $ fst x) xs) of
    Nothing           -> do bwtseqstackr <- readSTRef bwtseqstack
                            return bwtseqstackr
    Just nothingindex -> do let nothingfirst = DS.index xs
                                                        nothingindex
                            writeSTRef bwtcounterstacke
                                               nothingindex
                            writeSTRef bwtcounterstackf
                                               (snd nothingfirst)
                            iBWT xs
                                 bwtseqstack
                                 bwtcounterstackf
                                 bwtcounterstacke
                            bwtseqstackr <- readSTRef bwtseqstack
                            return bwtseqstackr
      where
        iBWT ys bwtss bwtcsf bwtcse = do
          cbwtcsf <- readSTRef bwtcsf
          cbwtcse <- readSTRef bwtcse
          CM.when (cbwtcsf /= cbwtcse) $ do
            let next = DS.index ys cbwtcsf
            pushSTBWTSeq bwtss
                         (DMaybe.fromJust $ fst next)
            writeSTRef bwtcsf (snd next)
            iBWT ys
                 bwtss
                 bwtcsf
                 bwtcse

{--------------------}


{-Create BWT Matrix function.-}

-- | Simple yet efficient implementation of converting a given string
-- into a BWT Matrix (the BWTMatrix type is a 'DS.Seq' ('Maybe' a).
createBWTMatrix :: Ord a
                => [a]
                -> BWTMatrix a
createBWTMatrix t =
  BWTMatrix (fmap (\(a,b) -> if | isNothing a
                                -> Nothing DS.<|
                                   (fmap (\x -> Just x) $ fromJust b)
                                | isNothing b
                                -> (fmap (\x -> Just x) $ fromJust a) DS.|>
                                   Nothing
                                | otherwise
                                -> ((fmap (\x -> Just x) $ fromJust a) DS.|> Nothing) DS.><
                                   (fmap (\x -> Just x) $ fromJust b)
                  ) zippedf)
    where
      zippedf    = DS.unstableSortBy (\(a,_) (c,_) -> compare a c)
                   zippedp
      zippedp    = DS.zip suffixesf prefixesf
      suffixesf  = fmap (\x -> if | DS.null x
                                  -> Nothing
                                  | otherwise
                                  -> Just x
                        )
                   suffixes
      prefixesf  = fmap (\x -> if | DS.null x
                                  -> Nothing
                                  | otherwise
                                  -> Just x
                        )
                   prefixes
      suffixes   = DS.tails tseq
      prefixes   = DS.inits tseq
      tseq       = DS.fromList t

{-----------------------------}
