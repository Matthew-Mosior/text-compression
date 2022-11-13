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
-- The implementation of the BWT relies upon sequence provided
-- by the [containers](https://hackage.haskell.org/package/containers).
--
-- The internal 'BWTMatrix' data type relies upon the [massiv](https://hackage.haskell.org/package/massiv) package.


module Data.BWT.Internal where

import Control.Monad as CM
import Control.Monad.ST as CMST
import Control.Monad.State.Strict()
import Data.Foldable as DFold
import Data.List as DL
import Data.Maybe as DMaybe (fromJust,isNothing)
import Data.Sequence as DS (Seq(..),empty,findIndexL,fromList,length,index,inits,null,singleton,tails,unstableSortBy,unstableSortOn,zip,(><),(|>),(<|))
import Data.Massiv.Array as DMA
import Data.Massiv.Core()
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
  deriving (Show,Read,Eq,Ord,Generic)

-- | The SuffixArray data type.
-- Uses sequence internally.
type SuffixArray a = Seq (Suffix a)

-- | The BWT data type.
-- Uses sequence internally.
newtype BWT a = BWT (Seq (Maybe a))
  deriving (Eq,Ord,Show,Read,Generic)


-- | The BWTMatrix data type.
-- Uses a massiv array internally.
type BWTMatrix = DMA.Array BN Ix1 String

{-------------------}


{-toBWT functions.-}

-- | Computes the Burrows-Wheeler Transform (BWT) using the suffix array
-- and the original string (represented as a sequence for performance).
saToBWT :: SuffixArray a ->
           Seq a         ->
           Seq (Maybe a)
saToBWT DS.Empty      _ = DS.Empty
saToBWT (y DS.:<| ys) t =
  if | suffixstartpos y /= 1
     -> (Just $ DS.index t (suffixstartpos y - 1 - 1))
        DS.<| (saToBWT ys t)
     | otherwise
     -> Nothing
        DS.<| (saToBWT ys t)

-- | Computes the corresponding 'SuffixArray' of a given string. Please see [suffix array](https://en.wikipedia.org/wiki/Suffix_array)
-- for more information. 
createSuffixArray :: Ord a =>
                     Seq a ->
                     SuffixArray a
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
sortTB :: (Ord a1, Ord a2) =>
          (a1, a2)         ->
          (a1, a2)         ->
          Ordering
sortTB (c1,i1) (c2,i2) = compare c1 c2 <>
                         compare i1 i2

-- | Abstract BWTSeq type utilizing a sequence.
type BWTSeq a = Seq a

-- | Abstract data type representing a BWTSeq in the (strict) ST monad.
type STBWTSeq s a = STRef s (BWTSeq a)

-- | State function to push BWTString data into stack.
pushSTBWTSeq :: STBWTSeq s a -> a -> ST s ()
pushSTBWTSeq s e = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> e)

-- | State function to create empty STBWTString type.
emptySTBWTSeq :: ST s (STBWTSeq s a)
emptySTBWTSeq = newSTRef DS.empty

-- | Abstract BWTCounter and associated state type.
type STBWTCounter s a = STRef s Int

-- | State function to update BWTCounter.
updateSTBWTCounter :: STBWTCounter s Int -> Int -> ST s ()
updateSTBWTCounter s e = writeSTRef s e

-- | State function to create empty STBWTCounter type.
emptySTBWTCounter :: ST s (STBWTCounter s Int)
emptySTBWTCounter = newSTRef (-1)

-- | "Magic" Inverse BWT function.
magicInverseBWT :: Seq (Maybe a,Int) ->
                   ST s (BWTSeq a)
magicInverseBWT DS.Empty = do
  bwtseqstackempty  <- emptySTBWTSeq
  bwtseqstackemptyr <- readSTRef bwtseqstackempty
  return bwtseqstackemptyr
magicInverseBWT xs       = do
  bwtseqstack      <- emptySTBWTSeq
  bwtcounterstackf <- emptySTBWTCounter
  bwtcounterstacke <- emptySTBWTCounter
  case (DS.findIndexL (\x -> isNothing $ fst x) xs) of
    Nothing           -> do bwtseqstackr <- readSTRef bwtseqstack
                            return bwtseqstackr
    Just nothingindex -> do let nothingfirst = DS.index xs
                                                        nothingindex
                            updateSTBWTCounter bwtcounterstacke
                                               nothingindex
                            updateSTBWTCounter bwtcounterstackf
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
            updateSTBWTCounter bwtcsf
                               (snd next)
            iBWT ys
                 bwtss
                 bwtcsf
                 bwtcse

-- | Simple yet efficient implementation of converting a given string
-- into a BWT Matrix (the BWTMatrix type is a massiv array).
createBWTMatrix :: String ->
                   BWTMatrix
createBWTMatrix t =
  DMA.fromList (ParN 0) zippedffff :: Array BN Ix1 String
    where
      zippedffff = DL.map DFold.toList $
                   DL.map (\(a,b) -> if | isNothing a
                                        -> DS.singleton '$' DS.><
                                           fromJust b
                                        | isNothing b
                                        -> fromJust a DS.><
                                           DS.singleton '$'
                                        | otherwise
                                        -> fromJust a       DS.><
                                           DS.singleton '$' DS.><
                                           fromJust b
                          )
                   zippedfff
      zippedfff  = DFold.toList zippedff
      zippedff   = DS.unstableSortBy (\(a,_) (c,_) -> compare a c)
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

{--------------------}
