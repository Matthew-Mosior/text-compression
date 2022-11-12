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
-- The implementation of the BWT relies upon Boxed vectors, 'DVB.Vector', and Unboxed vectors, 'DVU.Vector',
-- provided by the [vector](https://hackage.haskell.org/package/vector).
--
-- The internal 'BWTMatrix' data type relies upon the [massiv](https://hackage.haskell.org/package/massiv) package.


module Data.BWT.Internal where

import Control.Monad as CM (when)
import Control.Monad.ST as CMST (ST,runST)
import Control.Monad.State.Strict()
import Data.Foldable as DFold
import Data.List as DL (map)
import Data.Maybe as DMaybe (fromJust,isNothing)
import Data.Sequence as DS (fromList,(><),null,singleton,zip,sortBy,tails,inits)
import Data.Massiv.Array as DMA
import Data.Massiv.Core()
import Data.STRef as DSTR (STRef,newSTRef,readSTRef,writeSTRef)
import Data.Vector as DVB (Vector,length,cons,empty,findIndex,iterateN,map,snoc,thaw,unsafeFreeze,zip,uncons,(!))
import Data.Vector.Algorithms.Tim as DVAT (sortBy)
import Data.Vector.Unboxed as DVU (Vector,null,tail,uncons,(!))
import GHC.Generics (Generic)
import Prelude as P


{-Base level types.-}

-- | Basic suffix data type.  Used to describe
-- the core data inside of the 'SuffixArray' data type.
data Suffix a = Suffix { suffixindex    :: Int
                       , suffixstartpos :: Int
                       , suffix         :: Maybe (DVU.Vector a)
                       }
  deriving (Show,Read,Eq,Ord,Generic)

-- | The SuffixArray data type.
-- Uses 'DVB.Vector' internally.
type SuffixArray a = DVB.Vector (Suffix a)

-- | The BWT data type.
-- Uses 'DVU.Vector' internally.
newtype BWT a = BWT (DVB.Vector (Maybe a))
  deriving (Eq,Ord,Show,Read,Generic)

-- | The BWTMatrix data type.
-- Uses a massiv array internally.
type BWTMatrix = DMA.Array BN Ix1 String

{-------------------}


{-toBWT functions.-}

-- | Computes the Burrows-Wheeler Transform (BWT) using the suffix array
-- and the original string (represented as a 'DVB.Vector' for performance).
saToBWT :: Unbox a
        => SuffixArray a
        -> DVU.Vector a
        -> BWT a
saToBWT (DVB.uncons -> Nothing) _ = BWT DVB.empty
saToBWT vs                      t =
  BWT
    (DVB.map (\v -> if | suffixstartpos v /= 1
                       -> Just $
                          (DVU.!) t (suffixstartpos v - 1 - 1)
                       | otherwise
                       -> Nothing
             )
     vs)

-- | 'DVU.Vector' based implementation of the
-- well-known tails function in the List library.
tailsV :: Unbox a
       => DVU.Vector a
       -> DVB.Vector (DVU.Vector a)
tailsV (DVU.uncons -> Nothing) = DVB.empty
tailsV vs                      =
  DVB.cons vs (tailsV (DVU.tail vs))

-- | Custom sort function for 'DVB.Vector's
-- used in the 'createSuffixArray' function.
sortVecSA :: Ord a
          => DVB.Vector (Int,a)
          -> DVB.Vector (Int,a)
sortVecSA vs =
  CMST.runST
    (do mv <- DVB.thaw vs
        DVAT.sortBy (\(_,b) (_,d) -> compare b d) mv
        DVB.unsafeFreeze mv)

-- | Custom sort function for 'DVB.Vector's
-- used in the fromBWT function.
sortVecBWT :: Ord a
           => DVB.Vector (a,Int)
           -> DVB.Vector (a,Int)
sortVecBWT vs =
  CMST.runST
    (do mv <- DVB.thaw vs
        DVAT.sortBy (\(a,b) (c,d) -> sortTB (a,b) (c,d)) mv
        DVB.unsafeFreeze mv) 

-- | Computes the corresponding 'SuffixArray' of a given string. Please see [suffix array](https://en.wikipedia.org/wiki/Suffix_array)
-- for more information.
createSuffixArray :: (Unbox a,Ord a)
                    => DVU.Vector a 
                    -> SuffixArray a
createSuffixArray vs =
  DVB.map (\(a,b,c) -> if | not $ DVU.null c
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
  vssuffixesfff
    where
      vssuffixes         = tailsV vs
      vssuffixesf        = DVB.zip (DVB.iterateN (DVB.length vssuffixes) (+1) 1 :: DVB.Vector Int)
                                   vssuffixes
      vssuffixesffsorted = sortVecSA vssuffixesf
      vssuffixesfff      = (\(a,(b,c)) -> (a,b,c))
                           <$>
                           DVB.zip (DVB.iterateN (DVB.length vssuffixesffsorted) (+1) 1 :: DVB.Vector Int)
                                   vssuffixesffsorted 

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
type BWTVec a = DVB.Vector a

-- | Abstract data type representing a 'BWTVec' in the (strict) ST monad.
type STBWTVec s a = STRef s (BWTVec a)

-- | State function to push 'BWTVec' data into stack.
pushSTBWTVec :: STBWTVec s a -> a -> ST s ()
pushSTBWTVec s e = do
  s2 <- readSTRef s
  writeSTRef s (DVB.snoc s2 e)

-- | State function to create empty 'STBWTVec' type.
emptySTBWTVec :: ST s (STBWTVec s a)
emptySTBWTVec = newSTRef DVB.empty

-- | Abstract BWTCounter and associated state type.
type STBWTCounter s a = STRef s Int

-- | State function to update BWTCounter.
updateSTBWTCounter :: STBWTCounter s Int -> Int -> ST s ()
updateSTBWTCounter s e = writeSTRef s e

-- | State function to create empty STBWTCounter type.
emptySTBWTCounter :: ST s (STBWTCounter s Int)
emptySTBWTCounter = newSTRef (-1)

-- | "Magic" Inverse BWT function.
magicInverseBWT :: DVB.Vector (Maybe a,Int) ->
                   ST s (BWTVec a)
magicInverseBWT (DVB.uncons -> Nothing) = do
  bwtvecstackempty  <- emptySTBWTVec
  bwtvecstackemptyr <- readSTRef bwtvecstackempty
  return bwtvecstackemptyr
magicInverseBWT xs                      = do
  bwtvecstack      <- emptySTBWTVec
  bwtcounterstackf <- emptySTBWTCounter
  bwtcounterstacke <- emptySTBWTCounter
  case (DVB.findIndex (\x -> isNothing $ fst x) xs) of
    Nothing           -> do bwtvecstackr <- readSTRef bwtvecstack
                            return bwtvecstackr
    Just nothingindex -> do let nothingfirst = (DVB.!) xs
                                                        nothingindex
                            updateSTBWTCounter bwtcounterstacke
                                               nothingindex
                            updateSTBWTCounter bwtcounterstackf
                                               (snd nothingfirst)
                            iBWT xs
                                 bwtvecstack
                                 bwtcounterstackf
                                 bwtcounterstacke
                            bwtvecstackr <- readSTRef bwtvecstack
                            return bwtvecstackr
      where
        iBWT ys bwtvs bwtcsf bwtcse = do
          cbwtcsf <- readSTRef bwtcsf
          cbwtcse <- readSTRef bwtcse
          CM.when (cbwtcsf /= cbwtcse) $ do 
            let next = (DVB.!) ys cbwtcsf
            pushSTBWTVec bwtvs
                         (DMaybe.fromJust $ fst next)
            updateSTBWTCounter bwtcsf
                               (snd next)
            iBWT ys
                 bwtvs
                 bwtcsf
                 bwtcse

-- | Simple yet efficient implementation of converting a given string
-- into a BWT Matrix (the BWTMatrix type is a massiv array).
createBWTMatrix :: String ->
                   BWTMatrix
createBWTMatrix t =
  DMA.fromList (ParN 0) zippedffff :: DMA.Array BN Ix1 String
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
      zippedff   = DS.sortBy (\(a,_) (c,_) -> compare a c)
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
