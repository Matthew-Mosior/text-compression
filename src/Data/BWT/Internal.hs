{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}


module Data.BWT.Internal where

import Control.Monad as CM
import Control.Monad.ST as CMST
import Control.Monad.State.Strict()
import Data.Foldable as DFold
import Data.List as DL
import Data.Sequence as DS
import Data.Massiv.Array as DMA
import Data.Massiv.Core()
import Data.STRef as DSTR
import GHC.Generics
import Prelude as P


{-Base level types.-}

-- | Basic suffix data type.  Used to describe
-- the core data inside of the SuffixArray data type.
data Suffix = Suffix { suffixindex    :: Int
                     , suffixstartpos :: Int
                     , suffix         :: Seq Char
                     }
  deriving (Show,Read,Eq,Ord,Generic)

-- | The SuffixArray data type. Uses Data.Sequence internally.
type SuffixArray = Seq Suffix

-- | The BWT data type. Uses Data.Sequence internally.
type BWT         = Seq Char

-- | The BWTMatrix data type. Uses Data.Massiv internally.
type BWTMatrix   = DMA.Array BN Ix1 String

{-------------------}


{-toBWT functions.-}

-- | Computes the Burrows-Wheeler Transform (BWT) using the suffix array
-- and the original string (represented as a sequence for performance).
saToBWT :: SuffixArray -> Seq Char -> BWT
saToBWT DS.Empty _ = DS.Empty
saToBWT (y DS.:<| ys) t =
  if | suffixstartpos y /= 1
     -> DS.index t (suffixstartpos y - 1 - 1)
        DS.<| (saToBWT ys t)
     | otherwise
     -> DS.index t (DS.length t - 1)
        DS.<| (saToBWT ys t)

-- | Computes the corresponding suffix array of a given string.
createSuffixArray :: Seq Char -> SuffixArray
createSuffixArray xs =
  fmap (\x -> Suffix { suffixindex    = ((\(a,_,_) -> a) x)
                     , suffixstartpos = ((\(_,b,_) -> b) x)
                     , suffix         = ((\(_,_,c) -> c) x)
                     }
       ) xsssuffixesfff
    where
      xsssuffixes         = DS.tails xs
      xsssuffixesf        = DS.zip (DS.fromList [1..(DS.length xsssuffixes)])
                                   xsssuffixes
      xsssuffixesff       = DS.filter (\(_,b) -> not $ DS.null b)
                                      xsssuffixesf
      xsssuffixesffsorted = DS.sortOn snd xsssuffixesff
      xsssuffixesfff      = (\(a,(b,c)) -> (a,b,c))
                            <$>
                            DS.zip (DS.fromList [1..(DS.length xsssuffixesffsorted)])
                                   xsssuffixesffsorted

{------------------}


{-fromBWT functions.-}

-- | Hierarchical sorting scheme that compares fst first then snd.
-- Necessary for the setting up the BWT in order to correctly
-- invert it using the "Magic" algorithm.
sortTB :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Ordering
sortTB (c1,i1) (c2,i2) = compare c1 c2 <>
                         compare i1 i2

-- | Abstract BWTString and associated state type.
type BWTSeq a = Seq Char
type STBWTSeq s a = STRef s (BWTSeq Char)

-- | State function to push BWTString data into stack.
pushSTBWTSeq :: STBWTSeq s Char -> Char -> ST s ()
pushSTBWTSeq s e = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> e)

-- | State function to create empty STBWTString type.
emptySTBWTSeq :: ST s (STBWTSeq s Char)
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
magicInverseBWT :: Seq (Char,Int) -> ST s (BWTSeq Char)
magicInverseBWT DS.Empty = do
  bwtseqstackempty <- emptySTBWTSeq
  bwtseqstackemptyr <- readSTRef bwtseqstackempty
  return bwtseqstackemptyr
magicInverseBWT xs       = do
  bwtseqstack <- emptySTBWTSeq
  bwtcounterstack <- emptySTBWTCounter
  case (DS.findIndexL ((== '$') . fst) xs) of
    Nothing              -> do bwtseqstackr <- readSTRef bwtseqstack
                               return bwtseqstackr
    Just dollarsignindex -> do let dollarsignfirst = DS.index xs
                                                              dollarsignindex
                               updateSTBWTCounter bwtcounterstack
                                                  (snd dollarsignfirst)
                               iBWT xs
                                    bwtseqstack
                                    bwtcounterstack
                               bwtseqstackr <- readSTRef bwtseqstack
                               return bwtseqstackr
      where
        iBWT ys bwtss bwtcs = do
          cbwtcs <- readSTRef bwtcs
          cbwtss <- readSTRef bwtss
          CM.when (DS.length cbwtss < DS.length ys) $ do
            let next = DS.index ys cbwtcs
            pushSTBWTSeq bwtss
                         (fst next)
            updateSTBWTCounter bwtcs
                               (snd next)
            iBWT ys bwtss bwtcs

-- | Easy way to grab the first two elements of a sequence.
grabHeadChunks :: Seq (Seq Char) -> (Seq Char,Seq Char)
grabHeadChunks DS.Empty       = (DS.Empty,DS.Empty)
grabHeadChunks (x1 DS.:<| xs) = (x1,grabHeadChunksInternal xs)
    where
      grabHeadChunksInternal :: Seq (Seq Char) -> Seq Char
      grabHeadChunksInternal DS.Empty       = DS.Empty
      grabHeadChunksInternal (y1 DS.:<| _) = y1

-- | Simple yet efficient implementation of converting a given string
-- into a BWT Matrix (the BWTMatrix type is a massiv array).
createBWTMatrix :: String -> BWTMatrix
createBWTMatrix t =
  DMA.fromList (ParN 0) zippedfff :: Array BN Ix1 String   
    where
      zippedfff = DL.map DFold.toList          $
                  DL.map (\(a,b) -> a DS.>< b) $
                  DFold.toList zippedff
      zippedff  = DS.sortBy (\(a,_) (c,_) -> compare a c)
                  zippedf
      zippedf   = zippedh
                  DS.><
                  zippedp
      zippedh   = DS.singleton   $
                  grabHeadChunks $
                  DS.chunksOf ((DS.length tseq) - 1)
                              tseq
      zippedp   = DS.zip suffixesf prefixesf
      prefixesf = DS.take ((DS.length prefixes) - 1)
                          prefixes
      suffixesf = DS.drop 1
                          suffixes
      suffixes = DS.filter (not . DS.null)
                 (DS.tails tseq)
      prefixes = DS.filter (not . DS.null)
                 (DS.inits tseq)
      tseq = (DS.fromList t) DS.|> '$'

{--------------------}
