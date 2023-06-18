{-# LANGUAGE MultiWayIf             #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE Strict                 #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE RankNTypes             #-}
{-# OPTIONS_GHC -Wno-name-shadowing -Wall #-}


-- |
-- Module      :  Data.FMIndex.Internal
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
-- Various data structures and custom data types to describe the
-- [Full-text Minute-space index (FM-index)](https://en.wikipedia.org/wiki/FM-index)
-- and the Inverse FM-index implementations, namely 'seqToOccCKB', 'seqToOccCKT', 'seqToCcB', 'seqToCcT', 'seqFromFMIndexB', and 'seqFromFMIndexT'.
--
-- The FM-index implementations rely heavily upon 'Seq' provided by the [containers](https://hackage.haskell.org/package/containers) library,
-- 'STRef' and associated functions in the [stref](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-STRef.html) library,
-- and 'runST' in the [Control.Monad.ST](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-ST.html) library.
--
-- = Example FM-index Output
--
-- The below example is taken from [this](https://en.wikipedia.org/wiki/FM-index) wikipedia page.
--
--
--
-- Given the following input, "abracadabra"
--
-- and
--
-- the following Burrows-Wheeler matrix (BWM) of the input "abracadabra":
--
-- +----+---+---------------------------------------+---+
-- | I  | F |                                       | L |
-- +====+===+===+===+===+===+===+===+===+===+===+===+===+
-- | 1  | $ | a | b | r | a | c | a | d | a | b | r | a |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 2  | a | $ | a | b | r | a | c | a | d | a | b | r |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 3  | a | b | r | a | $ | a | b | r | a | c | a | d |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 4  | a | b | r | a | c | a | d | a | b | r | a | $ |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 5  | a | c | a | d | a | b | r | a | $ | a | b | r |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 6  | a | d | a | b | r | a | $ | a | b | r | a | c |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 7  | b | r | a | $ | a | b | r | a | c | a | d | a |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 8  | b | r | a | c | a | d | a | b | r | a | $ | a |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 9  | c | a | d | a | b | r | a | $ | a | b | r | a |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 10 | d | a | b | r | a | $ | a | b | r | a | c | a |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 11 | r | a | $ | a | b | r | a | c | a | d | a | b |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
-- | 12 | r | a | c | a | d | a | b | r | a | $ | a | b |
-- +----+---+---+---+---+---+---+---+---+---+---+---+---+
--
-- The FM-index output of the Burrows-Wheeler transform of the input is:
--
-- C[c] of "ard$rcaaaabb"
--
-- +------+---+---+---+---+---+----+
-- | c    | $ | a | b | c | d | r  |
-- +------+---+---+---+---+---+----+
-- | C[c] | 0 | 1 | 6 | 8 | 9 | 10 |
-- +------+---+---+---+---+---+----+
--
-- and
--
-- Occ(c,k) of "ard$rcaaaabb"
--
-- +---+---+---+---+---+---+---+---+---+---+----+----+----+
-- |   | a | r | d | $ | r | c | a | a | a | a  | b  | b  |
-- +---+---+---+---+---+---+---+---+---+---+----+----+----+
-- |   | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 |
-- +===+===+===+===+===+===+===+===+===+===+====+====+====+
-- | $ | 0 | 0 | 0 | 1 | 1 | 1 | 1 | 1 | 1 | 1  | 1  | 1  |
-- +---+---+---+---+---+---+---+---+---+---+----+----+----+
-- | a | 1 | 1 | 1 | 1 | 1 | 1 | 2 | 3 | 4 | 5  | 5  | 5  |
-- +---+---+---+---+---+---+---+---+---+---+----+----+----+
-- | b | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0  | 1  | 2  |
-- +---+---+---+---+---+---+---+---+---+---+----+----+----+
-- | c | 0 | 0 | 0 | 0 | 0 | 1 | 1 | 1 | 1 | 1  | 1  | 1  |
-- +---+---+---+---+---+---+---+---+---+---+----+----+----+
-- | d | 0 | 0 | 1 | 1 | 1 | 1 | 1 | 1 | 1 | 1  | 1  | 1  |
-- +---+---+---+---+---+---+---+---+---+---+----+----+----+
-- | r | 0 | 1 | 1 | 1 | 2 | 2 | 2 | 2 | 2 | 2  | 2  | 2  |
-- +---+---+---+---+---+---+---+---+---+---+----+----+----+
--
--
--
-- Keep in mind that the __$__ is translated into a __Nothing__.


module Data.FMIndex.Internal ( -- * Base FM-index types
                               FMIndex(..),
                               OccCK(..),
                               Cc(..),
                               SA(..),
                               -- * To OccCK (ByteString) functions
                               seqToOccCK,
                               -- * Cc (ByteString) functions
                               seqToCc,
                               -- * From FMIndex (ByteString) functions
                               seqFromFMIndex,
                               -- * Count (ByteString) operation
                               countFMIndex,
                               -- * Locate (ByteString) operation
                               locateFMIndex,
                             ) where

import Data.BWT.Internal (SuffixArray)
import Data.MTF.Internal

import Control.Monad as CM
import Control.Monad.ST as CMST
import Data.RLE.Internal (Pack)
import Data.Sequence as DS (Seq(..),ViewL(..),ViewR(..),empty,findIndexL,fromList,index,length,(|>))
import Data.Sequence.Internal as DSI
import Data.STRef as DSTR
import GHC.Generics (Generic)
import Prelude as P


{-Base level types.-}

-- | Basic FMIndex ('ByteString') data type.
newtype FMIndex b = FMIndex (Cc b,OccCK b,SA b)
  deriving (Eq,Ord,Show,Read,Generic)

-- | Basic OccCKB ('ByteString') data type.
newtype OccCK b = OccCK (Seq (Maybe b,Seq (Int,Int,Maybe b)))
  deriving (Eq,Ord,Show,Read,Generic)

-- | Basic C[c] table ('ByteString') data type.
newtype Cc b = Cc (Seq (Int,Maybe b))
  deriving (Eq,Ord,Show,Read,Generic)

-- | Basic count ('ByteString') operation data type.
newtype CB = CB (Maybe Int)
  deriving (Eq,Ord,Show,Read,Generic)

-- | Basic 'SuffixArray' ('ByteString') data type.
newtype SA b = SA (SuffixArray b)
  deriving (Eq,Ord,Show,Read,Generic)

{-------------------}

-- | State function to update 'OccCKSeqB'
-- with each step of the OccCK.
updateSTOccCKSeqAB :: STRef s (Seq (Maybe b,Seq (Int,Int,Maybe b)))
                   -> (Int,Int,Maybe b)
                   -> ST s ()
updateSTOccCKSeqAB s e = do
  s2 <- readSTRef s
  case viewr s2 of
    EmptyR           -> pure ()
    (s2h DS.:> s2fm) -> writeSTRef s (s2h DS.|> (((\(a,_) -> a) s2fm),((\(_,b) -> b) s2fm) DS.|> e))

-- | State function to update 'OccCKSeqB'
-- with each step of the OccCK.
updateSTOccCKSeqBB :: STRef s (Seq (Maybe b,Seq (Int,Int,Maybe b)))
                   -> Maybe b
                   -> ST s ()
updateSTOccCKSeqBB s e = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> (e,DS.empty))

-- | Strict state monad function.
seqToOccCK :: forall b. (Pack b, Ord b) => Seq (Maybe b) -> Seq (Maybe b,Seq (Int,Int,Maybe b))
seqToOccCK DS.Empty      = CMST.runST $ do
  boccckseqstackempty  <- newSTRef DS.empty
  boccckseqstackemptyr <- readSTRef boccckseqstackempty
  return boccckseqstackemptyr
seqToOccCK xs            = CMST.runST $ do
  boccckseqstack     <- newSTRef DS.empty
  boccckinitiallist  <- newSTRef DS.empty
  boccckcounterstack <- newSTRef 0
  let il = nubSeq' xs
  writeSTRef boccckinitiallist
                 il
  cboccckinitiallist <- readSTRef boccckinitiallist
  iOccCKB cboccckinitiallist
          xs
          boccckseqstack
          boccckcounterstack
  boccckseqstackr <- readSTRef boccckseqstack
  return boccckseqstackr
    where
      iOccCKB DS.Empty      _      _        _        = pure ()
      iOccCKB (y DS.:<| ys) zs     boccckss boccckcs = do
        boccckis <- newSTRef 0
        writeSTRef boccckis
                              1
        updateSTOccCKSeqBB boccckss
                           y
        iiOccCKB y
                 zs
                 boccckss
                 boccckis
                 boccckcs
        iOccCKB ys
                zs
                boccckss
                boccckcs
      iiOccCKB _  DS.Empty      _        _        boccckcs = do
        writeSTRef boccckcs
                              0
        pure ()
      iiOccCKB as (b DS.:<| bs) boccckss boccckis boccckcs = do
        cboccckis <- readSTRef boccckis
        cboccckcs <- readSTRef boccckcs
        if | as == b
           -> do updateSTOccCKSeqAB boccckss
                                    (cboccckis,cboccckcs + 1,b)
                 writeSTRef boccckcs
                                       (cboccckcs + 1)
                 writeSTRef boccckis
                                       (cboccckis + 1)
                 iiOccCKB as
                          bs
                          boccckss
                          boccckis
                          boccckcs
           | otherwise
           -> do updateSTOccCKSeqAB boccckss
                                    (cboccckis,cboccckcs,b)
                 writeSTRef boccckis
                                       (cboccckis + 1)
                 iiOccCKB as
                          bs
                          boccckss
                          boccckis
                          boccckcs

{---------------------------}

{-To Cc (ByteString) functions.-}

-- | State function to update 'CcSeqB'
-- with each step of the C[c].
updateSTCcSeqB :: STRef s (Seq (Int,Maybe b))
               -> (Int,Maybe b)
               -> ST s ()
updateSTCcSeqB s e = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> e)

-- | Strict state monad function.
seqToCc :: Ord b => Seq (Maybe b) -> Seq (Int,Maybe b)
seqToCc DS.Empty      = CMST.runST $ do
  bccseqstackempty  <- newSTRef DS.empty
  bccseqstackemptyr <- readSTRef bccseqstackempty
  return bccseqstackemptyr
seqToCc xs            = CMST.runST $ do
  bccseqstack     <- newSTRef DS.empty
  bccinitiallist  <- newSTRef DS.empty
  bcccounterstack <- newSTRef 0
  let il = nubSeq' xs
  writeSTRef bccinitiallist il
  cbccinitiallist <- readSTRef bccinitiallist
  iCcB cbccinitiallist
       xs
       bccseqstack
       bcccounterstack
  bccseqstackr <- readSTRef bccseqstack
  return bccseqstackr
    where
      iCcB DS.Empty      _      _        _  = pure ()
      iCcB (y DS.:<| ys) zs     bccss bcccs = do
        writeSTRef bcccs 0
        iiCcB y
              zs
              bccss
              bcccs
        iCcB ys
             zs
             bccss
             bcccs
      iiCcB _  DS.Empty      _     _     = pure ()
      iiCcB as (b DS.:<| bs) bccss bcccs = do
        cbcccs <- readSTRef bcccs
        if | as == b
           -> updateSTCcSeqB bccss
                             (cbcccs,as)
           | otherwise
           -> do writeSTRef bcccs (cbcccs + 1)
                 iiCcB as
                       bs
                       bccss
                       bcccs

{-------------------------}


{-fromFMIndex functions.-}

-- | Simple Inverse FMIndex function.
seqFromFMIndex :: FMIndex b -> Seq (Maybe b)
seqFromFMIndex (FMIndex (Cc DS.Empty,_,_))    = DS.Empty
seqFromFMIndex (FMIndex (_,OccCK DS.Empty,_)) = DS.Empty
seqFromFMIndex (FMIndex (_,_,SA DS.Empty))    = DS.Empty
seqFromFMIndex xs                               = do
  let xss = (\(OccCK b) -> b) $
            (\(_,b,_) -> b)    $
            (\(FMIndex b) -> b) xs
  iFFMIndexB xss
    where
      iFFMIndexB DS.Empty = DS.Empty
      iFFMIndexB ((_,b) DS.:<| _) =
        fmap (\(_,_,e) -> e) b

{-------------------------------}


{-Count (ByteString) operation.-}

-- | Count operation on a 'FMIndexB'.
-- This operation takes a pattern ('Seq' 'ByteString')
-- and returns the number of occurences of that pattern
-- in the original text T [credit](https://en.wikipedia.org/wiki/FM-index).
countFMIndex :: forall b. Pack b => Seq b -> FMIndex b -> Maybe Int
countFMIndex DS.Empty _ = CMST.runST $ return Nothing
countFMIndex _ (FMIndex (Cc DS.Empty,_,_))    = CMST.runST $ return Nothing
countFMIndex _ (FMIndex (_,OccCK DS.Empty,_)) = CMST.runST $ return Nothing
countFMIndex _ (FMIndex (_,_,SA DS.Empty))    = CMST.runST $ return Nothing
countFMIndex xs       ys                               = CMST.runST $ do
  bccounter      <- newSTRef (0 :: Int)
  bcbool         <- newSTRef False
  bccurrentstart <- newSTRef (-1)
  bccurrentend   <- newSTRef (-1)
  iCB xs
      ys
      bccounter
      bcbool
      bccurrentstart
      bccurrentend
  cbccurrentstart <- readSTRef bccurrentstart
  cbccurrentend   <- readSTRef bccurrentend
  cbcbool         <- readSTRef bcbool
  let count = if | (cbccurrentstart == (-1) && cbccurrentend == (-1)) ||
                   ((cbccurrentend - cbccurrentstart) + 1) == 0       ||
                   cbcbool
                 -> Nothing
                 | otherwise
                 -> Just ((cbccurrentend - cbccurrentstart) + 1)
  return count
    where
      iCB DS.Empty      _  _   _   _    _    = pure ()
      iCB (as DS.:|> a) bs bcc bcb bccs bcce = do
        let ccbbs = (\(Cc b) -> b) $
                    (\(a,_,_) -> a) $
                    (\(FMIndex b) -> b) bs
        let coccckbs = (\(OccCK b) -> b) $
                       (\(_,b,_) -> b)    $
                       (\(FMIndex b) -> b) bs
        cbcc <- readSTRef bcc
        cbccs <- readSTRef bccs
        cbcce <- readSTRef bcce
        if | cbccs > cbcce
           -> do writeSTRef bcb True
                 pure ()
           | otherwise
           -> if | cbcc == 0
                 -> do case DS.findIndexL (\(_,d) -> d == Just a) ccbbs of
                         Nothing     -> pure ()
                         Just bindex -> do if | bindex == (DS.length ccbbs) - 1
                                              -> do let istart = (fst $ DS.index ccbbs bindex) + 1
                                                    let iend   = case viewl coccckbs of
                                                                   EmptyL      -> (-1)
                                                                   (x DS.:< _) -> DS.length $
                                                                                  snd x
                                                    writeSTRef bccs istart
                                                    writeSTRef bcce iend
                                                    writeSTRef bcc 1
                                                    iCB as
                                                        bs
                                                        bcc
                                                        bcb
                                                        bccs
                                                        bcce
                                              | otherwise
                                              -> do let istart = (fst $ DS.index ccbbs bindex) + 1
                                                    let iend   = fst $ DS.index ccbbs (bindex + 1)
                                                    writeSTRef bccs istart
                                                    writeSTRef bcce iend
                                                    writeSTRef bcc 1
                                                    iCB as
                                                        bs
                                                        bcc
                                                        bcb
                                                        bccs
                                                        bcce
                 | otherwise
                 -> do case DS.findIndexL (\(_,d) -> d == Just a) ccbbs of
                         Nothing     -> pure ()
                         Just bindex -> do case DS.findIndexL (\(e,_) -> e == Just a) coccckbs of
                                             Nothing     -> pure ()
                                             Just cindex -> do let istart = (fst $ DS.index ccbbs bindex)                               +
                                                                            ((\(_,b,_) -> b) $
                                                                             DS.index (snd $ DS.index coccckbs cindex) (cbccs - 1 - 1)) +
                                                                            1
                                                               let iend   = (fst $ DS.index ccbbs bindex) +
                                                                            ((\(_,b,_) -> b) $
                                                                             DS.index (snd $ DS.index coccckbs cindex) (cbcce - 1))
                                                               writeSTRef bccs istart
                                                               writeSTRef bcce iend
                                                               iCB as
                                                                   bs
                                                                   bcc
                                                                   bcb
                                                                   bccs
                                                                   bcce

{-------------------------------}

{-Locate operation.-}

-- | Locate operation on a 'FMIndexB'.
-- This operation takes a pattern ('Seq' 'b')
-- and returns the indexe(s) of occurences of that pattern
-- in the original text T [credit](https://en.wikipedia.org/wiki/FM-index).
locateFMIndex :: Eq b => Seq b -> FMIndex b -> Seq (Maybe Int)
locateFMIndex DS.Empty _ = CMST.runST $ return DS.Empty
locateFMIndex _ (FMIndex (Cc DS.Empty,_,_))    = CMST.runST $ return DS.Empty
locateFMIndex _ (FMIndex (_,OccCK DS.Empty,_)) = CMST.runST $ return DS.Empty
locateFMIndex _ (FMIndex (_,_,SA DS.Empty))    = CMST.runST $ return DS.Empty
locateFMIndex xs       ys                               = CMST.runST $ do
  blcounter      <- newSTRef (0 :: Int)
  blbool         <- newSTRef False
  blcurrentstart <- newSTRef (-1)
  blcurrentend   <- newSTRef (-1)
  iLB xs
      ys
      blcounter
      blbool
      blcurrentstart
      blcurrentend
  cblcurrentstart <- readSTRef blcurrentstart
  cblcurrentend   <- readSTRef blcurrentend
  cblbool         <- readSTRef blbool
  let indexes = if | (cblcurrentstart == (-1) && cblcurrentend == (-1)) ||
                     ((cblcurrentend - cblcurrentstart) + 1) == 0       ||
                     cblbool
                   -> DS.Empty
                   | otherwise
                   -> fmap Just   $
                      DS.fromList $
                      [cblcurrentstart..cblcurrentend]
  return indexes
    where
      iLB DS.Empty      _  _   _   _    _    = pure ()
      iLB (as DS.:|> a) bs blc blb blcs blce = do
        let ccbbs = (\(Cc b) -> b) $
                    (\(a,_,_) -> a) $
                    (\(FMIndex b) -> b) bs
        let coccckbs = (\(OccCK b) -> b) $
                       (\(_,b,_) -> b)    $
                       (\(FMIndex b) -> b) bs
        cblc <- readSTRef blc
        cblcs <- readSTRef blcs
        cblce <- readSTRef blce
        if | cblcs > cblce
           -> do writeSTRef blb
                                True
                 pure ()
           | otherwise
           -> if | cblc == 0
                 -> do case DS.findIndexL (\(_,d) -> d == Just a) ccbbs of
                         Nothing     -> pure ()
                         Just bindex -> do if | bindex == (DS.length ccbbs) - 1
                                              -> do let istart = (fst $ DS.index ccbbs bindex) + 1
                                                    let iend   = case viewl coccckbs of
                                                                   EmptyL      -> (-1)
                                                                   (x DS.:< _) -> DS.length $
                                                                                  snd x
                                                    writeSTRef blcs istart
                                                    writeSTRef blce iend
                                                    writeSTRef blc 1
                                                    iLB as
                                                        bs
                                                        blc
                                                        blb
                                                        blcs
                                                        blce
                                              | otherwise
                                              -> do let istart = (fst $ DS.index ccbbs bindex) + 1
                                                    let iend   = fst $ DS.index ccbbs (bindex + 1)
                                                    writeSTRef blcs istart
                                                    writeSTRef blce iend
                                                    writeSTRef blc 1
                                                    iLB as
                                                        bs
                                                        blc
                                                        blb
                                                        blcs
                                                        blce
                 | otherwise
                 -> do case DS.findIndexL (\(_,d) -> d == Just a) ccbbs of
                         Nothing     -> pure ()
                         Just bindex -> do case DS.findIndexL (\(e,_) -> e == Just a) coccckbs of
                                             Nothing     -> pure ()
                                             Just cindex -> do let istart = (fst $ DS.index ccbbs bindex)                               +
                                                                            ((\(_,b,_) -> b) $
                                                                             DS.index (snd $ DS.index coccckbs cindex) (cblcs - 1 - 1)) +
                                                                            1
                                                               let iend   = (fst $ DS.index ccbbs bindex) +
                                                                            ((\(_,b,_) -> b) $
                                                                             DS.index (snd $ DS.index coccckbs cindex) (cblce - 1))
                                                               writeSTRef blcs istart
                                                               writeSTRef blce iend
                                                               iLB as
                                                                   bs
                                                                   blc
                                                                   blb
                                                                   blcs
                                                                   blce
