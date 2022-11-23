{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}


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
-- and the Inverse FM-index implementations, namely 'seqToFMIndexB', 'seqToFMIndexT', 'seqFromFMIndexB', and 'seqFromFMIndexT'.
--
-- The FM-index implementations rely heavily upon 'Seq' provided by the [containers](https://hackage.haskell.org/package/containers),
-- 'STRef' and associated functions in the [stref](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-STRef.html) library,
-- and 'runST' in the [Control.Monad.ST](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-ST.html) library.


module Data.FMIndex.Internal where

import Data.BWT.Internal()
import Data.MTF.Internal

import Control.Monad as CM
import Control.Monad.ST as CMST
import Control.Monad.State.Strict()
import Data.ByteString as BS
import Data.ByteString.Char8()
import Data.ByteString.Internal()
import Data.Foldable()
import Data.List()
import Data.Maybe()
import Data.Sequence as DS (Seq(..),ViewR(..),empty,(|>))
import Data.Sequence.Internal as DSI
import Data.STRef as DSTR
import Data.Text as DText
import GHC.Generics (Generic)
import Prelude as P


{-Base level types.-}

-- | Basic FMIndex ('ByteString') data type.
newtype FMIndexB = FMIndexB (Seq (Maybe ByteString,Seq (Int,Int,Maybe ByteString)))
  deriving (Eq,Ord,Show,Read,Generic)

-- | Basic FMIndex ('Text') data type.
newtype FMIndexT = FMIndexT (Seq (Maybe Text,Seq (Int,Int,Maybe Text)))
  deriving (Eq,Ord,Show,Read,Generic)

{-------------------}


{-toFMIndex (ByteString) functions.-}

-- | Abstract 'PBFMIndexSeqB' type utilizing a 'Seq'.
type PBFMIndexSeqB = Seq (Maybe ByteString)

-- | Abstract 'FMIndexSeqB' type utilizing a 'Seq'.
-- (c,(indexofinputcurrentelement,Occ(c,k),inputcurrentelement))
-- Please see [this](https://en.wikipedia.org/wiki/FM-index)
-- for an explanation of the above abbreviations.
type FMIndexSeqB = Seq (Maybe ByteString,Seq (Int,Int,Maybe ByteString))

-- | Abstract data type representing a 'FMIndexSeqB' in the (strict) ST monad.
type STFMIndexSeqB s a = STRef s FMIndexSeqB

-- | State function to update 'FMIndexSeqB'
-- with each step of the FMIndex.
updateSTFMIndexSeqAB :: STFMIndexSeqB s (Seq (Maybe ByteString,Seq (Int,Int,Maybe ByteString)))
                     -> (Int,Int,Maybe ByteString)
                     -> ST s ()
updateSTFMIndexSeqAB s e = do
  s2 <- readSTRef s
  case viewr s2 of
    EmptyR           -> pure ()
    (s2h DS.:> s2fm) -> writeSTRef s (s2h DS.|> (((\(a,_) -> a) s2fm),((\(_,b) -> b) s2fm) DS.|> e))

-- | State function to update 'FMIndexSeqB'
-- with each step of the FMIndex.
updateSTFMIndexSeqBB :: STFMIndexSeqB s (Seq (Maybe ByteString,Seq (Int,Int,Maybe ByteString)))
                     -> Maybe ByteString
                     -> ST s ()
updateSTFMIndexSeqBB s e = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> (e,DS.empty))

-- | State function to create empty 'STFMIndexSeqB' type.
emptySTFMIndexSeqB :: ST s (STFMIndexSeqB s a)
emptySTFMIndexSeqB = newSTRef DS.empty

-- | Abstract 'STFMIndexILB' and associated state type.
type STFMIndexILB s a = STRef s (Seq (Maybe ByteString))

-- | State function to load list into 'STFMIndexILB'.
loadSTFMIndexILB :: STMTFILB s (Maybe ByteString)
                 -> Seq (Maybe ByteString)
                 -> ST s ()
loadSTFMIndexILB s e = writeSTRef s e

-- | State function to create empty 'STFMIndexILB' type.
emptySTFMIndexILB :: ST s (STFMIndexILB s a)
emptySTFMIndexILB = newSTRef DS.empty

-- | Abstract 'STFMIndexCounterB' and associated state type.
type STFMIndexCounterB s a = STRef s Int

-- | State function to update 'STFMIndexCounterB'.
updateSTFMIndexCounterB :: STFMIndexCounterB s Int
                        -> Int
                        -> ST s ()
updateSTFMIndexCounterB s e = writeSTRef s e

-- | State function to create empty 'STFMIndexCounterB' type.
emptySTFMIndexCounterB :: ST s (STFMIndexCounterB s Int)
emptySTFMIndexCounterB = newSTRef 0 

-- | Strict state monad function.
seqToFMIndexB :: PBFMIndexSeqB
              -> ST s FMIndexSeqB
seqToFMIndexB DS.Empty      = do
  bfmiseqstackempty  <- emptySTFMIndexSeqB
  bfmiseqstackemptyr <- readSTRef bfmiseqstackempty
  return bfmiseqstackemptyr
seqToFMIndexB xs            = do
  bfmiseqstack     <- emptySTFMIndexSeqB 
  bfmiinitiallist  <- emptySTFMIndexILB
  bfmicounterstack <- emptySTFMIndexCounterB
  let il = nubSeq' xs
  loadSTFMIndexILB bfmiinitiallist
                   il
  cbfmiinitiallist <- readSTRef bfmiinitiallist
  iFMIndexB cbfmiinitiallist
            xs
            bfmiseqstack
            bfmicounterstack
  bfmiseqstackr <- readSTRef bfmiseqstack
  return bfmiseqstackr
    where
      iFMIndexB DS.Empty      _      _      _      = pure ()
      iFMIndexB (y DS.:<| ys) zs     bfmiss bfmics = do
        bfmiis <- emptySTFMIndexCounterB
        updateSTFMIndexCounterB bfmiis
                                1
        updateSTFMIndexSeqBB bfmiss
                             y
        iiFMIndexB y
                   zs
                   bfmiss
                   bfmiis
                   bfmics                           
        iFMIndexB ys
                  zs
                  bfmiss
                  bfmics
      iiFMIndexB _  DS.Empty      _      _      bfmics = do
        updateSTFMIndexCounterB bfmics
                                0
        pure ()
      iiFMIndexB as (b DS.:<| bs) bfmiss bfmiis bfmics = do
        cbfmiis <- readSTRef bfmiis
        cbfmics <- readSTRef bfmics
        if | as == b
           -> do updateSTFMIndexSeqAB bfmiss
                                      (cbfmiis,cbfmics + 1,b)
                 updateSTFMIndexCounterB bfmics
                                         (cbfmics + 1)
                 updateSTFMIndexCounterB bfmiis
                                         (cbfmiis + 1)
                 iiFMIndexB as
                            bs
                            bfmiss
                            bfmiis
                            bfmics    
           | otherwise
           -> do updateSTFMIndexSeqAB bfmiss
                                      (cbfmiis,cbfmics,b)
                 updateSTFMIndexCounterB bfmiis
                                         (cbfmiis + 1)
                 iiFMIndexB as
                            bs
                            bfmiss
                            bfmiis
                            bfmics

{-----------------------------------}


{-toFMIndex (Text) functions.-}

-- | Abstract 'PTFMIndexSeqT' type utilizing a 'Seq'.
type PTFMIndexSeqT = Seq (Maybe Text)

-- | Abstract 'FMIndexSeqT' type utilizing a 'Seq'.
-- (c,(indexofinputcurrentelement,Occ(c,k),inputcurrentelement))
-- Please see [this](https://en.wikipedia.org/wiki/FM-index)
-- for an explanation of the above abbreviations.
type FMIndexSeqT = Seq (Maybe Text,Seq (Int,Int,Maybe Text))

-- | Abstract data type representing a 'FMIndexSeqT' in the (strict) ST monad.
type STFMIndexSeqT s a = STRef s FMIndexSeqT

-- | State function to update 'FMIndexSeqT'
-- with each step of the FMIndex.
updateSTFMIndexSeqAT :: STFMIndexSeqT s (Seq (Maybe Text,Seq (Int,Int,Maybe Text)))
                     -> (Int,Int,Maybe Text)
                     -> ST s ()
updateSTFMIndexSeqAT s e = do
  s2 <- readSTRef s
  case viewr s2 of
    EmptyR           -> pure ()
    (s2h DS.:> s2fm) -> writeSTRef s (s2h DS.|> (((\(a,_) -> a) s2fm),((\(_,b) -> b) s2fm) DS.|> e))

-- | State function to update 'FMIndexSeqT'
-- with each step of the FMIndex.
updateSTFMIndexSeqBT :: STFMIndexSeqT s (Seq (Maybe Text,Seq (Int,Int,Maybe Text)))
                     -> Maybe Text
                     -> ST s ()
updateSTFMIndexSeqBT s e = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> (e,DS.empty))

-- | State function to create empty 'STFMIndexSeqT' type.
emptySTFMIndexSeqT :: ST s (STFMIndexSeqT s a)
emptySTFMIndexSeqT = newSTRef DS.empty

-- | Abstract 'STFMIndexILT' and associated state type.
type STFMIndexILT s a = STRef s (Seq (Maybe Text))

-- | State function to load list into 'STFMIndexILT'.
loadSTFMIndexILT :: STMTFILT s (Maybe Text)
                 -> Seq (Maybe Text)
                 -> ST s ()
loadSTFMIndexILT s e = writeSTRef s e

-- | State function to create empty 'STFMIndexILT' type.
emptySTFMIndexILT :: ST s (STFMIndexILT s a)
emptySTFMIndexILT = newSTRef DS.empty

-- | Abstract 'STFMIndexCounterT' and associated state type.
type STFMIndexCounterT s a = STRef s Int

-- | State function to update 'STFMIndexCounterT'.
updateSTFMIndexCounterT :: STFMIndexCounterT s Int
                        -> Int
                        -> ST s ()
updateSTFMIndexCounterT s e = writeSTRef s e

-- | State function to create empty 'STFMIndexCounterT' type.
emptySTFMIndexCounterT :: ST s (STFMIndexCounterT s Int)
emptySTFMIndexCounterT = newSTRef 0

-- | Strict state monad function.
seqToFMIndexT :: PTFMIndexSeqT
              -> ST s FMIndexSeqT
seqToFMIndexT DS.Empty      = do
  tfmiseqstackempty  <- emptySTFMIndexSeqT
  tfmiseqstackemptyr <- readSTRef tfmiseqstackempty
  return tfmiseqstackemptyr
seqToFMIndexT xs            = do
  tfmiseqstack     <- emptySTFMIndexSeqT
  tfmiinitiallist  <- emptySTFMIndexILT
  tfmicounterstack <- emptySTFMIndexCounterT
  let il = nubSeq' xs
  loadSTFMIndexILT tfmiinitiallist
                   il
  ctfmiinitiallist <- readSTRef tfmiinitiallist
  iFMIndexT ctfmiinitiallist
            xs
            tfmiseqstack
            tfmicounterstack
  tfmiseqstackr <- readSTRef tfmiseqstack
  return tfmiseqstackr
    where
      iFMIndexT DS.Empty      _      _      _      = pure ()
      iFMIndexT (y DS.:<| ys) zs     tfmiss tfmics = do
        tfmiis <- emptySTFMIndexCounterT
        updateSTFMIndexCounterT tfmiis
                                1
        updateSTFMIndexSeqBT tfmiss
                             y
        iiFMIndexT y
                   zs
                   tfmiss
                   tfmiis
                   tfmics
        iFMIndexT ys
                  zs
                  tfmiss
                  tfmics
      iiFMIndexT _  DS.Empty      _      _      tfmics = do
        updateSTFMIndexCounterT tfmics
                                0
        pure ()
      iiFMIndexT as (b DS.:<| bs) tfmiss tfmiis tfmics = do
        ctfmiis <- readSTRef tfmiis
        ctfmics <- readSTRef tfmics
        if | as == b
           -> do updateSTFMIndexSeqAT tfmiss
                                      (ctfmiis,ctfmics + 1,b)
                 updateSTFMIndexCounterT tfmics
                                         (ctfmics + 1)
                 updateSTFMIndexCounterT tfmiis
                                         (ctfmiis + 1)
                 iiFMIndexT as
                            bs
                            tfmiss
                            tfmiis
                            tfmics
           | otherwise
           -> do updateSTFMIndexSeqAT tfmiss
                                      (ctfmiis,ctfmics,b)
                 updateSTFMIndexCounterT tfmiis
                                         (ctfmiis + 1)
                 iiFMIndexT as
                            bs
                            tfmiss
                            tfmiis
                            tfmics

{-----------------------------}


{-fromFMIndex (ByteString) functions.-}

-- | Abstract 'FFMIndexSeqB' type utilizing a 'Seq'.
type FFMIndexSeqB = Seq (Maybe ByteString)

-- | Simple Inverse FMIndex function. 
seqFromFMIndexB :: FMIndexB
                -> FFMIndexSeqB
seqFromFMIndexB (FMIndexB DS.Empty) = DS.Empty
seqFromFMIndexB xs                  = do
  let xss = (\(FMIndexB b) -> b) xs
  iFFMIndexB xss
    where
      iFFMIndexB DS.Empty         = DS.Empty 
      iFFMIndexB ((_,b) DS.:<| _) =
        fmap (\(_,_,e) -> e) b

{-------------------------------------}


{-fromFMIndex (Text) functions.-}

-- | Abstract 'FFMIndexSeqT' type utilizing a 'Seq'.
type FFMIndexSeqT = Seq (Maybe Text)

-- | Simple Inverse FMIndex function.
seqFromFMIndexT :: FMIndexT
                -> FFMIndexSeqT
seqFromFMIndexT (FMIndexT DS.Empty) = DS.Empty
seqFromFMIndexT xs                  = do
  let xss = (\(FMIndexT t) -> t) xs
  iFFMIndexT xss
    where
      iFFMIndexT DS.Empty         = DS.Empty
      iFFMIndexT ((_,b) DS.:<| _) =
        fmap (\(_,_,e) -> e) b

{-------------------------------}
