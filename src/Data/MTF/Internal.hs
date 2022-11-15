{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}


-- |
-- Module      :  Data.MTF.Internal
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
-- Various data structures and custom data types to describe the Move-to-front transform (MTF)
-- and the Inverse MTF implementations, namely 'seqToMTFB', 'seqToMTFT', 'seqFromMTFB', and 'seqFromMTFT'.
--
-- The MTF implementations rely heavily upon 'Seq' provided by the [containers](https://hackage.haskell.org/package/containers),
-- 'STRef' and associated functions in the [stref](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-STRef.html) library,
-- and 'runST' in the [Control.Monad.ST](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-ST.html) library.


module Data.MTF.Internal where

import Control.Monad as CM
import Control.Monad.ST as CMST
import Control.Monad.State.Strict()
import Data.ByteString as BS
import Data.ByteString.Char8()
import Data.ByteString.Internal()
import Data.Foldable as DFold (foldr')
import Data.List()
import Data.Maybe()
import Data.Set as DSet
import Data.Sequence as DS (Seq(..),deleteAt,findIndexL,empty,index,unstableSort,(|>),(<|))
import Data.Sequence.Internal as DSI
import Data.STRef as DSTR
import Data.Text as DText
import GHC.Generics (Generic)
import Prelude as P


{-Base level types.-}

-- | Basic MTF ('ByteString') data type.
newtype MTFB = MTFB ((Seq Int,Seq (Maybe ByteString)))
  deriving (Eq,Ord,Show,Read,Generic)

-- | Basic MTF ('Text') data type.
newtype MTFT = MTFT ((Seq Int,Seq (Maybe Text)))
  deriving (Eq,Ord,Show,Read,Generic)

{-------------------}


{-Auxilary function(s).-}

-- | Useful to acquire the unique elements
-- that make up a 'Seq'.
-- Credit to @DavidFletcher.
-- See [this stackoverflow post](https://stackoverflow.com/questions/45757839/removing-duplicate-elements-in-a-seq).
nubSeq' :: Ord a
        => Seq (Maybe a)
        -> Seq (Maybe a)
nubSeq' xs =
  unstableSort $
  DFold.foldr' cons'
               nil
               xs
               DSet.empty
    where
      cons' :: Ord a
           => Maybe a
           -> (Set (Maybe a) -> Seq (Maybe a))
           -> (Set (Maybe a) -> Seq (Maybe a))
      cons' y ys seen = if | y `P.elem` seen
                           -> ys seen
                           | otherwise
                           -> y DS.<| ys (DSet.insert y seen)
      nil :: Set (Maybe a)
          -> Seq (Maybe a)
      nil _ = DS.empty

{-----------------------}


{-toMTF (ByteString) functions.-}

-- | Abstract 'PBMTFSeqB' type utilizing a 'Seq'
type PBMTFSeqB = Seq (Maybe ByteString)

-- | Abstract 'MTFLSSeqB' type utilizing a 'Seq'.
type MTFLSSeqB = (Seq Int,Seq (Maybe ByteString))

-- | Abstract data type representing a 'MTFLSSeqB' in the (strict) ST monad.
type STMTFLSSeqB s a = STRef s MTFLSSeqB

-- | Abstract data type to initialize a 'STMTFLSSeqB'
-- using the initial list.
initializeSTMTFLSSeqB :: STMTFLSSeqB s (Seq Int,Seq (Maybe ByteString))
                     -> Seq (Maybe ByteString)
                     -> ST s ()
initializeSTMTFLSSeqB s DS.Empty = do
  (s2i,_) <- readSTRef s
  writeSTRef s (s2i,DS.empty)
initializeSTMTFLSSeqB s e        = do
  (s2i,_) <- readSTRef s
  writeSTRef s (s2i,e)

-- | State function to update 'MTFLSSeqB'
-- with each step of the MTF.
updateSTMTFLSSeqB :: STMTFLSSeqB s (Seq Int,Seq (Maybe ByteString))
                  -> Int
                  -> ST s () 
updateSTMTFLSSeqB s i = do
  (s2i,s2b) <- readSTRef s
  let newheade = DS.index s2b i
  writeSTRef s (s2i,DS.deleteAt i s2b)
  (ns2i,ns2b) <- readSTRef s
  writeSTRef s (ns2i DS.|> i,newheade DS.<| ns2b)

-- | State function to create empty 'STMTFLSSeqB' type.
emptySTMTFLSSeqB :: ST s (STMTFLSSeqB s a)
emptySTMTFLSSeqB = newSTRef (DS.empty,DS.empty) 

-- | Abstract 'STMTFILB' and associated state type.
type STMTFILB s a = STRef s (Seq (Maybe ByteString))

-- | State function to load list into 'STMTFILB'.
loadSTMTFILB :: STMTFILB s (Maybe ByteString)
             -> Seq (Maybe ByteString)
             -> ST s ()
loadSTMTFILB s e = writeSTRef s e

-- | State function to create empty 'STMTFILB' type.
emptySTMTFILB :: ST s (STMTFILB s a)
emptySTMTFILB = newSTRef DS.empty

-- | Abstract 'STMTFCounterB' and associated state type.
type STMTFCounterB s a = STRef s Int

-- | State function to update 'STMTFCounterB'.
updateSTMTFCounterB :: STMTFCounterB s Int
                    -> Int
                    -> ST s ()
updateSTMTFCounterB s e = writeSTRef s e

-- | State function to create empty 'STMTFCounterB' type.
emptySTMTFCounterB :: ST s (STMTFCounterB s Int)
emptySTMTFCounterB = newSTRef (-1)

-- | Strict state monad function.
seqToMTFB :: PBMTFSeqB
          -> ST s MTFLSSeqB
seqToMTFB DS.Empty      = do
  bmtfseqstackempty  <- emptySTMTFLSSeqB
  bmtfseqstackemptyr <- readSTRef bmtfseqstackempty
  return bmtfseqstackemptyr
seqToMTFB xs            = do
  bmtfseqstack     <- emptySTMTFLSSeqB
  bmtfinitiallist  <- emptySTMTFILB
  bmtfcounterstack <- emptySTMTFCounterB
  let il = nubSeq' xs
  loadSTMTFILB bmtfinitiallist
               il 
  iMTFB xs
        bmtfinitiallist
        bmtfseqstack
        bmtfcounterstack 
  bmtfseqstackr <- readSTRef bmtfseqstack
  return bmtfseqstackr
    where
      iMTFB DS.Empty      _      _      _      = pure ()
      iMTFB (y DS.:<| ys) bmtfil bmtfss bmtfcs = do
        cbmtfcs <- readSTRef bmtfcs
        if | cbmtfcs == (-1)
           -> do updateSTMTFCounterB bmtfcs
                                     1
                 cbmtfil <- readSTRef bmtfil
                 initializeSTMTFLSSeqB bmtfss
                                       cbmtfil
                 (_,cbmtfss) <- readSTRef bmtfss
                 case (DS.findIndexL (\z -> z == y) cbmtfss) of
                   Nothing     -> iMTFB ys
                                        bmtfil
                                        bmtfss
                                        bmtfcs
                   Just bindex -> do updateSTMTFLSSeqB bmtfss
                                                       bindex
                                     iMTFB ys
                                           bmtfil
                                           bmtfss
                                           bmtfcs
           | otherwise
           -> do (_,cbmtfss) <- readSTRef bmtfss
                 case (DS.findIndexL (\z -> z == y) cbmtfss) of
                   Nothing     -> iMTFB ys
                                        bmtfil
                                        bmtfss
                                        bmtfcs
                   Just bindex -> do updateSTMTFLSSeqB bmtfss
                                                       bindex
                                     iMTFB ys
                                           bmtfil
                                           bmtfss
                                           bmtfcs

{-------------------------------}


{-toMTF (Text) functions.-}

-- | Abstract 'PTMTFSeqT' type utilizing a 'Seq'
type PTMTFSeqT = Seq (Maybe Text)

-- | Abstract 'MTFLSSeqT' type utilizing a 'Seq'.
type MTFLSSeqT = (Seq Int,Seq (Maybe Text))

-- | Abstract data type representing a 'MTFLSSeqT' in the (strict) ST monad.
type STMTFLSSeqT s a = STRef s MTFLSSeqT

-- | Abstract data type to initialize a 'STMTFLSSeqT'
-- using the initial list.
initializeSTMTFLSSeqT :: STMTFLSSeqT s (Seq Int,Seq (Maybe Text))
                      -> Seq (Maybe Text)
                      -> ST s ()
initializeSTMTFLSSeqT s DS.Empty  = do
  (s2i,_) <- readSTRef s
  writeSTRef s (s2i,DS.empty)
initializeSTMTFLSSeqT s e         = do
  (s2i,_) <- readSTRef s
  writeSTRef s (s2i,e)

-- | State function to update 'STMTFLSSeqT'
-- with each step of the MTF.
updateSTMTFLSSeqT :: STMTFLSSeqT s (Seq Int,Seq (Maybe Text))
                  -> Int
                  -> ST s ()
updateSTMTFLSSeqT s i = do
  (s2i,s2b) <- readSTRef s
  let newheade = DS.index s2b i
  writeSTRef s (s2i,DS.deleteAt i s2b)
  (ns2i,ns2b) <- readSTRef s
  writeSTRef s (ns2i DS.|> i,newheade DS.<| ns2b)

-- | State function to create empty 'STMTFLSSeqT' type.
emptySTMTFLSSeqT :: ST s (STMTFLSSeqT s a)
emptySTMTFLSSeqT = newSTRef (DS.empty,DS.empty)

-- | Abstract 'STMTFILT' and associated state type.
type STMTFILT s a = STRef s (Seq (Maybe Text))

-- | State function to load list into 'STMTFILT'.
loadSTMTFILT :: STMTFILT s (Maybe Text)
             -> Seq (Maybe Text)
             -> ST s ()
loadSTMTFILT s e = writeSTRef s e

-- | State function to create empty 'STMTFILT' type.
emptySTMTFILT :: ST s (STMTFILT s a)
emptySTMTFILT = newSTRef DS.empty

-- | Abstract 'STMTFCounterT' and associated state type.
type STMTFCounterT s a = STRef s Int

-- | State function to update 'STMTFCounterT'.
updateSTMTFCounterT :: STMTFCounterT s Int
                    -> Int
                    -> ST s ()
updateSTMTFCounterT s e = writeSTRef s e

-- | State function to create empty 'STMTFCounterT' type.
emptySTMTFCounterT :: ST s (STMTFCounterT s Int)
emptySTMTFCounterT = newSTRef (-1)

-- | Strict state monad function.
seqToMTFT :: PTMTFSeqT
          -> ST s MTFLSSeqT
seqToMTFT DS.Empty      = do
  tmtfseqstackempty  <- emptySTMTFLSSeqT
  tmtfseqstackemptyr <- readSTRef tmtfseqstackempty
  return tmtfseqstackemptyr
seqToMTFT xs            = do
  tmtfseqstack     <- emptySTMTFLSSeqT
  tmtfinitiallist  <- emptySTMTFILT
  tmtfcounterstack <- emptySTMTFCounterT
  let il = nubSeq' xs
  loadSTMTFILT tmtfinitiallist
               il
  iMTFT xs
        tmtfinitiallist
        tmtfseqstack
        tmtfcounterstack
  tmtfseqstackr <- readSTRef tmtfseqstack
  return tmtfseqstackr
    where
      iMTFT DS.Empty      _      _      _      = pure ()
      iMTFT (y DS.:<| ys) tmtfil tmtfss tmtfcs = do
        ctmtfcs <- readSTRef tmtfcs
        if | ctmtfcs == (-1)
           -> do updateSTMTFCounterT tmtfcs
                                     1
                 ctmtfil <- readSTRef tmtfil
                 initializeSTMTFLSSeqT tmtfss
                                       ctmtfil
                 (_,ctmtfss) <- readSTRef tmtfss
                 case (DS.findIndexL (\z -> z == y) ctmtfss) of
                   Nothing     -> iMTFT ys
                                        tmtfil
                                        tmtfss
                                        tmtfcs
                   Just tindex -> do updateSTMTFLSSeqT tmtfss
                                                       tindex
                                     iMTFT ys
                                           tmtfil
                                           tmtfss
                                           tmtfcs
           | otherwise
           -> do (_,ctmtfss) <- readSTRef tmtfss
                 case (DS.findIndexL (\z -> z == y) ctmtfss) of
                   Nothing     -> iMTFT ys
                                        tmtfil
                                        tmtfss
                                        tmtfcs
                   Just tindex -> do updateSTMTFLSSeqT tmtfss
                                                       tindex
                                     iMTFT ys
                                           tmtfil
                                           tmtfss
                                           tmtfcs

{-------------------------}


{-fromMTF (ByteString) functions.-}

-- | Abstract 'FMTFSeqB' type utilizing a 'Seq'.
type FMTFSeqB = Seq (Maybe ByteString)

-- | Abstract data type representing a 'FMTFSeqB' in the (strict) ST monad.
type FSTMTFSeqB s a = STRef s FMTFSeqB

-- | State function to update 'FSTMTFSeqB' with each step of the inverse MTF.
updateFSTMTFSeqB :: FSTMTFSeqB s (Maybe ByteString)
                 -> (Maybe ByteString)
                 -> ST s ()
updateFSTMTFSeqB s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Nothing)
updateFSTMTFSeqB s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> (Just e))

-- | State function to create empty 'FSTMTFSeqB' type.
emptyFSTMTFSeqB :: ST s (FSTMTFSeqB s a)
emptyFSTMTFSeqB = newSTRef DS.empty

-- | Abstract 'FSTMTFILB' and associated state type.
type FSTMTFILB s a = STRef s (Seq (Maybe ByteString))

-- | State function to load list into 'FSTMTFILB'.
loadFSTMTFILB :: FSTMTFILB s (Maybe ByteString) -> Seq (Maybe ByteString) -> ST s ()
loadFSTMTFILB s e = writeSTRef s e

-- | State function to update 'FSTMTFILB'.
updateFSTMTFILB :: FSTMTFILB s (Maybe ByteString)
                -> Int
                -> ST s ()
updateFSTMTFILB s i = do
  s2 <- readSTRef s
  let newheade = DS.index s2 i
  writeSTRef s (DS.deleteAt i s2)
  ns2 <- readSTRef s 
  writeSTRef s (newheade DS.<| ns2)

-- | State function to create empty 'FSTMTFILB' type.
emptyFSTMTFILB :: ST s (FSTMTFILB s a)
emptyFSTMTFILB = newSTRef DS.empty

-- | Strict state monad function.
seqFromMTFB :: MTFB
            -> ST s FMTFSeqB
seqFromMTFB (MTFB (DS.Empty,_)) = do
  fbmtfseqstackempty  <- emptyFSTMTFSeqB
  fbmtfseqstackemptyr <- readSTRef fbmtfseqstackempty
  return fbmtfseqstackemptyr
seqFromMTFB (MTFB (_,DS.Empty)) = do
  fbmtfseqstackempty  <- emptyFSTMTFSeqB
  fbmtfseqstackemptyr <- readSTRef fbmtfseqstackempty
  return fbmtfseqstackemptyr
seqFromMTFB xs                  = do
  let xss = (\(MTFB b) -> b) xs
  fbmtfseqstack     <- emptyFSTMTFSeqB 
  fbmtfinitiallist  <- emptyFSTMTFILB
  let il = nubSeq' (snd xss)
  loadFSTMTFILB fbmtfinitiallist
                il
  iFMTFB (fst xss)
         fbmtfinitiallist
         fbmtfseqstack
  fbmtfseqstackr <- readSTRef fbmtfseqstack
  return fbmtfseqstackr
    where
      iFMTFB DS.Empty      _       _       = pure ()
      iFMTFB (y DS.:<| ys) fbmtfil fbmtfss = do
        cfbmtfil <- readSTRef fbmtfil
        updateFSTMTFSeqB fbmtfss
                         (DS.index cfbmtfil y)
        updateFSTMTFILB fbmtfil
                        y 
        iFMTFB ys
               fbmtfil
               fbmtfss

{---------------------------------}


{-fromRLE (Text) functions.-}

-- | Abstract 'FMTFSeqT' type utilizing a 'Seq'.
type FMTFSeqT = Seq (Maybe Text)

-- | Abstract data type representing a 'FMTFSeqT' in the (strict) ST monad.
type FSTMTFSeqT s a = STRef s FMTFSeqT

-- | State function to update 'FSTMTFSeqT' with each step of the inverse MTF.
updateFSTMTFSeqT :: FSTMTFSeqT s (Maybe Text)
                 -> (Maybe Text)
                 -> ST s ()
updateFSTMTFSeqT s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Nothing)
updateFSTMTFSeqT s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> (Just e))

-- | State function to create empty 'FSTMTFSeqT' type.
emptyFSTMTFSeqT :: ST s (FSTMTFSeqT s a)
emptyFSTMTFSeqT = newSTRef DS.empty

-- | Abstract 'FSTMTFILT' and associated state type.
type FSTMTFILT s a = STRef s (Seq (Maybe Text))

-- | State function to load list into 'FSTMTFILT'.
loadFSTMTFILT :: FSTMTFILT s (Maybe Text)
              -> Seq (Maybe Text)
              -> ST s ()
loadFSTMTFILT s e = writeSTRef s e

-- | State function to update 'FSTMTFILT'.
updateFSTMTFILT :: FSTMTFILT s (Maybe Text)
                -> Int
                -> ST s ()
updateFSTMTFILT s i = do
  s2 <- readSTRef s
  let newheade = DS.index s2 i
  writeSTRef s (DS.deleteAt i s2)
  ns2 <- readSTRef s
  writeSTRef s (newheade DS.<| ns2)

-- | State function to create empty 'FSTMTFILT' type.
emptyFSTMTFILT :: ST s (FSTMTFILT s a)
emptyFSTMTFILT = newSTRef DS.empty

-- | Strict state monad function.
seqFromMTFT :: MTFT
            -> ST s FMTFSeqT
seqFromMTFT (MTFT (DS.Empty,_)) = do
  ftmtfseqstackempty  <- emptyFSTMTFSeqT
  ftmtfseqstackemptyr <- readSTRef ftmtfseqstackempty
  return ftmtfseqstackemptyr
seqFromMTFT (MTFT (_,DS.Empty)) = do
  ftmtfseqstackempty  <- emptyFSTMTFSeqT
  ftmtfseqstackemptyr <- readSTRef ftmtfseqstackempty
  return ftmtfseqstackemptyr
seqFromMTFT xs                  = do
  let xss = (\(MTFT t) -> t) xs
  ftmtfseqstack     <- emptyFSTMTFSeqT
  ftmtfinitiallist  <- emptyFSTMTFILT
  let il = nubSeq' (snd xss)
  loadFSTMTFILT ftmtfinitiallist
                il
  iFMTFT (fst xss)
         ftmtfinitiallist
         ftmtfseqstack
  ftmtfseqstackr <- readSTRef ftmtfseqstack
  return ftmtfseqstackr
    where
      iFMTFT DS.Empty      _       _       = pure ()
      iFMTFT (y DS.:<| ys) ftmtfil ftmtfss = do
        cftmtfil <- readSTRef ftmtfil
        updateFSTMTFSeqT ftmtfss
                         (DS.index cftmtfil y)
        updateFSTMTFILT ftmtfil
                        y
        iFMTFT ys
               ftmtfil
               ftmtfss

{---------------------------}
