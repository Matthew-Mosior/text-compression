{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies     #-}


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
-- and the Inverse MTF implementations, namely 'seqToMTF', 'seqFromMTFB', and 'seqFromMTFT'.
--
-- The MTF implementations rely heavily upon 'Seq' provided by the [containers](https://hackage.haskell.org/package/containers),
-- 'STRef' and associated functions in the [stref](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-STRef.html) library,
-- and 'runST' in the [Control.Monad.ST](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-ST.html) library.


module Data.MTF.Internal ( -- * Base MTF types
                           MTF(..),
                           -- * Auxiliary functions
                           nubSeq',
                           -- * To MTF functions
                           seqToMTF,
                           -- * From MTF (ByteString) functions
                           seqFromMTFB,
                           -- * From MTF (Text) functions
                           seqFromMTFT
                         ) where

import Control.Monad as CM
import Control.Monad.ST as CMST
import Data.ByteString as BS
import Data.Foldable as DFold (foldr')
import Data.RLE.Internal (Pack)
import Data.Set as DSet
import Data.Sequence as DS (Seq(..),deleteAt,findIndexL,empty,index,unstableSort,(|>),(<|))
import Data.STRef as DSTR
import Data.Text as DText
import GHC.Generics (Generic)
import Prelude as P


{-Base level types.-}

-- | Basic MTF ('ByteString') data type.
newtype MTF b = MTF ((Seq Int,Seq (Maybe b)))
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

-- | Abstract data type to initialize a 'STMTFLSSeqB'
-- using the initial list.
initializeSTMTFLSSeq :: STRef s (Seq Int,Seq (Maybe b))
                     -> Seq (Maybe b)
                     -> ST s ()
initializeSTMTFLSSeq s e        = do
  (s2i,_) <- readSTRef s
  writeSTRef s (s2i,e)

-- | State function to update 'MTFLSSeqB'
-- with each step of the MTF.
updateSTMTFLSSeq :: STRef s (Seq Int,Seq (Maybe b))
                  -> Int
                  -> ST s ()
updateSTMTFLSSeq s i = do
  (s2i,s2b) <- readSTRef s
  let newheade = DS.index s2b i
  writeSTRef s (s2i,DS.deleteAt i s2b)
  (ns2i,ns2b) <- readSTRef s
  writeSTRef s (ns2i DS.|> i,newheade DS.<| ns2b)

-- | Strict state monad function.
seqToMTF :: forall b. (Pack b, Ord b) => Seq (Maybe b) -> (Seq Int,Seq (Maybe b))
seqToMTF DS.Empty      = CMST.runST $ do
  bmtfseqstackempty  <- newSTRef (DS.empty,DS.empty)
  bmtfseqstackemptyr <- readSTRef bmtfseqstackempty
  return bmtfseqstackemptyr
seqToMTF xs            = CMST.runST $ do
  bmtfseqstack     <- newSTRef (DS.empty,DS.empty)
  bmtfinitiallist  <- newSTRef DS.empty
  bmtfcounterstack <- newSTRef (-1 :: Int)
  let il = nubSeq' xs
  writeSTRef bmtfinitiallist il
  iMTFB xs bmtfinitiallist bmtfseqstack bmtfcounterstack
  bmtfseqstackr <- readSTRef bmtfseqstack
  return bmtfseqstackr
    where
      iMTFB DS.Empty      _      _      _      = pure ()
      iMTFB (y DS.:<| ys) bmtfil bmtfss bmtfcs = do
        cbmtfcs <- readSTRef bmtfcs
        if | cbmtfcs == (-1)
           -> do writeSTRef bmtfcs
                                     1
                 cbmtfil <- readSTRef bmtfil
                 initializeSTMTFLSSeq bmtfss cbmtfil
                 (_,cbmtfss) <- readSTRef bmtfss
                 case (DS.findIndexL (\z -> z == y) cbmtfss) of
                   Nothing     -> iMTFB ys
                                        bmtfil
                                        bmtfss
                                        bmtfcs
                   Just bindex -> do updateSTMTFLSSeq bmtfss
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
                   Just bindex -> do updateSTMTFLSSeq bmtfss
                                                       bindex
                                     iMTFB ys
                                           bmtfil
                                           bmtfss
                                           bmtfcs

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

-- | State function to load list into 'FSTMTFILB'.
loadFSTMTFILB :: STRef s (Seq (Maybe ByteString)) -> Seq (Maybe ByteString) -> ST s ()
loadFSTMTFILB s e = writeSTRef s e

-- | State function to update 'FSTMTFILB'.
updateFSTMTFILB :: STRef s (Seq (Maybe ByteString))
                -> Int
                -> ST s ()
updateFSTMTFILB s i = do
  s2 <- readSTRef s
  let newheade = DS.index s2 i
  writeSTRef s (DS.deleteAt i s2)
  ns2 <- readSTRef s
  writeSTRef s (newheade DS.<| ns2)

-- | State function to create empty 'FSTMTFILB' type.
emptyFSTMTFILB :: ST s (STRef s (Seq (Maybe ByteString)))
emptyFSTMTFILB = newSTRef DS.empty

-- | Strict state monad function.
seqFromMTFB :: MTF ByteString
            -> FMTFSeqB
seqFromMTFB (MTF (DS.Empty,_)) = CMST.runST $ do
  fbmtfseqstackempty  <- emptyFSTMTFSeqB
  fbmtfseqstackemptyr <- readSTRef fbmtfseqstackempty
  return fbmtfseqstackemptyr
seqFromMTFB (MTF (_,DS.Empty)) = CMST.runST $ do
  fbmtfseqstackempty  <- emptyFSTMTFSeqB
  fbmtfseqstackemptyr <- readSTRef fbmtfseqstackempty
  return fbmtfseqstackemptyr
seqFromMTFB xs                  = CMST.runST $ do
  let xss = (\(MTF b) -> b) xs
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
seqFromMTFT :: MTF Text
            -> FMTFSeqT
seqFromMTFT (MTF (DS.Empty,_)) = CMST.runST $ do
  ftmtfseqstackempty  <- emptyFSTMTFSeqT
  ftmtfseqstackemptyr <- readSTRef ftmtfseqstackempty
  return ftmtfseqstackemptyr
seqFromMTFT (MTF (_,DS.Empty)) = CMST.runST $ do
  ftmtfseqstackempty  <- emptyFSTMTFSeqT
  ftmtfseqstackemptyr <- readSTRef ftmtfseqstackempty
  return ftmtfseqstackemptyr
seqFromMTFT xs                  = CMST.runST $ do
  let xss = (\(MTF t) -> t) xs
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
