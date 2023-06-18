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
                           -- * From MTF functions
                           seqFromMTF,
                         ) where

import Control.Monad as CM
import Control.Monad.ST as CMST
import Data.Foldable as DFold (foldr')
import Data.RLE.Internal (Pack)
import Data.Set as DSet
import Data.Sequence as DS (Seq(..),deleteAt,findIndexL,empty,index,unstableSort,(|>),(<|))
import Data.STRef as DSTR
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

-- | State function to update 'FSTMTFSeqB' with each step of the inverse MTF.
updateFSTMTFSeq :: Pack b => STRef s (Seq (Maybe b)) -> (Maybe b) -> ST s ()
updateFSTMTFSeq s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Nothing)
updateFSTMTFSeq s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> (Just e))

-- | State function to update 'FSTMTFILB'.
updateFSTMTFIL :: Pack b => STRef s (Seq (Maybe b)) -> Int -> ST s ()
updateFSTMTFIL s i = do
  s2 <- readSTRef s
  let newheade = DS.index s2 i
  writeSTRef s (DS.deleteAt i s2)
  ns2 <- readSTRef s
  writeSTRef s (newheade DS.<| ns2)

-- | Strict state monad function.
seqFromMTF :: (Pack b, Ord b) => MTF b -> Seq (Maybe b)
seqFromMTF (MTF (DS.Empty,_)) = CMST.runST $ do
  fbmtfseqstackempty  <- newSTRef DS.empty
  fbmtfseqstackemptyr <- readSTRef fbmtfseqstackempty
  return fbmtfseqstackemptyr
seqFromMTF (MTF (_,DS.Empty)) = CMST.runST $ do
  fbmtfseqstackempty  <- newSTRef DS.empty
  fbmtfseqstackemptyr <- readSTRef fbmtfseqstackempty
  return fbmtfseqstackemptyr
seqFromMTF xs                  = CMST.runST $ do
  let xss = (\(MTF b) -> b) xs
  fbmtfseqstack     <- newSTRef DS.empty
  fbmtfinitiallist  <- newSTRef DS.empty
  let il = nubSeq' (snd xss)
  writeSTRef fbmtfinitiallist
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
        updateFSTMTFSeq fbmtfss
                         (DS.index cfbmtfil y)
        updateFSTMTFIL fbmtfil
                        y
        iFMTFB ys
               fbmtfil
               fbmtfss
