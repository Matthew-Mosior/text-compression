{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}


-- |
-- Module      :  Data.RLE.Internal
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
-- Various data structures and custom data types to describe the Run-length encoding (RLE)
-- and the Inverse RLE implementations, namely 'seqToRLEB', 'seqToRLET', 'seqFromRLEB', and 'seqFromRLET'.
--
-- The RLE implementations rely heavily upon 'Seq' provided by the [containers](https://hackage.haskell.org/package/containers),
-- 'STRef' and associated functions in the [stref](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-STRef.html) library,
-- and 'runST' in the [Control.Monad.ST](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-ST.html) library.


module Data.RLE.Internal where

import Control.Monad as CM
import Control.Monad.ST as CMST
import Control.Monad.State.Strict()
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC8 (pack,unpack)
import Data.ByteString.Internal()
import Data.List()
import Data.Maybe as DMaybe (fromJust,isJust,isNothing)
import Data.Sequence as DS
import Data.Sequence.Internal as DSI
import Data.STRef as DSTR
import Data.Text as DText
import GHC.Generics (Generic)
import Prelude as P


{-Base level types.-}

-- | Basic RLE ('ByteString') data type.
newtype RLEB = RLEB (Seq (Maybe ByteString))
  deriving (Eq,Ord,Show,Read,Generic)

-- | Basic RLE ('Text') data type.
newtype RLET = RLET (Seq (Maybe Text))
  deriving (Eq,Ord,Show,Read,Generic)

{-------------------}


{-toRLE (ByteString) functions.-}

-- | Abstract 'RLESeqB' type utilizing a sequence.
type RLESeqB = Seq (Maybe ByteString)

-- | Abstract data type representing a 'RLESeqB' in the (strict) ST monad.
type STRLESeqB s a = STRef s RLESeqB

-- | State function to push 'RLESeqB' data into stack.
pushSTRLESeqB :: STRLESeqB s (Maybe ByteString) -> Maybe ByteString -> ST s ()
pushSTRLESeqB s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Nothing)
pushSTRLESeqB s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Just e)

-- | State function to create empty 'STRLESeqB' type.
emptySTRLESeqB :: ST s (STRLESeqB s a)
emptySTRLESeqB = newSTRef DS.empty

-- | Abstract 'STRLETempB' and associated state type.
type STRLETempB s a = STRef s (Maybe ByteString)

-- | State function to update 'STRLETempB'.
updateSTRLETempB :: STRLETempB s (Maybe ByteString) -> Maybe ByteString -> ST s ()
updateSTRLETempB s Nothing  = writeSTRef s Nothing
updateSTRLETempB s (Just e) = writeSTRef s (Just e)

-- | State function to create empty 'STRLETempB' type.
emptySTRLETempB :: ST s (STRLETempB s a)
emptySTRLETempB = newSTRef (Just BS.empty)

-- | Abstract 'STRLECounterB' state type.
type STRLECounterB s a = STRef s Int

-- | State function to update 'STRLECounterB'.
updateSTRLECounterB :: STRLECounterB s Int -> Int -> ST s ()
updateSTRLECounterB s e = writeSTRef s e

-- | State function to create empty 'STRLECounterB' type.
emptySTRLECounterB :: ST s (STRLECounterB s Int)
emptySTRLECounterB = newSTRef (-1)

-- | Strict state monad function.
seqToRLEB :: RLESeqB
          -> ST s RLESeqB
seqToRLEB DS.Empty      = do
  brleseqstackempty  <- emptySTRLESeqB
  brleseqstackemptyr <- readSTRef brleseqstackempty
  return brleseqstackemptyr
seqToRLEB (x DS.:<| xs) = do
  brleseqstack     <- emptySTRLESeqB
  brlecounterstack <- emptySTRLECounterB
  brletempstack    <- emptySTRLETempB
  updateSTRLECounterB brlecounterstack
                      1 
  updateSTRLETempB brletempstack
                   x
  iRLEB xs
        brleseqstack
        brlecounterstack
        brletempstack
  brleseqstackr <- readSTRef brleseqstack
  return brleseqstackr
    where
      iRLEB DS.Empty      brless brlecs brlets = do
        cbrlecs <- readSTRef brlecs
        cbrlets <- readSTRef brlets
        pushSTRLESeqB brless
                      (Just      $
                       BSC8.pack $
                       show cbrlecs)
        pushSTRLESeqB brless
                      cbrlets
        pure ()
      iRLEB (y DS.:<| ys) brless brlecs brlets = do
        cbrlecs <- readSTRef brlecs
        cbrlets <- readSTRef brlets
        if | isNothing y
           -> do pushSTRLESeqB brless
                               (Just      $
                                BSC8.pack $
                                show cbrlecs)
                 pushSTRLESeqB brless
                               cbrlets 
                 pushSTRLESeqB brless
                               (Just      $
                                BSC8.pack $
                                show (1 :: Int))
                 pushSTRLESeqB brless
                               Nothing
                 updateSTRLETempB brlets
                                  Nothing             
                 iRLEB ys
                       brless
                       brlecs
                       brlets
           | isNothing cbrlets
           -> do updateSTRLECounterB brlecs
                                     1
                 updateSTRLETempB brlets
                                  y
                 iRLEB ys
                       brless
                       brlecs
                       brlets
           | fromJust cbrlets == fromJust y
           -> do updateSTRLECounterB brlecs
                                     (cbrlecs + 1)
                 iRLEB ys
                       brless
                       brlecs
                       brlets
           | otherwise
           -> do pushSTRLESeqB brless
                               (Just      $
                                BSC8.pack $
                                show cbrlecs)
                 pushSTRLESeqB brless
                               cbrlets
                 updateSTRLECounterB brlecs
                                     1
                 updateSTRLETempB brlets
                                  y
                 iRLEB ys
                       brless
                       brlecs
                       brlets

{-------------------------------}


{-toRLE (Text) functions.-}

-- | Abstract 'RLESeqT' type utilizing a sequence.
type RLESeqT = Seq (Maybe Text)

-- | Abstract data type representing a 'RLESeqT' in the (strict) ST monad.
type STRLESeqT s a = STRef s RLESeqT

-- | State function to push 'RLESeqT' data into stack.
pushSTRLESeqT :: STRLESeqT s (Maybe Text) -> (Maybe Text) -> ST s ()
pushSTRLESeqT s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Nothing)
pushSTRLESeqT s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Just e)

-- | State function to create empty 'STRLESeqT' type.
emptySTRLESeqT :: ST s (STRLESeqT s a)
emptySTRLESeqT = newSTRef DS.empty

-- | Abstract 'STRLETempT' state type.
type STRLETempT s a = STRef s (Maybe Text)

-- | State function to update 'STRLETempT'.
updateSTRLETempT :: STRLETempT s (Maybe Text) -> (Maybe Text) -> ST s ()
updateSTRLETempT s Nothing  = writeSTRef s Nothing
updateSTRLETempT s (Just e) = writeSTRef s (Just e)

-- | State function to create empty 'STRLETempT' type.
emptySTRLETempT :: ST s (STRLETempT s a)
emptySTRLETempT = newSTRef (Just DText.empty)

-- | Abstract 'STRLECounterT' and associated state type.
type STRLECounterT s a = STRef s Int

-- | State function to update 'STRLECounterT'.
updateSTRLECounterT :: STRLECounterT s Int -> Int -> ST s ()
updateSTRLECounterT s e = writeSTRef s e

-- | State function to create empty 'STRLECounterT' type.
emptySTRLECounterT :: ST s (STRLECounterT s Int)
emptySTRLECounterT = newSTRef (-1)

-- | Strict state monad function.
seqToRLET :: RLESeqT ->
             ST s RLESeqT
seqToRLET DS.Empty      = do
  trleseqstackempty  <- emptySTRLESeqT
  trleseqstackemptyr <- readSTRef trleseqstackempty
  return trleseqstackemptyr
seqToRLET (x DS.:<| xs) = do
  trleseqstack     <- emptySTRLESeqT
  trlecounterstack <- emptySTRLECounterT
  trletempstack    <- emptySTRLETempT
  updateSTRLECounterT trlecounterstack
                      1
  updateSTRLETempT trletempstack
                   x
  iRLET xs
        trleseqstack
        trlecounterstack
        trletempstack
  trleseqstackr <- readSTRef trleseqstack
  return trleseqstackr
    where
      iRLET DS.Empty      trless trlecs trlets = do
        ctrlecs <- readSTRef trlecs
        ctrlets <- readSTRef trlets
        pushSTRLESeqT trless
                      (Just       $
                       DText.pack $
                       show ctrlecs)
        pushSTRLESeqT trless
                      ctrlets 
        pure ()
      iRLET (y DS.:<| ys) trless trlecs trlets = do
        ctrlecs <- readSTRef trlecs
        ctrlets <- readSTRef trlets
        if | isNothing y
           -> do pushSTRLESeqT trless
                               (Just       $
                                DText.pack $
                                show ctrlecs)
                 pushSTRLESeqT trless
                               ctrlets
                 pushSTRLESeqT trless
                               (Just       $
                                DText.pack $
                                show (1 :: Int))
                 pushSTRLESeqT trless
                               Nothing
                 updateSTRLETempT trlets
                                  Nothing
                 iRLET ys
                       trless
                       trlecs
                       trlets
           | isNothing ctrlets
           -> do updateSTRLECounterT trlecs
                                     1
                 updateSTRLETempT trlets
                                  y
                 iRLET ys
                       trless
                       trlecs
                       trlets
           | fromJust ctrlets == fromJust y
           -> do updateSTRLECounterT trlecs
                                     (ctrlecs + 1)
                 iRLET ys
                       trless
                       trlecs
                       trlets
           | otherwise
           -> do pushSTRLESeqT trless
                               (Just       $
                                DText.pack $
                                show ctrlecs)
                 pushSTRLESeqT trless
                               ctrlets
                 updateSTRLECounterT trlecs
                                     1
                 updateSTRLETempT trlets
                                  y
                 iRLET ys
                       trless
                       trlecs
                       trlets

{-------------------------}


{-fromRLE (ByteString) functions.-}

-- | Abstract 'FRLESeqB' type utilizing a sequence.
type FRLESeqB = Seq (Maybe ByteString)

-- | Abstract data type representing a 'FRLESeqB' in the (strict) ST monad.
type FSTRLESeqB s a = STRef s FRLESeqB

-- | State function to push 'FRLESeqB' data into stack.
pushFSTRLESeqB :: FSTRLESeqB s (Maybe ByteString) -> (Maybe ByteString) -> ST s ()
pushFSTRLESeqB s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Nothing)
pushFSTRLESeqB s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Just e)

-- | State function to create empty 'FSTRLESeqB' type.
emptyFSTRLESeqB :: ST s (FSTRLESeqB s a)
emptyFSTRLESeqB = newSTRef DS.empty

-- | Strict state monad function.
seqFromRLEB :: RLEB
            -> ST s FRLESeqB
seqFromRLEB (RLEB DS.Empty) = do
  fbrleseqstackempty  <- emptyFSTRLESeqB
  fbrleseqstackemptyr <- readSTRef fbrleseqstackempty
  return fbrleseqstackemptyr
seqFromRLEB xs              = do
  fbrleseqstack <- emptySTRLESeqB
  let rlebseq = (\(RLEB b) -> b) xs
  iFRLEB rlebseq
         fbrleseqstack
  fbrleseqstackr <- readSTRef fbrleseqstack
  return fbrleseqstackr
    where
      iFRLEB (y1 DS.:<| y2 DS.:<| DS.Empty) fbrless =
        if | isJust y1    &&
             isNothing y2
           -> do pushFSTRLESeqB fbrless
                                Nothing
                 pure () 
           | otherwise
           -> do let y1' = read        $
                           BSC8.unpack $
                           fromJust y1 :: Int
                 let y2' = fromJust y2
                 CM.replicateM_ y1'
                                (pushFSTRLESeqB fbrless
                                                (Just y2'))
                 pure () 
      iFRLEB (y1 DS.:<| y2 DS.:<| ys)       fbrless =
        if | isJust y1     &&
             isNothing y2
           -> do pushFSTRLESeqB fbrless
                                Nothing
                 iFRLEB ys
                        fbrless
           | otherwise
           -> do let y1' = read        $
                           BSC8.unpack $
                           fromJust y1 :: Int
                 let y2' = fromJust y2
                 CM.replicateM_ y1'
                                (pushFSTRLESeqB fbrless
                                                (Just y2'))
                 iFRLEB ys
                        fbrless 
      iFRLEB (DSI.Seq EmptyT)               _       = pure ()
      iFRLEB (DSI.Seq (Single _))           _       = pure ()
      iFRLEB (DSI.Seq (Deep _ _ _ _))       _       = pure ()

{---------------------------------}


{-fromRLE (Text) functions.-}

-- | Abstract 'FRLESeqT' type utilizing a sequence.
type FRLESeqT = Seq (Maybe Text)

-- | Abstract data type representing a 'FRLESeqT' in the (strict) ST monad.
type FSTRLESeqT s a = STRef s FRLESeqT

-- | State function to push 'FSTRLESeqT' data into stack.
pushFSTRLESeqT :: FSTRLESeqT s (Maybe Text) -> (Maybe Text) -> ST s ()
pushFSTRLESeqT s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Nothing)
pushFSTRLESeqT s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Just e)

-- | State function to create empty 'FSTRLESeqT' type.
emptyFSTRLESeqT :: ST s (FSTRLESeqT s a)
emptyFSTRLESeqT = newSTRef DS.empty

-- | Strict state monad function.
seqFromRLET :: RLET ->
               ST s FRLESeqT
seqFromRLET (RLET DS.Empty) = do
  ftrleseqstackempty  <- emptyFSTRLESeqT
  ftrleseqstackemptyr <- readSTRef ftrleseqstackempty
  return ftrleseqstackemptyr
seqFromRLET xs              = do
  ftrleseqstack <- emptySTRLESeqT
  let rletseq = (\(RLET t) -> t) xs
  iFRLET rletseq
         ftrleseqstack
  ftrleseqstackr <- readSTRef ftrleseqstack
  return ftrleseqstackr
    where
      iFRLET (y1 DS.:<| y2 DS.:<| DS.Empty) ftrless =
        if | isJust y1    &&
             isNothing y2
           -> do pushFSTRLESeqT ftrless
                                Nothing
                 pure ()
           | otherwise
           -> do let y1' = read         $
                           DText.unpack $
                           fromJust y1 :: Int
                 let y2' = fromJust y2
                 CM.replicateM_ y1'
                                (pushFSTRLESeqT ftrless
                                                (Just y2'))
                 pure ()
      iFRLET (y1 DS.:<| y2 DS.:<| ys)       ftrless =
        if | isJust y1     &&
             isNothing y2
           -> do pushFSTRLESeqT ftrless
                                Nothing
                 iFRLET ys
                        ftrless
           | otherwise
           -> do let y1' = read         $
                           DText.unpack $
                           fromJust y1 :: Int
                 let y2' = fromJust y2
                 CM.replicateM_ y1'
                                (pushFSTRLESeqT ftrless
                                                (Just y2'))
                 iFRLET ys
                        ftrless
      iFRLET (DSI.Seq EmptyT)               _       = pure ()
      iFRLET (DSI.Seq (Single _))           _       = pure ()
      iFRLET (DSI.Seq (Deep _ _ _ _))       _       = pure ()

{---------------------------}
