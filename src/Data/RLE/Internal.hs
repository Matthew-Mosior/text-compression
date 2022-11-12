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
-- and the Inverse RLE implementations, namely 'vecToRLEB', 'vecToRLET', 'vecFromRLEB', and 'vecFromRLET'.
--
-- The RLE implementations rely heavily upon 'DVB.Vector' provided by the [vector](https://hackage.haskell.org/package/vector) library,
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
import Data.STRef as DSTR
import Data.Text as DText
import Data.Vector as DVB
import Data.Vector.Unboxed()
import GHC.Generics (Generic)
import Prelude as P


{-Base level types.-}

-- | Basic RLE ('ByteString') data type.
newtype RLEB = RLEB (DVB.Vector (Maybe ByteString))
  deriving (Eq,Ord,Show,Read,Generic)

-- | Basic RLE ('Text') data type.
newtype RLET = RLET (DVB.Vector (Maybe Text))
  deriving (Eq,Ord,Show,Read,Generic)

{-------------------}


{-toRLE (ByteString) functions.-}

-- | Abstract 'RLEVecB' type utilizing a sequence.
type RLEVecB = DVB.Vector (Maybe ByteString)

-- | Abstract data type representing a 'RLEVecB' in the (strict) ST monad.
type STRLEVecB s a = STRef s RLEVecB

-- | State function to push 'RLEVecB' data into stack.
pushSTRLEVecB :: STRLEVecB s (Maybe ByteString) -> Maybe ByteString -> ST s ()
pushSTRLEVecB s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (DVB.snoc s2 Nothing)
pushSTRLEVecB s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (DVB.snoc s2 (Just e))

-- | State function to create empty 'STRLEVecB' type.
emptySTRLEVecB :: ST s (STRLEVecB s a)
emptySTRLEVecB = newSTRef DVB.empty

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
vecToRLEB :: RLEVecB
          -> ST s RLEVecB
vecToRLEB (DVB.uncons -> Nothing)     = do
  brlevecstackempty  <- emptySTRLEVecB
  brlevecstackemptyr <- readSTRef brlevecstackempty
  return brlevecstackemptyr
vecToRLEB (DVB.uncons -> Just (v,vs)) = do
  brlevecstack     <- emptySTRLEVecB
  brlecounterstack <- emptySTRLECounterB
  brletempstack    <- emptySTRLETempB
  updateSTRLECounterB brlecounterstack
                      1 
  updateSTRLETempB brletempstack
                   v
  iRLEB vs
        brlevecstack
        brlecounterstack
        brletempstack
  brlevecstackr <- readSTRef brlevecstack
  return brlevecstackr
    where
      iRLEB (DVB.uncons -> Nothing)     brless brlecs brlets = do
        cbrlecs <- readSTRef brlecs
        cbrlets <- readSTRef brlets
        pushSTRLEVecB brless
                      (Just      $
                       BSC8.pack $
                       show cbrlecs)
        pushSTRLEVecB brless
                      cbrlets
        pure ()
      iRLEB (DVB.uncons -> Just (y,ys)) brless brlecs brlets = do
        cbrlecs <- readSTRef brlecs
        cbrlets <- readSTRef brlets
        if | isNothing y
           -> do pushSTRLEVecB brless
                               (Just      $
                                BSC8.pack $
                                show cbrlecs)
                 pushSTRLEVecB brless
                               cbrlets 
                 pushSTRLEVecB brless
                               (Just      $
                                BSC8.pack $
                                show (1 :: Int))
                 pushSTRLEVecB brless
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
           -> do pushSTRLEVecB brless
                               (Just      $
                                BSC8.pack $
                                show cbrlecs)
                 pushSTRLEVecB brless
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

-- | Abstract 'RLEVecT' type utilizing a sequence.
type RLEVecT = DVB.Vector (Maybe Text)

-- | Abstract data type representing a 'RLEVecT' in the (strict) ST monad.
type STRLEVecT s a = STRef s RLEVecT

-- | State function to push 'RLEVecT' data into stack.
pushSTRLEVecT :: STRLEVecT s (Maybe Text) -> (Maybe Text) -> ST s ()
pushSTRLEVecT s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (DVB.snoc s2 Nothing)
pushSTRLEVecT s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (DVB.snoc s2 (Just e))

-- | State function to create empty 'STRLEVecT' type.
emptySTRLEVecT :: ST s (STRLEVecT s a)
emptySTRLEVecT = newSTRef DVB.empty

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
vecToRLET :: RLEVecT ->
             ST s RLEVecT
vecToRLET (DVB.uncons -> Nothing)     = do
  trlevecstackempty  <- emptySTRLEVecT
  trlevecstackemptyr <- readSTRef trlevecstackempty
  return trlevecstackemptyr
vecToRLET (DVB.uncons -> Just (v,vs)) = do
  trlevecstack     <- emptySTRLEVecT
  trlecounterstack <- emptySTRLECounterT
  trletempstack    <- emptySTRLETempT
  updateSTRLECounterT trlecounterstack
                      1
  updateSTRLETempT trletempstack
                   v
  iRLET vs
        trlevecstack
        trlecounterstack
        trletempstack
  trlevecstackr <- readSTRef trlevecstack
  return trlevecstackr
    where
      iRLET (DVB.uncons -> Nothing)     trless trlecs trlets = do
        ctrlecs <- readSTRef trlecs
        ctrlets <- readSTRef trlets
        pushSTRLEVecT trless
                      (Just       $
                       DText.pack $
                       show ctrlecs)
        pushSTRLEVecT trless
                      ctrlets 
        pure ()
      iRLET (DVB.uncons -> Just (y,ys)) trless trlecs trlets = do
        ctrlecs <- readSTRef trlecs
        ctrlets <- readSTRef trlets
        if | isNothing y
           -> do pushSTRLEVecT trless
                               (Just       $
                                DText.pack $
                                show ctrlecs)
                 pushSTRLEVecT trless
                               ctrlets
                 pushSTRLEVecT trless
                               (Just       $
                                DText.pack $
                                show (1 :: Int))
                 pushSTRLEVecT trless
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
           -> do pushSTRLEVecT trless
                               (Just       $
                                DText.pack $
                                show ctrlecs)
                 pushSTRLEVecT trless
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

-- | 'DVB.Vector' auxilary function
-- to pattern match on first two elements
-- of a vector.
unconsb2 :: DVB.Vector a -> Maybe (a,DVB.Vector a,Maybe (DVB.Vector a))
unconsb2 v = if | DVB.length v < 3
                -> Just (DVB.unsafeHead v,DVB.drop 1 v,Nothing)
                | otherwise
                -> Just (DVB.unsafeHead v,DVB.drop 1 v,Just $ DVB.drop 2 v)

-- | Abstract 'FRLEVecB' type utilizing a sequence.
type FRLEVecB = DVB.Vector (Maybe ByteString)

-- | Abstract data type representing a 'FRLEVecB' in the (strict) ST monad.
type FSTRLEVecB s a = STRef s FRLEVecB

-- | State function to push 'FRLEVecB' data into stack.
pushFSTRLEVecB :: FSTRLEVecB s (Maybe ByteString) -> (Maybe ByteString) -> ST s ()
pushFSTRLEVecB s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (DVB.snoc s2 Nothing)
pushFSTRLEVecB s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (DVB.snoc s2 (Just e))

-- | State function to create empty 'FSTRLEVecB' type.
emptyFSTRLEVecB :: ST s (FSTRLEVecB s a)
emptyFSTRLEVecB = newSTRef DVB.empty

-- | Strict state monad function.
vecFromRLEB :: RLEB
            -> ST s FRLEVecB
vecFromRLEB (RLEB (DVB.uncons -> Nothing)) = do
  fbrlevecstackempty  <- emptyFSTRLEVecB
  fbrlevecstackemptyr <- readSTRef fbrlevecstackempty
  return fbrlevecstackemptyr
vecFromRLEB vs                             = do
  fbrlevecstack <- emptySTRLEVecB
  let rlebvec = (\(RLEB b) -> b) vs
  iFRLEB rlebvec
         fbrlevecstack
  fbrlevecstackr <- readSTRef fbrlevecstack
  return fbrlevecstackr
    where
      iFRLEB (unconsb2 -> Just (y1,y2,Nothing)) fbrless =
        if | isJust y1    &&
             isNothing (DVB.head y2)
           -> do pushFSTRLEVecB fbrless
                                Nothing
                 pure () 
           | otherwise
           -> do let y1' = read        $
                           BSC8.unpack $
                           fromJust y1 :: Int
                 let y2' = fromJust $
                           DVB.head y2
                 CM.replicateM_ y1'
                                (pushFSTRLEVecB fbrless
                                                (Just y2'))
                 pure () 
      iFRLEB (unconsb2 -> Just (y1,y2,Just ys)) fbrless =
        if | isJust y1     &&
             isNothing (DVB.head y2)
           -> do pushFSTRLEVecB fbrless
                                Nothing
                 iFRLEB ys
                        fbrless
           | otherwise
           -> do let y1' = read        $
                           BSC8.unpack $
                           fromJust y1 :: Int
                 let y2' = fromJust $
                           DVB.head y2
                 CM.replicateM_ y1'
                                (pushFSTRLEVecB fbrless
                                                (Just y2'))
                 iFRLEB ys
                        fbrless

{---------------------------------}


{-fromRLE (Text) functions.-}

-- | 'DVB.Vector' auxilary function
-- to pattern match on first two elements
-- of a vector.
unconst2 :: DVB.Vector a -> Maybe (a,DVB.Vector a, Maybe (DVB.Vector a))
unconst2 v = if | DVB.length v < 3
                -> Just (DVB.unsafeHead v,DVB.drop 1 v,Nothing)
                | otherwise
                -> Just (DVB.unsafeHead v, DVB.drop 1 v,Just $ DVB.drop 2 v)

-- | Abstract 'FRLEVecT' type utilizing a sequence.
type FRLEVecT = DVB.Vector (Maybe Text)

-- | Abstract data type representing a 'FRLEVecT' in the (strict) ST monad.
type FSTRLEVecT s a = STRef s FRLEVecT

-- | State function to push 'FSTRLEVecT' data into stack.
pushFSTRLEVecT :: FSTRLEVecT s (Maybe Text) -> (Maybe Text) -> ST s ()
pushFSTRLEVecT s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (DVB.snoc s2 Nothing)
pushFSTRLEVecT s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (DVB.snoc s2 (Just e))

-- | State function to create empty 'FSTRLEVecT' type.
emptyFSTRLEVecT :: ST s (FSTRLEVecT s a)
emptyFSTRLEVecT = newSTRef DVB.empty

-- | Strict state monad function.
vecFromRLET :: RLET ->
               ST s FRLEVecT
vecFromRLET (RLET (DVB.uncons -> Nothing)) = do
  ftrlevecstackempty  <- emptyFSTRLEVecT
  ftrlevecstackemptyr <- readSTRef ftrlevecstackempty
  return ftrlevecstackemptyr
vecFromRLET vs                             = do
  ftrlevecstack <- emptySTRLEVecT
  let rletvec = (\(RLET t) -> t) vs
  iFRLET rletvec
         ftrlevecstack
  ftrlevecstackr <- readSTRef ftrlevecstack
  return ftrlevecstackr
    where 
      iFRLET (unconst2 -> Just (y1,y2,Nothing)) ftrless =
        if | isJust y1    &&
             isNothing (DVB.head y2)
           -> do pushFSTRLEVecT ftrless
                                Nothing
                 pure ()
           | otherwise
           -> do let y1' = read         $
                           DText.unpack $
                           fromJust y1 :: Int
                 let y2' = fromJust $
                           DVB.head y2
                 CM.replicateM_ y1'
                                (pushFSTRLEVecT ftrless
                                                (Just y2'))
                 pure ()
      iFRLET (unconst2 -> Just (y1,y2,Just ys)) ftrless =
        if | isJust y1     &&
             isNothing (DVB.head y2)
           -> do pushFSTRLEVecT ftrless
                                Nothing
                 iFRLET ys
                        ftrless
           | otherwise
           -> do let y1' = read         $
                           DText.unpack $
                           fromJust y1 :: Int
                 let y2' = fromJust $
                           DVB.head y2
                 CM.replicateM_ y1'
                                (pushFSTRLEVecT ftrless
                                                (Just y2'))
                 iFRLET ys
                        ftrless

{---------------------------}
