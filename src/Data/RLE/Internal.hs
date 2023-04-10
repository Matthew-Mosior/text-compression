{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE ViewPatterns     #-}
{-# LANGUAGE Strict           #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ScopedTypeVariables #-}


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
-- and the Inverse RLE implementations, namely 'seqToRLE', 'seqToRLET', 'seqFromRLE', and 'seqFromRLET'.
--
-- The RLE implementations rely heavily upon 'Seq' provided by the [containers](https://hackage.haskell.org/package/containers),
-- 'STRef' and associated functions in the [stref](https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-STRef.html) library,
-- and 'runST' in the [Control.Monad.ST](https://hackage.haskell.org/package/base-4.17.0.0/docs/Control-Monad-ST.html) library.


module Data.RLE.Internal ( -- * Base RLE types
                           Pack(pck, unpck, Itm, one),
                           RLE(..),
                           -- * To RLE functions
                           RLESeq,
                           STRLESeq,
                           pushSTRLESeq,
                           emptySTRLESeq,
                           STRLETemp,
                           updateSTRLETemp,
                           emptySTRLETemp,
                           STRLECounter,
                           updateSTRLECounter,
                           emptySTRLECounter,
                           seqToRLE,
                           -- * From RLE functions
                           FRLESeq,
                           FSTRLESeq,
                           pushFSTRLESeq,
                           emptyFSTRLESeq,
                           seqFromRLE,
                           -- * From RLE (Text) functions
                         ) where

import Control.Monad as CM
import Control.Monad.ST as CMST
import Control.Monad.State.Strict()
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC8 (pack,unpack)
import Data.ByteString.Internal()
import Data.List()
import Data.Maybe as DMaybe (fromJust,isJust,isNothing)
import Data.Sequence as DS (Seq(..),empty,(|>))
import Data.Sequence.Internal as DSI
import Data.STRef as DSTR
import Data.Text as DText
import Data.Word (Word8)
import GHC.Generics (Generic)
import Prelude as P


class (Monoid b, Eq b) => Pack b where
  pck :: [Itm b] -> b
  unpck :: b -> [Itm b]
  type Itm b
  one :: Itm b -> b
  fromString :: String -> b
  toString :: b -> String
{-# DEPRECATED fromString "temporary until run representation is changed" #-}
{-# DEPRECATED toString "temporary until run representation is changed" #-}

instance Pack ByteString where
  pck = BS.pack
  unpck = BS.unpack
  type Itm ByteString = Word8
  one = BS.singleton
  fromString = BSC8.pack
  toString = BSC8.unpack

instance Pack Text where
  pck = DText.pack
  unpck = DText.unpack
  type Itm Text = Char
  one = DText.singleton
  fromString = pck
  toString = unpck

{-Base level types.-}

-- | Basic RLE ('ByteString') data type.
newtype RLE b = RLE (Seq (Maybe b))
  deriving (Eq,Ord,Show,Read,Generic)

{-------------------}


{-toRLE (ByteString) functions.-}

-- | Abstract 'RLESeq' type utilizing a 'Seq'.
type RLESeq b = Seq (Maybe b)

-- | Abstract data type representing a 'RLESeq b' in the (strict) ST monad.
type STRLESeq b s a = STRef s (RLESeq b)

-- | State function to push 'RLESeq' data into stack.
pushSTRLESeq :: STRLESeq b s (Maybe b)
              -> Maybe b
              -> ST s ()
pushSTRLESeq s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Nothing)
pushSTRLESeq s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Just e)

-- | State function to create empty 'STRLESeq b' type.
emptySTRLESeq :: ST s (STRLESeq b s a)
emptySTRLESeq = newSTRef DS.empty

-- | Abstract 'STRLETemp' and associated state type.
type STRLETemp b s a = STRef s (Maybe b)

-- | State function to update 'STRLETemp'.
updateSTRLETemp :: STRLETemp b s (Maybe b)
                 -> Maybe b
                 -> ST s ()
updateSTRLETemp s Nothing  = writeSTRef s Nothing
updateSTRLETemp s (Just e) = writeSTRef s (Just e)

-- | State function to create empty 'STRLETemp' type.
emptySTRLETemp :: Monoid b => ST s (STRLETemp b s a)
emptySTRLETemp = newSTRef (Just mempty)

-- | Abstract 'STRLECounter' state type.
type STRLECounter b s a = STRef s Int

-- | State function to update 'STRLECounter'.
updateSTRLECounter :: STRLECounter b s Int
                    -> Int
                    -> ST s ()
updateSTRLECounter s e = writeSTRef s e

-- | State function to create empty 'STRLECounter' type.
emptySTRLECounter :: ST s (STRLECounter b s Int)
emptySTRLECounter = newSTRef (-1)

-- | Strict state monad function.
seqToRLE :: forall b. Pack b => RLESeq b -> RLESeq b
seqToRLE DS.Empty      = CMST.runST $ do
  brleseqstackempty  <- emptySTRLESeq
  brleseqstackemptyr <- readSTRef brleseqstackempty
  return brleseqstackemptyr
seqToRLE (x DS.:<| xs) = CMST.runST $ do
  brleseqstack     <- emptySTRLESeq
  brlecounterstack <- emptySTRLECounter
  brletempstack    <- emptySTRLETemp
  updateSTRLECounter brlecounterstack
                      1
  updateSTRLETemp brletempstack
                   x
  iRLE xs
        brleseqstack
        brlecounterstack
        brletempstack
  brleseqstackr <- readSTRef brleseqstack
  return brleseqstackr
    where
      iRLE ::
           Seq (Maybe b)
        -> STRef s (RLESeq b)
        -> STRef s Int
        -> STRef s (Maybe b)
        -> ST s ()
      iRLE DS.Empty      brless brlecs brlets = do
        cbrlecs <- readSTRef brlecs
        cbrlets <- readSTRef brlets
        pushSTRLESeq brless
                      (Just      $
                       fromString $
                       show cbrlecs)
        pushSTRLESeq brless
                      cbrlets
        pure ()
      iRLE (y DS.:<| ys) brless brlecs brlets = do
        cbrlecs <- readSTRef brlecs
        cbrlets <- readSTRef brlets
        if | isNothing y
           -> do pushSTRLESeq brless
                               (Just      $
                                fromString $
                                show cbrlecs)
                 pushSTRLESeq brless
                               cbrlets
                 pushSTRLESeq brless
                               (Just      $
                                fromString $
                                show (1 :: Int))
                 pushSTRLESeq brless
                               Nothing
                 updateSTRLETemp brlets
                                  Nothing
                 iRLE ys
                       brless
                       brlecs
                       brlets
           | isNothing cbrlets
           -> do updateSTRLECounter brlecs
                                     1
                 updateSTRLETemp brlets
                                  y
                 iRLE ys
                       brless
                       brlecs
                       brlets
           | fromJust cbrlets == fromJust y
           -> do updateSTRLECounter brlecs
                                     (cbrlecs + 1)
                 iRLE ys
                       brless
                       brlecs
                       brlets
           | otherwise
           -> do pushSTRLESeq brless
                               (Just      $
                                fromString $
                                show cbrlecs)
                 pushSTRLESeq brless
                               cbrlets
                 updateSTRLECounter brlecs
                                     1
                 updateSTRLETemp brlets
                                  y
                 iRLE ys
                       brless
                       brlecs
                       brlets

{-------------------------}


{-fromRLE (ByteString) functions.-}

-- | Abstract 'FRLESeq' type utilizing a 'Seq'.
type FRLESeq b = Seq (Maybe b)

-- | Abstract data type representing a 'FRLESeq' in the (strict) ST monad.
type FSTRLESeq b s a = STRef s (FRLESeq b)

-- | State function to push 'FRLESeq' data into stack.
pushFSTRLESeq :: FSTRLESeq b s (Maybe b)
               -> (Maybe b)
               -> ST s ()
pushFSTRLESeq s Nothing  = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Nothing)
pushFSTRLESeq s (Just e) = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> Just e)

-- | State function to create empty 'FSTRLESeq' type.
emptyFSTRLESeq :: ST s (FSTRLESeq b s a)
emptyFSTRLESeq = newSTRef DS.empty

-- | Strict state monad function.
seqFromRLE :: forall b. Pack b => RLE b -> FRLESeq b
seqFromRLE (RLE DS.Empty) = CMST.runST $ do
  fbrleseqstackempty  <- emptyFSTRLESeq
  fbrleseqstackemptyr <- readSTRef fbrleseqstackempty
  return fbrleseqstackemptyr
seqFromRLE xs              = CMST.runST $ do
  fbrleseqstack <- emptySTRLESeq
  let rlebseq = (\(RLE b) -> b) xs
  iFRLE rlebseq
         fbrleseqstack
  fbrleseqstackr <- readSTRef fbrleseqstack
  return fbrleseqstackr
    where
      iFRLE :: Seq (Maybe b) -> STRef s (RLESeq b) -> ST s ()
      iFRLE (y1 DS.:<| y2 DS.:<| DS.Empty) fbrless =
        if | isJust y1    &&
             isNothing y2
           -> do pushFSTRLESeq fbrless
                                Nothing
                 pure ()
           | otherwise
           -> do let y1' = read $
                           toString $
                           fromJust y1 :: Int
                 let y2' = fromJust y2
                 CM.replicateM_ y1'
                                (pushFSTRLESeq fbrless
                                                (Just y2'))
                 pure ()
      iFRLE (y1 DS.:<| y2 DS.:<| ys)       fbrless =
        if | isJust y1     &&
             isNothing y2
           -> do pushFSTRLESeq fbrless
                                Nothing
                 iFRLE ys
                        fbrless
           | otherwise
           -> do let y1' = read        $
                           toString $
                           fromJust y1 :: Int
                 let y2' = fromJust y2
                 CM.replicateM_ y1'
                                (pushFSTRLESeq fbrless
                                                (Just y2'))
                 iFRLE ys
                        fbrless
      iFRLE (DSI.Seq EmptyT)               _       = pure ()
      iFRLE (DSI.Seq (Single _))           _       = pure ()
      iFRLE (DSI.Seq (Deep _ _ _ _))       _       = pure ()

{---------------------------}
