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


module Data.RLE.Internal ( Pack(pck, unpck, Itm, one),
                           -- * Base RLE types
                           RLE(..),
                           -- * To RLE functions
                           seqToRLE,
                           -- * From RLE functions
                           seqFromRLE,
                         ) where

import Control.Monad as CM
import Control.Monad.ST as CMST
import Data.ByteString as BS hiding (count)
import Data.ByteString.Char8 as BSC8 (pack,unpack)
import Data.Maybe as DMaybe (fromJust,isJust,isNothing)
import Data.Sequence as DS (Seq(..),empty,(|>))
import Data.Sequence.Internal as DSI
import Data.STRef as DSTR
import Data.Text as DText hiding (count)
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

-- | State function to push 'RLESeq' data into stack.
push :: STRef s (Seq (Maybe b)) -> Maybe b -> ST s ()
push s e = do
  s2 <- readSTRef s
  writeSTRef s (s2 DS.|> e)

seqToRLE :: forall b. Pack b => Seq (Maybe b) -> Seq (Maybe b)
seqToRLE DS.Empty      = CMST.runST $ do
  stackRef  <- newSTRef DS.empty
  stack <- readSTRef stackRef
  return stack
seqToRLE (x DS.:<| xs) = CMST.runST $ do
  stackRef     <- newSTRef DS.empty
  countersRef <- newSTRef (-1)
  itemsRef    <- newSTRef (Just mempty)
  writeSTRef countersRef 1
  writeSTRef itemsRef x
  iRLE xs stackRef countersRef itemsRef
  stack <- readSTRef stackRef
  return stack
    where
      iRLE ::
           Seq (Maybe b)
        -> STRef s (Seq (Maybe b))
        -> STRef s Int
        -> STRef s (Maybe b)
        -> ST s ()
      iRLE DS.Empty stackRef countRef brlets = do
        count <- readSTRef countRef
        item <- readSTRef brlets
        push stackRef (Just $ fromString $ show count)
        push stackRef item
        pure ()
      iRLE (y DS.:<| ys) stackRef countRef brlets = do
        count <- readSTRef countRef
        item <- readSTRef brlets
        if | isNothing y
           -> do push stackRef (Just $ fromString $ show count)
                 push stackRef item
                 push stackRef (Just $ fromString $ show (1 :: Int))
                 push stackRef Nothing
                 writeSTRef brlets Nothing
                 iRLE ys stackRef countRef brlets
           | isNothing item
           -> do writeSTRef countRef 1
                 writeSTRef brlets y
                 iRLE ys stackRef countRef brlets
           | fromJust item == fromJust y
           -> do writeSTRef countRef (count + 1)
                 iRLE ys stackRef countRef brlets
           | otherwise
           -> do push stackRef (Just $ fromString $ show count)
                 push stackRef item
                 writeSTRef countRef 1
                 writeSTRef brlets y
                 iRLE ys stackRef countRef brlets

seqFromRLE :: forall b. Pack b => RLE b -> Seq (Maybe b)
seqFromRLE (RLE DS.Empty) = CMST.runST $ do
  stackRef <- newSTRef DS.empty
  stack <- readSTRef stackRef
  return stack
seqFromRLE (RLE xs) = CMST.runST $ do
  stackRef <- newSTRef DS.empty
  iFRLE xs stackRef
  stack <- readSTRef stackRef
  return stack
    where
      iFRLE :: Seq (Maybe b) -> STRef s (Seq (Maybe b)) -> ST s ()
      iFRLE (y1 DS.:<| y2 DS.:<| DS.Empty) stackRef =
        if | isJust y1 && isNothing y2
           -> do push stackRef Nothing
                 pure ()
           | otherwise
           -> do let y1' = read $ toString $ fromJust y1 :: Int
                 let y2' = fromJust y2
                 CM.replicateM_ y1' (push stackRef (Just y2'))
                 pure ()
      iFRLE (y1 DS.:<| y2 DS.:<| ys) stackRef =
        if | isJust y1 && isNothing y2
           -> do push stackRef Nothing
                 iFRLE ys stackRef
           | otherwise
           -> do let y1' = read        $
                           toString $
                           fromJust y1 :: Int
                 let y2' = fromJust y2
                 CM.replicateM_ y1' (push stackRef (Just y2'))
                 iFRLE ys stackRef
      iFRLE (DSI.Seq EmptyT)               _       = pure ()
      iFRLE (DSI.Seq (Single _))           _       = pure ()
      iFRLE (DSI.Seq (Deep _ _ _ _))       _       = pure ()
