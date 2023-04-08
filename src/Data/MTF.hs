{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}


-- |
-- Module      :  Data.MTF
-- Copyright   :  (c) Matthew Mosior 2022
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Move-to-front transform (MTF)
--
-- Users will get the most mileage by first compressing to a 'BWT'
-- on the initial 'ByteString' or 'Text' input before compressing to
-- a 'MTFB' or 'MTFT'.
--
-- To do this, users can use the 'bytestringToBWTToMTFB' and 'bytestringToBWTToMTFT' functions,
-- as well as the 'textToBWTToMTFB' and 'textToBWTToMTFT' functions.
--
-- The base functions for 'ByteString', 'bytestringToMTFB' and 'bytestringToMTFT' can be used to
-- convert a 'Seq' ('Maybe' 'ByteString') to a 'MTFB' and 'MTFT', respectively.
--
-- Likewise, the base functions for 'Text', 'textToMTFB' and 'textToMTFT' can be used to
-- convert a 'Seq' ('Maybe' 'Text') to a 'MTFB' and 'MTFT' respectively.
--
-- There are various other lower-level functions for interacting with the MTF implementation on 'ByteString' and 'Text' as well.
--
-- @"Data.MTF.Internal"@ contains efficient and stateful implementations of the MTF and Inverse MTF algorithms.


module Data.MTF ( -- * To MTF functions
                  bytestringToBWTToMTFB,
                  bytestringToBWTToMTFT,
                  textToBWTToMTFB,
                  textToBWTToMTFT,
                  textBWTToMTFB,
                  bytestringBWTToMTFB,
                  textBWTToMTFT,
                  bytestringBWTToMTFT,
                  textToMTFB,
                  bytestringToMTFB,
                  textToMTFT,
                  bytestringToMTFT,
                  -- * From MTF functions
                  bytestringFromBWTFromMTFB,
                  bytestringFromBWTFromMTFT,
                  textFromBWTFromMTFB,
                  textFromBWTFromMTFT,
                  textBWTFromMTFT,
                  bytestringBWTFromMTFT,
                  textBWTFromMTFB,
                  bytestringBWTFromMTFB,
                  textFromMTFB,
                  bytestringFromMTFB,
                  textFromMTFT,
                  bytestringFromMTFT,
                  tests
                ) where

import Data.BWT hiding (tests)
import Data.BWT.Internal
import Data.MTF.Internal

import Control.Monad()
import Control.Monad.ST as CMST
import Control.Monad.State.Strict()
import Data.ByteString as BS
import Data.ByteString.Char8()
import Data.Char()
import Data.Foldable()
import Data.Maybe as DMaybe (isNothing,fromJust)
import Data.Sequence as DS (Seq(..))
import Data.STRef()
import Data.Text as DText
import Data.Text.Encoding as DTE (decodeUtf8,encodeUtf8)
import Data.Word (Word8)
import Prelude as P
import Test.HUnit


{-toMTF Function(s)-}

-- | Helper function for converting a 'ByteString'
-- to a 'MTFB' via a 'BWT' first.
bytestringToBWTToMTFB :: ByteString ->
                         MTFB
bytestringToBWTToMTFB = bytestringBWTToMTFB . bytestringToBWT

-- | Helper function for converting a 'ByteString'
-- to a 'MTFT' via a 'BWT' first.
bytestringToBWTToMTFT :: ByteString ->
                         MTFT
bytestringToBWTToMTFT = bytestringBWTToMTFT . bytestringToBWT

-- | Helper function for converting a 'Text'
-- to a 'MTFB' via a 'BWT' first.
textToBWTToMTFB :: Text ->
                   MTFB
textToBWTToMTFB = textBWTToMTFB . textToBWT

-- | Helper function for converting a 'Text'
-- to a 'MTFT' via a 'BWT' first.
textToBWTToMTFT :: Text ->
                   MTFT
textToBWTToMTFT = textBWTToMTFT . textToBWT

-- | Take a 'BWT' of 'Word8's and generate the
-- Move-to-front transform ('MTFB').
textBWTToMTFB :: TextBWT
              -> MTFB
textBWTToMTFB xs =
  MTFB (CMST.runST $ seqToMTFB xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just         $
                              BS.singleton $
                              fromJust x
                 )
            ((\(BWT t) -> t) $
            ((\(TextBWT t) -> t) xs))

-- | Take a 'BWT' of 'Word8's and generate the
-- Move-to-front transform ('MTFB').
bytestringBWTToMTFB :: BWT Word8
                    -> MTFB
bytestringBWTToMTFB xs =
  MTFB (CMST.runST $ seqToMTFB xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just         $
                              BS.singleton $
                              fromJust x
                 )
            ((\(BWT t) -> t) xs)

-- | Take a 'BWT' of 'Word8's and generate the
-- Move-to-front transform ('MTFB').
textBWTToMTFT :: TextBWT
              -> MTFT
textBWTToMTFT xs =
  MTFT (CMST.runST $ seqToMTFT xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just           $
                              DTE.decodeUtf8 $
                              BS.singleton   $
                              fromJust x
                 )
            ((\(BWT t) -> t) $
            ((\(TextBWT t) -> t) xs))

-- | Take a 'BWT' of 'Word8's and generate the
-- Move-to-front transform ('MTFT').
bytestringBWTToMTFT :: BWT Word8
                    -> MTFT
bytestringBWTToMTFT xs =
  MTFT (CMST.runST $ seqToMTFT xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just           $
                              DTE.decodeUtf8 $
                              BS.singleton   $
                              fromJust x
                 )
            ((\(BWT t) -> t) xs)

-- | Takes a 'Text' and returns the Move-to-front transform ('MTFB').
textToMTFB :: Seq (Maybe Text)
           -> MTFB
textToMTFB DS.Empty = MTFB (DS.Empty,DS.Empty)
textToMTFB xs       =
  MTFB (CMST.runST $ seqToMTFB xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just            $
                               DTE.encodeUtf8 $
                               fromJust x
                 )
            xs

-- | Takes a 'Seq' of 'ByteString's and returns the Move-to-front transform ('MTFB').
bytestringToMTFB :: Seq (Maybe ByteString)
                 -> MTFB
bytestringToMTFB DS.Empty = MTFB (DS.Empty,DS.Empty)
bytestringToMTFB xs       =
 MTFB (CMST.runST $ seqToMTFB xs)

-- | Takes a 'Text' and returns the Move-to-front transform ('MTFT').
textToMTFT :: Seq (Maybe Text)
           -> MTFT
textToMTFT DS.Empty = MTFT (DS.Empty,DS.Empty)
textToMTFT xs       =
  MTFT (CMST.runST $ seqToMTFT xs)

-- | Takes a 'ByteString' and returns the Move-to-front transform ('MTFT').
bytestringToMTFT :: Seq (Maybe ByteString)
                 -> MTFT
bytestringToMTFT DS.Empty = MTFT (DS.Empty,DS.Empty)
bytestringToMTFT xs       =
  MTFT (CMST.runST $ seqToMTFT xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just           $
                              DTE.decodeUtf8 $
                              fromJust x
                 )
            xs

{-------------------}


{-fromMTF function(s)-}

-- | Helper function for converting a 'BWT'ed 'MTFB'
-- back to the original 'ByteString'.
bytestringFromBWTFromMTFB :: MTFB
                          -> ByteString
bytestringFromBWTFromMTFB = bytestringFromByteStringBWT . bytestringBWTFromMTFB

-- | Helper function for converting a 'BWT'ed 'MTFT'
-- back to the original 'ByteString'.
bytestringFromBWTFromMTFT :: MTFT
                          -> ByteString
bytestringFromBWTFromMTFT xs = bytestringFromByteStringBWT $
                               BWT                         $
                               fmap (\x -> if | isNothing x
                                              -> Nothing
                                              | otherwise
                                              -> Just           $
                                                 DTE.encodeUtf8 $
                                                 fromJust x
                                    )
                                                           $
                            ((\(BWT t) -> t) (textBWTFromMTFT xs))

-- | Helper function for converting a 'BWT'ed 'MTFB'
-- back to the original 'Text'.
textFromBWTFromMTFB :: MTFB
                    -> Text
textFromBWTFromMTFB = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromMTFB

-- | Helper function for converting a 'BWT'ed 'MTFT'
-- back to the original 'Text'.
textFromBWTFromMTFT :: MTFT
                    -> Text
textFromBWTFromMTFT = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromMTFT

-- | Takes a 'MTFT' and returns
-- the 'BWT' of 'Text's.
textBWTFromMTFT :: MTFT
                -> BWT Text
textBWTFromMTFT (MTFT (DS.Empty,_)) = BWT DS.Empty
textBWTFromMTFT (MTFT (_,DS.Empty)) = BWT DS.Empty
textBWTFromMTFT xs                  =
  BWT (CMST.runST $ seqFromMTFT xs)

-- | Takes a 'MTFT' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromMTFT :: MTFT
                      -> BWT ByteString
bytestringBWTFromMTFT (MTFT (DS.Empty,_)) = BWT DS.Empty
bytestringBWTFromMTFT (MTFT (_,DS.Empty)) = BWT DS.Empty
bytestringBWTFromMTFT xs                  = do
  let originalbwtb = CMST.runST $ seqFromMTFT xs
  BWT (fmap (\x -> if | isNothing x
                      -> Nothing
                      | otherwise
                      -> Just           $
                         DTE.encodeUtf8 $
                        fromJust x
            ) originalbwtb)

-- | Takes a 'MTFB' and returns
-- the 'BWT' of 'Text's.
textBWTFromMTFB :: MTFB
                -> BWT Text
textBWTFromMTFB (MTFB (DS.Empty,_)) = BWT DS.Empty
textBWTFromMTFB (MTFB (_,DS.Empty)) = BWT DS.Empty
textBWTFromMTFB xs                  = do
  let originalbwtt = CMST.runST $ seqFromMTFB xs
  BWT (fmap (\x -> if | isNothing x
                      -> Nothing
                      | otherwise
                      -> Just           $
                         DTE.decodeUtf8 $
                        fromJust x
            ) originalbwtt)

-- | Take a 'MTFB' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromMTFB :: MTFB
                      -> BWT ByteString
bytestringBWTFromMTFB (MTFB (DS.Empty,_)) = BWT DS.Empty
bytestringBWTFromMTFB (MTFB (_,DS.Empty)) = BWT DS.Empty
bytestringBWTFromMTFB xs              =
  BWT (CMST.runST $ seqFromMTFB xs)

-- | Takes a 'MTFB' and returns
-- the original 'Seq' of 'Text's.
textFromMTFB :: MTFB
             -> Seq (Maybe Text)
textFromMTFB (MTFB (DS.Empty,_)) = DS.Empty
textFromMTFB (MTFB (_,DS.Empty)) = DS.Empty
textFromMTFB xs                  = do
  let originalt = CMST.runST $ seqFromMTFB xs
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    DTE.decodeUtf8 $
                    fromJust x
       ) originalt

-- | Takes a 'MTFB' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromMTFB :: MTFB
                   -> Seq (Maybe ByteString)
bytestringFromMTFB (MTFB (DS.Empty,_)) = DS.Empty
bytestringFromMTFB (MTFB (_,DS.Empty)) = DS.Empty
bytestringFromMTFB xs                  =
  CMST.runST $ seqFromMTFB xs

-- | Takes a 'MTFT' and returns
-- the original 'Seq' of 'Text's.
textFromMTFT :: MTFT
             -> Seq (Maybe Text)
textFromMTFT (MTFT (DS.Empty,_)) = DS.Empty
textFromMTFT (MTFT (_,DS.Empty)) = DS.Empty
textFromMTFT xs                  =
  CMST.runST $ seqFromMTFT xs

-- | Takes a 'MTFT' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromMTFT :: MTFT
                   -> Seq (Maybe ByteString)
bytestringFromMTFT (MTFT (DS.Empty,_)) = DS.Empty
bytestringFromMTFT (MTFT (_,DS.Empty)) = DS.Empty
bytestringFromMTFT xs                  = do
  let originalb = CMST.runST $ seqFromMTFT xs
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    DTE.encodeUtf8 $
                    fromJust x
       ) originalb

{---------------------}

tests :: Test
tests = TestList []
