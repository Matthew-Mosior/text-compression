{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE Strict        #-}


-- |
-- Module      :  Data.FMIndex
-- Copyright   :  (c) Matthew Mosior 2022
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Full-text Minute-space index (FM-index)
--
-- Users will get the most mileage by first compressing to a 'BWT'
-- on the initial 'ByteString' or 'Text' input before compressing to
-- a 'FMIndexB' or 'FMIndexT'.
--
-- To do this, users can use the 'bytestringToBWTToFMIndexB' and 'bytestringToBWTToFMIndexT' functions,
-- as well as the 'textToBWTToFMIndexB' and 'textToBWTToFMIndexT' functions.
--
-- The base functions for 'ByteString', 'bytestringToFMIndexB' and 'bytestringToFMIndexT' can be used to
-- convert a 'Seq' ('Maybe' 'ByteString') to a 'FMIndexB' and 'FMIndexT', respectively.
--
-- Likewise, the base functions for 'Text', 'textToFMIndexB' and 'textToFMIndexT' can be used to
-- convert a 'Seq' ('Maybe' 'Text') to a 'FMIndexB' and 'FMIndexT' respectively.
--
-- There are various other lower-level functions for interacting with the FMIndex implementation on 'ByteString' and 'Text' as well.
--
-- @"Data.FMIndex.Internal"@ contains efficient and stateful implementations of the FMIndex and Inverse FMIndex algorithms.


module Data.FMIndex ( -- * To FMIndex functions
                      bytestringToBWTToFMIndexB,
                      bytestringToBWTToFMIndexT,
                      textToBWTToFMIndexB,
                      textToBWTToFMIndexT,
                      textBWTToFMIndexB,
                      bytestringBWTToFMIndexB,
                      textBWTToFMIndexT,
                      bytestringBWTToFMIndexT,
                      textToFMIndexB,
                      bytestringToFMIndexB,
                      textToFMIndexT,
                      bytestringToFMIndexT,
                      -- * From FMIndex functions
                      bytestringFromBWTFromFMIndexB,
                      bytestringFromBWTFromFMIndexT,
                      textFromBWTFromFMIndexB,
                      textFromBWTFromFMIndexT,
                      textBWTFromFMIndexT,
                      bytestringBWTFromFMIndexT,
                      textBWTFromFMIndexB,
                      bytestringBWTFromFMIndexB,
                      textFromFMIndexB,
                      bytestringFromFMIndexB,
                      textFromFMIndexT,
                      bytestringFromFMIndexT
                    ) where

import Data.BWT
import Data.BWT.Internal
import Data.FMIndex.Internal

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


{-toFMIndex Function(s)-}

-- | Helper function for converting a 'ByteString'
-- to a 'FMIndexB' via a 'BWT' first.
bytestringToBWTToFMIndexB :: ByteString ->
                             FMIndexB
bytestringToBWTToFMIndexB = bytestringBWTToFMIndexB . bytestringToBWT

-- | Helper function for converting a 'ByteString'
-- to a 'FMIndexT' via a 'BWT' first.
bytestringToBWTToFMIndexT :: ByteString ->
                             FMIndexT
bytestringToBWTToFMIndexT = bytestringBWTToFMIndexT . bytestringToBWT

-- | Helper function for converting a 'Text'
-- to a 'FMIndexB' via a 'BWT' first.
textToBWTToFMIndexB :: Text ->
                       FMIndexB
textToBWTToFMIndexB = textBWTToFMIndexB . textToBWT

-- | Helper function for converting a 'Text'
-- to a 'FMIndexT' via a 'BWT' first.
textToBWTToFMIndexT :: Text ->
                       FMIndexT
textToBWTToFMIndexT = textBWTToFMIndexT . textToBWT

-- | Take a 'BWT' of 'Word8's and generate the
-- FM-index ('FMIndexB').
textBWTToFMIndexB :: TextBWT
                  -> FMIndexB
textBWTToFMIndexB xs =
  FMIndexB (CMST.runST $ seqToFMIndexB xss)
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
-- FM-index ('FMIndexB').
bytestringBWTToFMIndexB :: BWT Word8
                        -> FMIndexB
bytestringBWTToFMIndexB xs =
  FMIndexB (CMST.runST $ seqToFMIndexB xss)
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
-- FM-index ('FMIndexB').
textBWTToFMIndexT :: TextBWT
                  -> FMIndexT
textBWTToFMIndexT xs =
  FMIndexT (CMST.runST $ seqToFMIndexT xss)
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
-- FM-index ('FMIndexT').
bytestringBWTToFMIndexT :: BWT Word8
                        -> FMIndexT
bytestringBWTToFMIndexT xs =
  FMIndexT (CMST.runST $ seqToFMIndexT xss)
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

-- | Takes a 'Text' and returns the FM-index ('FMIndexB').
textToFMIndexB :: Seq (Maybe Text)
               -> FMIndexB
textToFMIndexB DS.Empty = FMIndexB DS.Empty
textToFMIndexB xs       =
  FMIndexB (CMST.runST $ seqToFMIndexB xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just            $
                               DTE.encodeUtf8 $
                               fromJust x
                 )
            xs

-- | Takes a 'Seq' of 'ByteString's and returns the FM-index ('FMIndexB').
bytestringToFMIndexB :: Seq (Maybe ByteString)
                     -> FMIndexB
bytestringToFMIndexB DS.Empty = FMIndexB DS.Empty
bytestringToFMIndexB xs       =
 FMIndexB (CMST.runST $ seqToFMIndexB xs)

-- | Takes a 'Text' and returns the FM-index ('FMIndexT').
textToFMIndexT :: Seq (Maybe Text)
               -> FMIndexT
textToFMIndexT DS.Empty = FMIndexT DS.Empty
textToFMIndexT xs       =
  FMIndexT (CMST.runST $ seqToFMIndexT xs)

-- | Takes a 'ByteString' and returns the FM-index ('FMIndexT').
bytestringToFMIndexT :: Seq (Maybe ByteString)
                     -> FMIndexT
bytestringToFMIndexT DS.Empty = FMIndexT DS.Empty 
bytestringToFMIndexT xs       =
  FMIndexT (CMST.runST $ seqToFMIndexT xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just           $
                              DTE.decodeUtf8 $
                              fromJust x
                 )
            xs

{-----------------------}


{-fromFMIndex function(s)-}

-- | Helper function for converting a 'BWT'ed 'FMIndexB'
-- back to the original 'ByteString'.
bytestringFromBWTFromFMIndexB :: FMIndexB
                              -> ByteString
bytestringFromBWTFromFMIndexB = bytestringFromByteStringBWT . bytestringBWTFromFMIndexB

-- | Helper function for converting a 'BWT'ed 'FMIndexT'
-- back to the original 'ByteString'.
bytestringFromBWTFromFMIndexT :: FMIndexT
                              -> ByteString
bytestringFromBWTFromFMIndexT xs = bytestringFromByteStringBWT $
                               BWT                         $
                               fmap (\x -> if | isNothing x
                                              -> Nothing
                                              | otherwise
                                              -> Just           $
                                                 DTE.encodeUtf8 $
                                                 fromJust x
                                    )
                                                           $
                            ((\(BWT t) -> t) (textBWTFromFMIndexT xs))

-- | Helper function for converting a 'BWT'ed 'FMIndexB'
-- back to the original 'Text'.
textFromBWTFromFMIndexB :: FMIndexB
                        -> Text
textFromBWTFromFMIndexB = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromFMIndexB

-- | Helper function for converting a 'BWT'ed 'FMIndexT'
-- back to the original 'Text'.
textFromBWTFromFMIndexT :: FMIndexT
                        -> Text
textFromBWTFromFMIndexT = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromFMIndexT

-- | Takes a 'FMIndexT' and returns
-- the 'BWT' of 'Text's.
textBWTFromFMIndexT :: FMIndexT
                    -> BWT Text
textBWTFromFMIndexT (FMIndexT DS.Empty) = BWT DS.Empty
textBWTFromFMIndexT xs                  =
  BWT (seqFromFMIndexT xs)

-- | Takes a 'FMIndexT' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromFMIndexT :: FMIndexT
                          -> BWT ByteString
bytestringBWTFromFMIndexT (FMIndexT DS.Empty) = BWT DS.Empty
bytestringBWTFromFMIndexT xs                  = do
  let originalbwtb = seqFromFMIndexT xs
  BWT (fmap (\x -> if | isNothing x
                      -> Nothing
                      | otherwise
                      -> Just           $
                         DTE.encodeUtf8 $
                        fromJust x
            ) originalbwtb)

-- | Takes a 'FMIndexB' and returns
-- the 'BWT' of 'Text's.
textBWTFromFMIndexB :: FMIndexB
                    -> BWT Text
textBWTFromFMIndexB (FMIndexB DS.Empty) = BWT DS.Empty
textBWTFromFMIndexB xs                  = do
  let originalbwtt = seqFromFMIndexB xs
  BWT (fmap (\x -> if | isNothing x
                      -> Nothing
                      | otherwise
                      -> Just           $
                         DTE.decodeUtf8 $
                        fromJust x
            ) originalbwtt)

-- | Take a 'FMIndexB' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromFMIndexB :: FMIndexB
                          -> BWT ByteString
bytestringBWTFromFMIndexB (FMIndexB DS.Empty) = BWT DS.Empty
bytestringBWTFromFMIndexB xs              =
  BWT (seqFromFMIndexB xs)

-- | Takes a 'FMIndexB' and returns
-- the original 'Seq' of 'Text's.
textFromFMIndexB :: FMIndexB
                 -> Seq (Maybe Text)
textFromFMIndexB (FMIndexB DS.Empty) = DS.Empty
textFromFMIndexB xs                  = do
  let originalt = seqFromFMIndexB xs
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    DTE.decodeUtf8 $
                    fromJust x
       ) originalt

-- | Takes a 'FMIndexB' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromFMIndexB :: FMIndexB
                       -> Seq (Maybe ByteString)
bytestringFromFMIndexB (FMIndexB DS.Empty) = DS.Empty
bytestringFromFMIndexB xs                  =
  seqFromFMIndexB xs

-- | Takes a 'FMIndexT' and returns
-- the original 'Seq' of 'Text's.
textFromFMIndexT :: FMIndexT
                 -> Seq (Maybe Text)
textFromFMIndexT (FMIndexT DS.Empty) = DS.Empty
textFromFMIndexT xs                  =
  seqFromFMIndexT xs

-- | Takes a 'FMIndexT' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromFMIndexT :: FMIndexT
                       -> Seq (Maybe ByteString)
bytestringFromFMIndexT (FMIndexT DS.Empty) = DS.Empty
bytestringFromFMIndexT xs                  = do
  let originalb = seqFromFMIndexT xs
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    DTE.encodeUtf8 $
                    fromJust x
       ) originalb

{-------------------------}
