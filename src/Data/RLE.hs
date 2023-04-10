{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Data.RLE
-- Copyright   :  (c) Matthew Mosior 2022
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Run-length encoding (RLE)
--
-- Users will get the most mileage by first compressing to a 'BWT'
-- on the initial 'ByteString' or 'Text' input before compressing to
-- a 'RLEB' or 'RLET'.
--
-- To do this, users can use the 'bytestringToBWTToRLEB' and 'bytestringToBWTToRLET' functions,
-- as well as the 'textToBWTToRLEB' and 'textToBWTToRLET' functions.
--
-- The base functions for 'ByteString', 'bytestringToRLEB' and 'bytestringToRLET' can be used to
-- convert a 'Seq' ('Maybe' 'ByteString') to a 'RLEB' and 'RLET', respectively.
--
-- Likewise, the base functions for 'Text', 'textToRLEB' and 'textToRLET' can be used to
-- convert a 'Seq' ('Maybe' 'Text') to a 'RLEB' and 'RLET' respectively.
--
-- There are various other lower-level functions for interacting with the RLE implementation on 'ByteString' and 'Text' as well.
--
-- @"Data.RLE.Internal"@ contains efficient and stateful implementations of the RLE and Inverse RLE algorithms.


module Data.RLE ( -- * To RLE functions
                  bytestringToBWTToRLEB,
                  bytestringToBWTToRLET,
                  textToBWTToRLEB,
                  textToBWTToRLET,
                  textBWTToRLEB,
                  bytestringBWTToRLEB,
                  textBWTToRLET,
                  bytestringBWTToRLET,
                  textToRLEB,
                  bytestringToRLEB,
                  textToRLET,
                  bytestringToRLET,
                  -- * From RLE functions
                  bytestringFromBWTFromRLEB,
                  bytestringFromBWTFromRLET,
                  textFromBWTFromRLEB,
                  textFromBWTFromRLET,
                  textBWTFromRLET,
                  bytestringBWTFromRLET,
                  textBWTFromRLEB,
                  bytestringBWTFromRLEB,
                  textFromRLEB,
                  bytestringFromRLEB,
                  textFromRLET,
                  bytestringFromRLET,
                  tests
                ) where

import Data.BWT hiding (tests)
import Data.BWT.Internal
import Data.RLE.Internal

import Data.ByteString as BS
import Data.Sequence as DS (Seq(..))
import Data.Text as DText
import Data.Text.Encoding as DTE (decodeUtf8,encodeUtf8)
import Data.Word (Word8)
import GHC.Exts (fromList)
import Prelude as P
import Test.HUnit


{-toRLE Function(s)-}

-- | Helper function for converting a 'ByteString'
-- to a 'RLEB' via a 'BWT' first.
bytestringToBWTToRLEB :: ByteString
                      -> RLE ByteString
bytestringToBWTToRLEB = bytestringBWTToRLEB . bytestringToBWT

-- | Helper function for converting a 'ByteString'
-- to a 'RLET' via a 'BWT' first.
bytestringToBWTToRLET :: ByteString
                      -> RLE Text
bytestringToBWTToRLET = bytestringBWTToRLET . bytestringToBWT

-- | Helper function for converting a 'Text'
-- to a 'RLEB' via a 'BWT' first.
textToBWTToRLEB :: Text
                -> RLE ByteString
textToBWTToRLEB = textBWTToRLEB . textToBWT

-- | Helper function for converting a 'Text'
-- to a 'RLET' via a 'BWT' first.
textToBWTToRLET :: Text
                -> RLE Text
textToBWTToRLET = textBWTToRLET . textToBWT

-- | Take a 'BWT' of 'Word8's and generate the
-- Run-length encoding ('RLEB').
textBWTToRLEB :: TextBWT
              -> RLE ByteString
textBWTToRLEB xs =
  RLE (seqToRLE xss)
    where
      xss = fmap (fmap BS.singleton)
              ((\(BWT t) -> t) $ ((\(TextBWT t) -> t) xs))

-- | Take a 'BWT' of 'Word8's and generate the
-- Run-length encoding ('RLEB').
bytestringBWTToRLEB :: BWT Word8
                    -> RLE ByteString
bytestringBWTToRLEB (BWT DS.Empty) = RLE DS.Empty
bytestringBWTToRLEB xs             =
  RLE (seqToRLE xss)
    where
      xss = fmap (fmap BS.singleton) ((\(BWT t) -> t) xs)

-- | Take a 'BWT' of 'Word8's and generate the
-- Run-length encoding ('RLEB').
textBWTToRLET :: TextBWT
              -> RLE Text
textBWTToRLET xs =
  RLE (seqToRLE xss)
    where
      xss = fmap (fmap (DTE.decodeUtf8 . BS.singleton))
             ((\(BWT t) -> t) $ ((\(TextBWT t) -> t) xs))

-- | Take a 'BWT' of 'Word8's and generate the
-- Run-length encoding ('RLET').
bytestringBWTToRLET :: BWT Word8
                    -> RLE Text
bytestringBWTToRLET (BWT DS.Empty) = RLE DS.Empty
bytestringBWTToRLET xs             =
  RLE (seqToRLE xss)
    where
      xss = fmap (fmap (DTE.decodeUtf8 . BS.singleton)) ((\(BWT t) -> t) xs)

-- | Takes a 'Text' and returns the Run-length encoding ('RLEB').
textToRLEB :: Seq (Maybe Text)
           -> RLE ByteString
textToRLEB DS.Empty = RLE DS.Empty
textToRLEB xs       =
  RLE (seqToRLE xss)
    where
      xss = fmap (fmap DTE.encodeUtf8) xs

-- | Takes a 'Seq' of 'ByteString's and returns the Run-length encoding ('RLEB').
bytestringToRLEB :: Seq (Maybe ByteString)
                 -> RLE ByteString
bytestringToRLEB DS.Empty = RLE DS.Empty
bytestringToRLEB xs       =
 RLE (seqToRLE xs)

-- | Takes a 'Text' and returns the Run-length encoding (RLE).
textToRLET :: Seq (Maybe Text)
           -> RLE Text
textToRLET DS.Empty = RLE DS.Empty
textToRLET xs       =
  RLE (seqToRLE xs)

-- | Takes a 'ByteString' and returns the Run-length encoding (RLE).
bytestringToRLET :: Seq (Maybe ByteString)
                 -> RLE Text
bytestringToRLET DS.Empty = RLE DS.Empty
bytestringToRLET xs       =
  RLE (seqToRLE xss)
    where
      xss = fmap (fmap DTE.decodeUtf8) xs

{-------------------}


{-fromRLE function(s)-}

-- | Helper function for converting a 'BWT'ed 'RLEB'
-- back to the original 'ByteString'.
bytestringFromBWTFromRLEB :: RLE ByteString
                          -> ByteString
bytestringFromBWTFromRLEB = bytestringFromByteStringBWT . bytestringBWTFromRLEB

-- | Helper function for converting a 'BWT'ed 'RLET'
-- back to the original 'ByteString'.
bytestringFromBWTFromRLET :: RLE Text
                          -> ByteString
bytestringFromBWTFromRLET xs = bytestringFromByteStringBWT $
                               BWT                         $
                               fmap (fmap DTE.encodeUtf8)  $
                            ((\(BWT t) -> t) (textBWTFromRLET xs))

-- | Helper function for converting a 'BWT'ed 'RLEB'
-- back to the original 'Text'.
textFromBWTFromRLEB :: RLE ByteString
                    -> Text
textFromBWTFromRLEB = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromRLEB

-- | Helper function for converting a 'BWT'ed 'RLET'
-- back to the original 'Text'.
textFromBWTFromRLET :: RLE Text
                    -> Text
textFromBWTFromRLET = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromRLET

-- | Takes a 'RLET' and returns
-- the 'BWT' of 'Text's.
textBWTFromRLET :: RLE Text
                -> BWT Text
textBWTFromRLET (RLE DS.Empty) = BWT DS.Empty
textBWTFromRLET xs              =
  BWT (seqFromRLE xs)

-- | Takes a 'RLE' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromRLET :: RLE Text
                      -> BWT ByteString
bytestringBWTFromRLET (RLE DS.Empty) = BWT DS.Empty
bytestringBWTFromRLET xs              = do
  let originalbwtb = seqFromRLE xs
  BWT (fmap (fmap DTE.encodeUtf8) originalbwtb)

-- | Takes a 'RLEB' and returns
-- the 'BWT' of 'Text's.
textBWTFromRLEB :: RLE ByteString
                -> BWT Text
textBWTFromRLEB (RLE DS.Empty) = BWT DS.Empty
textBWTFromRLEB xs              = do
  let originalbwtt = seqFromRLE xs
  BWT (fmap (fmap DTE.decodeUtf8) originalbwtt)

-- | Take a 'RLE' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromRLEB :: RLE ByteString
                      -> BWT ByteString
bytestringBWTFromRLEB (RLE DS.Empty) = BWT DS.Empty
bytestringBWTFromRLEB xs              =
  BWT (seqFromRLE xs)

-- | Takes a 'RLE' and returns
-- the original 'Seq' of 'Text's.
textFromRLEB :: RLE ByteString
             -> Seq (Maybe Text)
textFromRLEB (RLE DS.Empty) = DS.Empty
textFromRLEB xs              = do
  let originalt = seqFromRLE xs
  fmap (fmap DTE.decodeUtf8) originalt

-- | Takes a 'RLE' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromRLEB :: RLE ByteString
                   -> Seq (Maybe ByteString)
bytestringFromRLEB (RLE DS.Empty) = DS.Empty
bytestringFromRLEB xs              =
  seqFromRLE xs

-- | Takes a 'RLE' and returns
-- the original 'Seq' of 'Text's.
textFromRLET :: RLE Text
             -> Seq (Maybe Text)
textFromRLET (RLE DS.Empty) = DS.Empty
textFromRLET xs              =
  seqFromRLE xs

-- | Takes a 'RLE' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromRLET :: RLE Text
                   -> Seq (Maybe ByteString)
bytestringFromRLET (RLE DS.Empty) = DS.Empty
bytestringFromRLET xs              = do
  let originalb = seqFromRLE xs
  fmap (fmap DTE.encodeUtf8) originalb

{---------------------}

tests :: Test
tests =
  TestList
  [ TestCase (assertEqual "test 1"
                (RLE (fromList [Just "1",Just "c",
                                Just "1",Nothing,
                                Just "4",Just "a",
                                Just "3",Just "b",
                                Just "3",Just "c",
                                Just "1",Just "b"]))
                (textToBWTToRLET "aaaabbbbcccc"))
  , TestCase
      (assertEqual "test 2"
        (RLE (fromList [Just "1", Just "U", Just "1", Just "r", Just "1", Just "t", Just "102",Just "z", Just "2", Just "4",
                         Just "2", Just "2", Just "42",Just "z", Just "2", Just "1", Just "5",  Just "z", Just "2", Just "1",
                         Just "8", Just "z", Just "2", Just "a", Just "2", Just "U", Just "7",  Just "z", Just "2", Just "f",
                         Just "2", Just "z", Just "2", Just "a", Just "2", Just "e", Just "2",  Just "z", Just "1", Just "4",
                         Just "2", Just "f", Just "3", Just "3", Just "1", Just "1", Just "1",  Just "4", Just "15",Just "0",
                         Just "2", Just "1", Just "3", Just "0", Just "2", Just "1", Just "1",  Just "0", Just "1", Just "1",
                         Just "36",Just "0", Just "2", Just "c", Just "8", Just "0", Just "1",  Just "1", Just "1", Just "5",
                         Just "1", Just "2", Just "1", Just "1", Just "1", Just "4", Just "41", Just "0", Just "2", Just "2",
                         Just "2", Just "a", Just "2", Just "0", Just "2", Just "f", Just "2",  Just "5", Just "1", Just "2",
                         Just "9", Just "0", Just "2", Just "d", Just "6", Just "0", Just "3",  Just "2", Just "2", Just "1",
                         Just "1", Just "4", Just "1", Just "2", Just "1", Just "0", Just "2",  Just "9", Just "2", Just "3",
                         Just "2", Just "8", Just "6", Just "0", Just "2", Just "9", Just "2",  Just "0", Just "2", Just "1",
                         Just "2", Just "7", Just "1", Just "e", Just "2", Just "0", Just "1",  Nothing,  Just "2", Just "0",
                         Just "2", Just "b", Just "2", Just "f", Just "2", Just "0", Just "2",  Just "z", Just "1", Just "d",
                         Just "1", Just "-", Just "1", Just "u", Just "2", Just "z", Just "1",  Just "t", Just "1", Just "m",
                         Just "1", Just "o", Just "1", Just "n", Just "1", Just "i", Just "1",  Just "o", Just "30",Just "U",
                         Just "1", Just "-", Just "95",Just "U", Just "2", Just "n", Just "2",  Just "3", Just "31",Just "U",
                         Just "2", Just "h", Just "9", Just "U"]))
        (textToBWTToRLEB "editor-mount-z0Uz0Uz0Uz0Uz0Uz0Uz0Uz5Uz0Uz0Uz0Uz2Uz3Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz1Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz1Uz0Uz0Uz0Uz2Uz1Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz1cUz0Uz0Uz0Uz1Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uznz0e9Uz7dUz20Uz0ffUz11Uz40Uz0a3Uz9aUz0bfU3z0f5Uz12Uz0a8Uzhz4Uz0Uz0Uz0Uz2Uz4Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz2Uz0Uz0Uz0Uz2Uz3Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz1Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz1Uz0Uz0Uz0Uz2Uz3Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz1Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz1cUz0Uz0Uz0Uz1Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uz0Uznz0e9Uz7dUz20Uz0ffUz11Uz40Uz0a3Uz9aUz0bfU3z0f5Uz12Uz0a8Uzhz4Uz4U"))]
