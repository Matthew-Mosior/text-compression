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
-- a 'MTF'.
--
-- To do this, users can use the 'bytestringToBWTToMTFB' and 'bytestringToBWTToMTFT' functions,
-- as well as the 'textToBWTToMTFB' and 'textToBWTToMTFT' functions.
--
-- The base functions for 'ByteString', 'bytestringToMTFB' and 'bytestringToMTFT' can be used to
-- convert a 'Seq' ('Maybe' 'ByteString') to a 'MTF', respectively.
--
-- Likewise, the base functions for 'Text', 'textToMTFB' and 'textToMTFT' can be used to
-- convert a 'Seq' ('Maybe' 'Text') to a 'MTF' respectively.
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

import Data.ByteString as BS
import Data.Sequence as DS (Seq(..))
import Data.Text as DText
import Data.Text.Encoding as DTE (decodeUtf8,encodeUtf8)
import Data.Word (Word8)
import Prelude as P
import Test.HUnit


{-toMTF Function(s)-}

-- | Helper function for converting a 'ByteString'
-- to a 'MTF' via a 'BWT' first.
bytestringToBWTToMTFB :: ByteString ->
                         MTF ByteString
bytestringToBWTToMTFB = bytestringBWTToMTFB . bytestringToBWT

-- | Helper function for converting a 'ByteString'
-- to a 'MTF' via a 'BWT' first.
bytestringToBWTToMTFT :: ByteString ->
                         MTF Text
bytestringToBWTToMTFT = bytestringBWTToMTFT . bytestringToBWT

-- | Helper function for converting a 'Text'
-- to a 'MTF' via a 'BWT' first.
textToBWTToMTFB :: Text ->
                   MTF ByteString
textToBWTToMTFB = textBWTToMTFB . textToBWT

-- | Helper function for converting a 'Text'
-- to a 'MTF' via a 'BWT' first.
textToBWTToMTFT :: Text ->
                   MTF Text
textToBWTToMTFT = textBWTToMTFT . textToBWT

-- | Take a 'BWT' of 'Word8's and generate the
-- Move-to-front transform ('MTF').
textBWTToMTFB :: TextBWT
              -> MTF ByteString
textBWTToMTFB xs =
  MTF (seqToMTF xss)
    where
      xss = fmap (fmap BS.singleton)
            ((\(BWT t) -> t) $
            ((\(TextBWT t) -> t) xs))

-- | Take a 'BWT' of 'Word8's and generate the
-- Move-to-front transform ('MTF').
bytestringBWTToMTFB :: BWT Word8
                    -> MTF ByteString
bytestringBWTToMTFB xs =
  MTF (seqToMTF xss)
    where
      xss = fmap (fmap BS.singleton) ((\(BWT t) -> t) xs)

-- | Take a 'BWT' of 'Word8's and generate the
-- Move-to-front transform ('MTF').
textBWTToMTFT :: TextBWT
              -> MTF Text
textBWTToMTFT xs =
  MTF (seqToMTF xss)
    where
      xss = fmap (fmap (DTE.decodeUtf8 . BS.singleton))
            ((\(BWT t) -> t) $
            ((\(TextBWT t) -> t) xs))

-- | Take a 'BWT' of 'Word8's and generate the
-- Move-to-front transform ('MTF').
bytestringBWTToMTFT :: BWT Word8
                    -> MTF Text
bytestringBWTToMTFT xs =
  MTF (seqToMTF xss)
    where
      xss = fmap (fmap (DTE.decodeUtf8 . BS.singleton))
            ((\(BWT t) -> t) xs)

-- | Takes a 'Text' and returns the Move-to-front transform ('MTF').
textToMTFB :: Seq (Maybe Text)
           -> MTF ByteString
textToMTFB DS.Empty = MTF (DS.Empty,DS.Empty)
textToMTFB xs       =
  MTF (seqToMTF xss)
    where
      xss = fmap (fmap DTE.encodeUtf8) xs

-- | Takes a 'Seq' of 'ByteString's and returns the Move-to-front transform ('MTF').
bytestringToMTFB :: Seq (Maybe ByteString)
                 -> MTF ByteString
bytestringToMTFB DS.Empty = MTF (DS.Empty,DS.Empty)
bytestringToMTFB xs       =
 MTF (seqToMTF xs)

-- | Takes a 'Text' and returns the Move-to-front transform ('MTF').
textToMTFT :: Seq (Maybe Text)
           -> MTF Text
textToMTFT DS.Empty = MTF (DS.Empty,DS.Empty)
textToMTFT xs       =
  MTF (seqToMTF xs)

-- | Takes a 'ByteString' and returns the Move-to-front transform ('MTF').
bytestringToMTFT :: Seq (Maybe ByteString)
                 -> MTF Text
bytestringToMTFT DS.Empty = MTF (DS.Empty,DS.Empty)
bytestringToMTFT xs       =
  MTF (seqToMTF xss)
    where
      xss = fmap (fmap DTE.decodeUtf8) xs

{-------------------}


{-fromMTF function(s)-}

-- | Helper function for converting a 'BWT'ed 'MTF'
-- back to the original 'ByteString'.
bytestringFromBWTFromMTFB :: MTF ByteString
                          -> ByteString
bytestringFromBWTFromMTFB = bytestringFromByteStringBWT . bytestringBWTFromMTFB

-- | Helper function for converting a 'BWT'ed 'MTF'
-- back to the original 'ByteString'.
bytestringFromBWTFromMTFT :: MTF Text
                          -> ByteString
bytestringFromBWTFromMTFT xs = bytestringFromByteStringBWT $
                               BWT                         $
                               fmap (fmap DTE.encodeUtf8)  $
                            ((\(BWT t) -> t) (textBWTFromMTFT xs))

-- | Helper function for converting a 'BWT'ed 'MTF'
-- back to the original 'Text'.
textFromBWTFromMTFB :: MTF ByteString
                    -> Text
textFromBWTFromMTFB = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromMTFB

-- | Helper function for converting a 'BWT'ed 'MTF'
-- back to the original 'Text'.
textFromBWTFromMTFT :: MTF Text
                    -> Text
textFromBWTFromMTFT = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromMTFT

-- | Takes a 'MTF' and returns
-- the 'BWT' of 'Text's.
textBWTFromMTFT :: MTF Text
                -> BWT Text
textBWTFromMTFT (MTF (DS.Empty,_)) = BWT DS.Empty
textBWTFromMTFT (MTF (_,DS.Empty)) = BWT DS.Empty
textBWTFromMTFT xs                  =
  BWT (seqFromMTFT xs)

-- | Takes a 'MTF' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromMTFT :: MTF Text
                      -> BWT ByteString
bytestringBWTFromMTFT (MTF (DS.Empty,_)) = BWT DS.Empty
bytestringBWTFromMTFT (MTF (_,DS.Empty)) = BWT DS.Empty
bytestringBWTFromMTFT xs                  = do
  let originalbwtb = seqFromMTFT xs
  BWT (fmap (fmap DTE.encodeUtf8) originalbwtb)

-- | Takes a 'MTF' and returns
-- the 'BWT' of 'Text's.
textBWTFromMTFB :: MTF ByteString
                -> BWT Text
textBWTFromMTFB (MTF (DS.Empty,_)) = BWT DS.Empty
textBWTFromMTFB (MTF (_,DS.Empty)) = BWT DS.Empty
textBWTFromMTFB xs                  = do
  let originalbwtt = seqFromMTFB xs
  BWT (fmap (fmap DTE.decodeUtf8) originalbwtt)

-- | Take a 'MTF' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromMTFB :: MTF ByteString
                      -> BWT ByteString
bytestringBWTFromMTFB (MTF (DS.Empty,_)) = BWT DS.Empty
bytestringBWTFromMTFB (MTF (_,DS.Empty)) = BWT DS.Empty
bytestringBWTFromMTFB xs              =
  BWT (seqFromMTFB xs)

-- | Takes a 'MTF' and returns
-- the original 'Seq' of 'Text's.
textFromMTFB :: MTF ByteString
             -> Seq (Maybe Text)
textFromMTFB (MTF (DS.Empty,_)) = DS.Empty
textFromMTFB (MTF (_,DS.Empty)) = DS.Empty
textFromMTFB xs                  = do
  let originalt = seqFromMTFB xs
  fmap (fmap DTE.decodeUtf8) originalt

-- | Takes a 'MTF' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromMTFB :: MTF ByteString
                   -> Seq (Maybe ByteString)
bytestringFromMTFB (MTF (DS.Empty,_)) = DS.Empty
bytestringFromMTFB (MTF (_,DS.Empty)) = DS.Empty
bytestringFromMTFB xs                  =
  seqFromMTFB xs

-- | Takes a 'MTF' and returns
-- the original 'Seq' of 'Text's.
textFromMTFT :: MTF Text
             -> Seq (Maybe Text)
textFromMTFT (MTF (DS.Empty,_)) = DS.Empty
textFromMTFT (MTF (_,DS.Empty)) = DS.Empty
textFromMTFT xs                  =
  seqFromMTFT xs

-- | Takes a 'MTF' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromMTFT :: MTF Text
                   -> Seq (Maybe ByteString)
bytestringFromMTFT (MTF (DS.Empty,_)) = DS.Empty
bytestringFromMTFT (MTF (_,DS.Empty)) = DS.Empty
bytestringFromMTFT xs                  = do
  let originalb = seqFromMTFT xs
  fmap (fmap DTE.encodeUtf8) originalb

{---------------------}

tests :: Test
tests =
  TestList
  [ TestCase (assertEqual "test 1"
               (MTF ([3,1,2,0,0,3,0,3,0,1],
                      [Just "b",Just "c",Just "a",Nothing]))
               (textToBWTToMTFB "aaabbbccc"))
  , TestCase (assertEqual "test 2"
               "aaabbbccc"
               (textFromBWTFromMTFB
                 (MTF ([3,1,2,0,0,3,0,3,0,1],
                        [Just "b",Just "c",Just "a",Nothing]))))
  ]
