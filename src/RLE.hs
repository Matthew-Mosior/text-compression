{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE Strict            #-}


-- |
-- Module      :  Data.RLE
-- Copyright   :  (c) Matthew Mosior 2022
-- License     :  BSD-style
-- Maintainer  :  mattm.github@gmail.com
-- Portability :  portable
--
-- = Run-length encoding (RLE)


module Data.RLE where

import Data.BWT
import Data.BWT.Internal 
import Data.RLE.Internal

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


{-toRLE Function(s)-}

-- | Helper function for converting a 'ByteString'
-- to a 'RLEB' via a 'BWT' first.
bytestringToBWTToRLEB :: ByteString ->
                         RLEB
bytestringToBWTToRLEB = bytestringBWTToRLEB . bytestringToBWT

-- | Helper function for converting a 'ByteString'
-- to a 'RLET' via a 'BWT' first.
bytestringToBWTToRLET :: ByteString ->
                         RLET
bytestringToBWTToRLET = bytestringBWTToRLET . bytestringToBWT

-- | Helper function for converting a 'Text'
-- to a 'RLEB' via a 'BWT' first.
textToBWTToRLEB :: Text ->
                   RLEB
textToBWTToRLEB = textBWTToRLEB . textToBWT

-- | Helper function for converting a 'Text'
-- to a 'RLET' via a 'BWT' first.
textToBWTToRLET :: Text ->
                   RLET
textToBWTToRLET = textBWTToRLET . textToBWT

-- | Take a 'BWT' of 'Word8's and generate the
-- Run-length encoding ('RLEB').
textBWTToRLEB :: TextBWT
              -> RLEB
textBWTToRLEB xs =
  RLEB (CMST.runST $ seqToRLEB xss)
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
-- Run-length encoding ('RLEB').
bytestringBWTToRLEB :: BWT Word8
                    -> RLEB
bytestringBWTToRLEB (BWT DS.Empty) = RLEB DS.Empty
bytestringBWTToRLEB xs             =
  RLEB (CMST.runST $ seqToRLEB xss)
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
-- Run-length encoding ('RLEB').
textBWTToRLET :: TextBWT
              -> RLET
textBWTToRLET xs =
  RLET (CMST.runST $ seqToRLET xss)
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
-- Run-length encoding ('RLET').
bytestringBWTToRLET :: BWT Word8
                    -> RLET
bytestringBWTToRLET (BWT DS.Empty) = RLET DS.Empty
bytestringBWTToRLET xs             =
  RLET (CMST.runST $ seqToRLET xss)
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

-- | Takes a 'Text' and returns the Run-length encoding ('RLEB').
textToRLEB :: Seq (Maybe Text)
           -> RLEB
textToRLEB DS.Empty = RLEB DS.Empty
textToRLEB xs       = 
  RLEB (CMST.runST $ seqToRLEB xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just            $
                               DTE.encodeUtf8 $
                               fromJust x
                 )
            xs

-- | Takes a 'Seq' of 'ByteString's and returns the Run-length encoding ('RLEB').
bytestringToRLEB :: Seq (Maybe ByteString)
                 -> RLEB
bytestringToRLEB DS.Empty = RLEB DS.Empty
bytestringToRLEB xs       =
 RLEB (CMST.runST $ seqToRLEB xs)

-- | Takes a 'Text' and returns the Run-length encoding (RLE).
textToRLET :: Seq (Maybe Text)
           -> RLET
textToRLET DS.Empty = RLET DS.Empty
textToRLET xs       =
  RLET (CMST.runST $ seqToRLET xs)

-- | Takes a 'ByteString' and returns the Run-length encoding (RLE).
bytestringToRLET :: Seq (Maybe ByteString)
                 -> RLET
bytestringToRLET DS.Empty = RLET DS.Empty
bytestringToRLET xs       =
  RLET (CMST.runST $ seqToRLET xss)
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


{-fromRLE function(s)-}

-- | Helper function for converting a 'BWT'ed 'RLEB'
-- back to the original 'ByteString'.
bytestringFromBWTFromRLEB :: RLEB 
                          -> ByteString
bytestringFromBWTFromRLEB = bytestringFromByteStringBWT . bytestringBWTFromRLEB

-- | Helper function for converting a 'BWT'ed 'RLET'
-- back to the original 'ByteString'.
bytestringFromBWTFromRLET :: RLET
                          -> ByteString
bytestringFromBWTFromRLET xs = bytestringFromByteStringBWT $
                               BWT                         $
                               fmap (\x -> if | isNothing x
                                              -> Nothing
                                              | otherwise
                                              -> Just           $
                                                 DTE.encodeUtf8 $
                                                 fromJust x
                                    )
                                                           $
                            ((\(BWT t) -> t) (textBWTFromRLET xs))

-- | Helper function for converting a 'BWT'ed 'RLEB'
-- back to the original 'Text'.
textFromBWTFromRLEB :: RLEB
                    -> Text
textFromBWTFromRLEB = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromRLEB 

-- | Helper function for converting a 'BWT'ed 'RLET'
-- back to the original 'Text'.
textFromBWTFromRLET :: RLET
                    -> Text
textFromBWTFromRLET = DTE.decodeUtf8 . bytestringFromByteStringBWT . bytestringBWTFromRLET

-- | Takes a 'RLET' and returns
-- the 'BWT' of 'Text's.
textBWTFromRLET :: RLET
                -> BWT Text
textBWTFromRLET (RLET DS.Empty) = BWT DS.Empty
textBWTFromRLET xs              = 
  BWT (CMST.runST $ seqFromRLET xs)

-- | Takes a 'RLET' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromRLET :: RLET
                      -> BWT ByteString
bytestringBWTFromRLET (RLET DS.Empty) = BWT DS.Empty
bytestringBWTFromRLET xs              = do
  let originalbwtb = CMST.runST $ seqFromRLET xs
  BWT (fmap (\x -> if | isNothing x
                      -> Nothing
                      | otherwise
                      -> Just           $
                         DTE.encodeUtf8 $
                        fromJust x 
            ) originalbwtb)

-- | Takes a 'RLEB' and returns
-- the 'BWT' of 'Text's.
textBWTFromRLEB :: RLEB
                -> BWT Text
textBWTFromRLEB (RLEB DS.Empty) = BWT DS.Empty
textBWTFromRLEB xs              = do
  let originalbwtt = CMST.runST $ seqFromRLEB xs
  BWT (fmap (\x -> if | isNothing x
                      -> Nothing
                      | otherwise
                      -> Just           $
                         DTE.decodeUtf8 $
                        fromJust x
            ) originalbwtt)

-- | Take a 'RLEB' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromRLEB :: RLEB 
                      -> BWT ByteString
bytestringBWTFromRLEB (RLEB DS.Empty) = BWT DS.Empty
bytestringBWTFromRLEB xs              =
  BWT (CMST.runST $ seqFromRLEB xs)

-- | Takes a 'RLEB' and returns
-- the original 'Seq' of 'Text's.
textFromRLEB :: RLEB
             -> Seq (Maybe Text)
textFromRLEB (RLEB DS.Empty) = DS.Empty
textFromRLEB xs              = do
  let originalt = CMST.runST $ seqFromRLEB xs
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    DTE.decodeUtf8 $
                    fromJust x
       ) originalt

-- | Takes a 'RLEB' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromRLEB :: RLEB
                   -> Seq (Maybe ByteString)
bytestringFromRLEB (RLEB DS.Empty) = DS.Empty
bytestringFromRLEB xs              = do
  CMST.runST $ seqFromRLEB xs

-- | Takes a 'RLET' and returns
-- the original 'Seq' of 'Text's.
textFromRLET :: RLET
             -> Seq (Maybe Text)
textFromRLET (RLET DS.Empty) = DS.Empty
textFromRLET xs              = do
  CMST.runST $ seqFromRLET xs

-- | Takes a 'RLET' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromRLET :: RLET
                   -> Seq (Maybe ByteString)
bytestringFromRLET (RLET DS.Empty) = DS.Empty
bytestringFromRLET xs              = do
  let originalb = CMST.runST $ seqFromRLET xs
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $ 
                    DTE.encodeUtf8 $
                    fromJust x
       ) originalb

{---------------------}
