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
import Data.STRef()
import Data.Text as DText 
import Data.Text.Encoding as DTE (decodeUtf8,encodeUtf8)
import Data.Vector as DVB (Vector,empty,map,uncons)
import Data.Vector.Unboxed()
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
  RLEB (CMST.runST $ vecToRLEB xss)
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
bytestringBWTToRLEB (BWT (DVB.uncons -> Nothing)) = RLEB DVB.empty
bytestringBWTToRLEB xs                            =
  RLEB (CMST.runST $ vecToRLEB xss)
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
  RLET (CMST.runST $ vecToRLET xss)
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
bytestringBWTToRLET (BWT (DVB.uncons -> Nothing)) = RLET DVB.empty
bytestringBWTToRLET xs                            =
  RLET (CMST.runST $ vecToRLET xss)
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
textToRLEB :: DVB.Vector (Maybe Text)
           -> RLEB
textToRLEB (DVB.uncons -> Nothing) = RLEB DVB.empty
textToRLEB xs                      = 
  RLEB (CMST.runST $ vecToRLEB xss)
    where
      xss = fmap (\x -> if | isNothing x
                           -> Nothing
                           | otherwise
                           -> Just            $
                               DTE.encodeUtf8 $
                               fromJust x
                 )
            xs

-- | Takes a 'DVB.Vector' of 'ByteString's and returns the Run-length encoding ('RLEB').
bytestringToRLEB :: DVB.Vector (Maybe ByteString)
                 -> RLEB
bytestringToRLEB (DVB.uncons -> Nothing) = RLEB DVB.empty
bytestringToRLEB xs                      =
 RLEB (CMST.runST $ vecToRLEB xs)

-- | Takes a 'Text' and returns the Run-length encoding (RLE).
textToRLET :: DVB.Vector (Maybe Text)
           -> RLET
textToRLET (DVB.uncons -> Nothing) = RLET DVB.empty
textToRLET xs                      =
  RLET (CMST.runST $ vecToRLET xs)

-- | Takes a 'ByteString' and returns the Run-length encoding (RLE).
bytestringToRLET :: DVB.Vector (Maybe ByteString)
                 -> RLET
bytestringToRLET (DVB.uncons -> Nothing) = RLET DVB.empty
bytestringToRLET xs                      =
  RLET (CMST.runST $ vecToRLET xss)
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
bytestringFromBWTFromRLET vs = bytestringFromByteStringBWT $
                               BWT                         $
                               DVB.map (\x -> if | isNothing x
                                                 -> Nothing
                                                 | otherwise
                                                 -> Just           $
                                                    DTE.encodeUtf8 $
                                                    fromJust x
                                       )
                                                           $ 
                               ((\(BWT t) -> t) (textBWTFromRLET vs))

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
textBWTFromRLET (RLET (DVB.uncons -> Nothing)) = BWT DVB.empty
textBWTFromRLET vs              = 
  BWT (CMST.runST $ vecFromRLET vs)

-- | Takes a 'RLET' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromRLET :: RLET
                      -> BWT ByteString
bytestringBWTFromRLET (RLET (DVB.uncons -> Nothing)) = BWT DVB.empty
bytestringBWTFromRLET vs                             = do
  let originalbwtb = CMST.runST $ vecFromRLET vs
  BWT (DVB.map (\x -> if | isNothing x
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
textBWTFromRLEB (RLEB (DVB.uncons -> Nothing)) = BWT DVB.empty
textBWTFromRLEB vs                             = do
  let originalbwtt = CMST.runST $ vecFromRLEB vs
  BWT (DVB.map (\x -> if | isNothing x
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
bytestringBWTFromRLEB (RLEB (DVB.uncons -> Nothing)) = BWT DVB.empty
bytestringBWTFromRLEB vs                             =
  BWT (CMST.runST $ vecFromRLEB vs)

-- | Takes a 'RLEB' and returns
-- the original 'DVB.Vector' of 'Text's.
textFromRLEB :: RLEB
             -> DVB.Vector (Maybe Text)
textFromRLEB (RLEB (DVB.uncons -> Nothing)) = DVB.empty
textFromRLEB vs                             = do
  let originalt = CMST.runST $ vecFromRLEB vs
  DVB.map (\x -> if | isNothing x
                    -> Nothing
                    | otherwise
                    -> Just           $
                       DTE.decodeUtf8 $
                       fromJust x
          ) originalt

-- | Takes a 'RLEB' and returns
-- the original 'DVB.Vector' of 'ByteString's.
bytestringFromRLEB :: RLEB
                   -> DVB.Vector (Maybe ByteString)
bytestringFromRLEB (RLEB (DVB.uncons -> Nothing)) = DVB.empty
bytestringFromRLEB vs                             =
  CMST.runST $ vecFromRLEB vs

-- | Takes a 'RLET' and returns
-- the original 'DVB.Vector' of 'Text's.
textFromRLET :: RLET
             -> DVB.Vector (Maybe Text)
textFromRLET (RLET (DVB.uncons -> Nothing)) = DVB.empty
textFromRLET vs                             =
  CMST.runST $ vecFromRLET vs

-- | Takes a 'RLET' and returns
-- the original 'DVB.Vector' of 'ByteString's.
bytestringFromRLET :: RLET
                   -> DVB.Vector (Maybe ByteString)
bytestringFromRLET (RLET (DVB.uncons -> Nothing)) = DVB.empty
bytestringFromRLET vs                             = do
  let originalb = CMST.runST $ vecFromRLET vs
  DVB.map (\x -> if | isNothing x
                    -> Nothing
                    | otherwise
                    -> Just           $
                       DTE.encodeUtf8 $
                       fromJust x
          ) originalb

{---------------------}
