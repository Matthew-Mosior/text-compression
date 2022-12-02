{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE Strict            #-}


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
-- = Operation: Count
--
-- The count operation on 'ByteString', 'bytestringFMIndexCount', is implemented using the 'countFMIndexB' function.
--
-- The count operation on 'Text', 'textFMIndexCount', is implemented using the 'countFMIndexT' function.
--
-- = Operation: Locate
--
-- The locate operation on 'ByteString', 'bytestringFMIndexLocate', is implemented using the 'locateFMIndexB' function.
--
-- The locate operation on 'Text', 'textFMIndexLocate', is implemented using the 'locateFMIndexT' function.
--
-- = Internal
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
                      bytestringFromFMIndexT,
                      -- * Count operations
                      bytestringFMIndexCount,
                      textFMIndexCount,
                      -- * Locate operations
                      bytestringFMIndexLocate,
                      bytestringPCFMIndexLocate,
                      textFMIndexLocate,
                      textPCFMIndexLocate
                    ) where

import Data.BWT
import Data.BWT.Internal
import Data.FMIndex.Internal

import Control.Monad()
import Control.Monad.ST as CMST
import Control.Monad.State.Strict()
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC8 (singleton,uncons,unpack)
import Data.Char()
import Data.Foldable()
import Data.Maybe as DMaybe (isNothing,fromJust)
import Data.Sequence as DS (Seq(..),ViewL(..),fromList,index,viewl)
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
bytestringToBWTToFMIndexB xs = bytestringBWTToFMIndexB (createBWTMatrix $ BS.unpack xs)
                                                       (bytestringToBWT xs)

-- | Helper function for converting a 'ByteString'
-- to a 'FMIndexT' via a 'BWT' first.
bytestringToBWTToFMIndexT :: ByteString ->
                             FMIndexT
bytestringToBWTToFMIndexT xs = bytestringBWTToFMIndexT (createBWTMatrix $ BS.unpack xs)
                                                       (bytestringToBWT xs)

-- | Helper function for converting a 'Text'
-- to a 'FMIndexB' via a 'BWT' first.
textToBWTToFMIndexB :: Text ->
                       FMIndexB
textToBWTToFMIndexB xs = textBWTToFMIndexB (createBWTMatrix $ BS.unpack $ DTE.encodeUtf8 xs)
                                           (textToBWT xs)

-- | Helper function for converting a 'Text'
-- to a 'FMIndexT' via a 'BWT' first.
textToBWTToFMIndexT :: Text ->
                       FMIndexT
textToBWTToFMIndexT xs = textBWTToFMIndexT (createBWTMatrix $ BS.unpack $ DTE.encodeUtf8 xs)
                                           (textToBWT xs)

-- | Take a 'BWT' of 'Word8's and generate the
-- FM-index ('FMIndexB').
textBWTToFMIndexB :: BWTMatrix Word8
                  -> TextBWT
                  -> FMIndexB
textBWTToFMIndexB (BWTMatrix DS.Empty) _  = FMIndexB (CcB DS.Empty,OccCKB DS.Empty)
textBWTToFMIndexB bwm                  xs = do
  let occckb = CMST.runST $ seqToOccCKB xss
  let ccb    = CMST.runST $ seqToCcB bwmff
  FMIndexB (CcB ccb,OccCKB occckb)
    where
      bwmf  = fmap (\x -> case viewl x of
                            EmptyL       -> Nothing
                            (xh DS.:< _) -> xh
                   ) $
              (\(BWTMatrix m) -> m) bwm
      bwmff = fmap (\x -> if | isNothing x
                             -> Nothing
                             | otherwise
                             -> Just         $
                                BS.singleton $
                                fromJust x
                   ) bwmf
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
bytestringBWTToFMIndexB :: BWTMatrix Word8
                        -> BWT Word8
                        -> FMIndexB
bytestringBWTToFMIndexB (BWTMatrix DS.Empty) _  = FMIndexB (CcB DS.Empty,OccCKB DS.Empty) 
bytestringBWTToFMIndexB bwm                  xs = do
  let occckb = CMST.runST $ seqToOccCKB xss
  let ccb    = CMST.runST $ seqToCcB bwmff 
  FMIndexB (CcB ccb,OccCKB occckb)
    where
      bwmf  = fmap (\x -> case viewl x of
                            EmptyL       -> Nothing
                            (xh DS.:< _) -> xh
                   ) $
              (\(BWTMatrix m) -> m) bwm
      bwmff = fmap (\x -> if | isNothing x
                             -> Nothing
                             | otherwise
                             -> Just         $
                                BS.singleton $
                                fromJust x
                   ) bwmf
      xss   = fmap (\x -> if | isNothing x
                             -> Nothing
                             | otherwise
                             -> Just         $
                                BS.singleton $
                                fromJust x
                   )
              ((\(BWT t) -> t) xs)

-- | Take a 'BWT' of 'Word8's and generate the
-- FM-index ('FMIndexB').
textBWTToFMIndexT :: BWTMatrix Word8
                  -> TextBWT
                  -> FMIndexT
textBWTToFMIndexT (BWTMatrix DS.Empty) _  = FMIndexT (CcT DS.Empty,OccCKT DS.Empty)
textBWTToFMIndexT bwm                  xs = do
  let occckt = CMST.runST $ seqToOccCKT xss
  let cct    = CMST.runST $ seqToCcT bwmff
  FMIndexT (CcT cct,OccCKT occckt)
    where
      bwmf  = fmap (\x -> case viewl x of
                            EmptyL       -> Nothing
                            (xh DS.:< _) -> xh
                   ) $
              (\(BWTMatrix m) -> m) bwm
      bwmff = fmap (\x -> if | isNothing x
                             -> Nothing
                             | otherwise
                             -> Just           $
                                DTE.decodeUtf8 $
                                BS.singleton   $
                                fromJust x
                   ) bwmf
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
bytestringBWTToFMIndexT :: BWTMatrix Word8
                        -> BWT Word8
                        -> FMIndexT
bytestringBWTToFMIndexT (BWTMatrix DS.Empty) _  = FMIndexT (CcT DS.Empty,OccCKT DS.Empty)
bytestringBWTToFMIndexT bwm                  xs = do
  let occckt = CMST.runST $ seqToOccCKT xss
  let cct    = CMST.runST $ seqToCcT bwmff
  FMIndexT (CcT cct,OccCKT occckt)
    where
      bwmf  = fmap (\x -> case viewl x of
                            EmptyL       -> Nothing
                            (xh DS.:< _) -> xh
                   ) $
              (\(BWTMatrix m) -> m) bwm
      bwmff = fmap (\x -> if | isNothing x
                             -> Nothing
                             | otherwise
                             -> Just           $
                                DTE.decodeUtf8 $
                                BS.singleton   $
                                fromJust x
                   ) bwmf
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
textToFMIndexB :: BWTMatrix Text
               -> Seq (Maybe Text)
               -> FMIndexB
textToFMIndexB (BWTMatrix DS.Empty) _        = FMIndexB (CcB DS.Empty,OccCKB DS.Empty) 
textToFMIndexB _                    DS.Empty = FMIndexB (CcB DS.Empty,OccCKB DS.Empty)
textToFMIndexB bwm                  xs       = do
  let occckb = CMST.runST $ seqToOccCKB xss
  let ccb    = CMST.runST $ seqToCcB bwmff
  FMIndexB (CcB ccb,OccCKB occckb)
    where
      bwmf  = fmap (\x -> case viewl x of
                            EmptyL       -> Nothing
                            (xh DS.:< _) -> xh
                   ) $
              (\(BWTMatrix m) -> m) bwm
      bwmff = fmap (\x -> if | isNothing x
                             -> Nothing
                             | otherwise
                             -> Just           $
                                DTE.encodeUtf8 $
                                fromJust x
                   ) bwmf
      xss   = fmap (\x -> if | isNothing x
                             -> Nothing
                             | otherwise
                             -> Just           $
                                DTE.encodeUtf8 $
                                fromJust x
                   )
              xs

-- | Takes a 'Seq' of 'ByteString's and returns the FM-index ('FMIndexB').
bytestringToFMIndexB :: BWTMatrix ByteString
                     -> Seq (Maybe ByteString)
                     -> FMIndexB
bytestringToFMIndexB (BWTMatrix DS.Empty) _        = FMIndexB (CcB DS.Empty,OccCKB DS.Empty)
bytestringToFMIndexB _                    DS.Empty = FMIndexB (CcB DS.Empty,OccCKB DS.Empty)
bytestringToFMIndexB bwm                  xs       = do
  let occckb = CMST.runST $ seqToOccCKB xs
  let ccb    = CMST.runST $ seqToCcB bwmf
  FMIndexB (CcB ccb,OccCKB occckb)
    where
      bwmf  = fmap (\x -> case viewl x of
                            EmptyL       -> Nothing
                            (xh DS.:< _) -> xh
                   ) $
              (\(BWTMatrix m) -> m) bwm

-- | Takes a 'Text' and returns the FM-index ('FMIndexT').
textToFMIndexT :: BWTMatrix Text
               -> Seq (Maybe Text)
               -> FMIndexT
textToFMIndexT (BWTMatrix DS.Empty) _        = FMIndexT (CcT DS.Empty,OccCKT DS.Empty)
textToFMIndexT _                    DS.Empty = FMIndexT (CcT DS.Empty,OccCKT DS.Empty) 
textToFMIndexT bwm                  xs       = do
  let occckt = CMST.runST $ seqToOccCKT xs
  let cct    = CMST.runST $ seqToCcT bwmf
  FMIndexT (CcT cct,OccCKT occckt)
    where
      bwmf  = fmap (\x -> case viewl x of
                            EmptyL       -> Nothing
                            (xh DS.:< _) -> xh
                   ) $
              (\(BWTMatrix m) -> m) bwm

-- | Takes a 'ByteString' and returns the FM-index ('FMIndexT').
bytestringToFMIndexT :: BWTMatrix ByteString
                     -> Seq (Maybe ByteString)
                     -> FMIndexT
bytestringToFMIndexT (BWTMatrix DS.Empty) _        = FMIndexT (CcT DS.Empty,OccCKT DS.Empty) 
bytestringToFMIndexT _                    DS.Empty = FMIndexT (CcT DS.Empty,OccCKT DS.Empty) 
bytestringToFMIndexT bwm                  xs       = do
  let occckt = CMST.runST $ seqToOccCKT xss
  let cct    = CMST.runST $ seqToCcT bwmff
  FMIndexT (CcT cct,OccCKT occckt)
    where
      bwmf  = fmap (\x -> case viewl x of
                            EmptyL       -> Nothing
                            (xh DS.:< _) -> xh
                   ) $
              (\(BWTMatrix m) -> m) bwm
      bwmff = fmap (\x -> if | isNothing x
                             -> Nothing
                             | otherwise
                             -> Just           $
                                DTE.decodeUtf8 $ 
                                fromJust x
                   ) bwmf
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
textBWTFromFMIndexT (FMIndexT (CcT DS.Empty,_))    = BWT DS.Empty
textBWTFromFMIndexT (FMIndexT (_,OccCKT DS.Empty)) = BWT DS.Empty
textBWTFromFMIndexT xs                             =
  BWT (seqFromFMIndexT xs)

-- | Takes a 'FMIndexT' and returns
-- the 'BWT' of 'ByteString's.
bytestringBWTFromFMIndexT :: FMIndexT
                          -> BWT ByteString
bytestringBWTFromFMIndexT (FMIndexT (CcT DS.Empty,_))    = BWT DS.Empty
bytestringBWTFromFMIndexT (FMIndexT (_,OccCKT DS.Empty)) = BWT DS.Empty 
bytestringBWTFromFMIndexT xs                             = do
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
textBWTFromFMIndexB (FMIndexB (CcB DS.Empty,_))    = BWT DS.Empty
textBWTFromFMIndexB (FMIndexB (_,OccCKB DS.Empty)) = BWT DS.Empty
textBWTFromFMIndexB xs                             = do
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
bytestringBWTFromFMIndexB (FMIndexB (CcB DS.Empty,_))    = BWT DS.Empty
bytestringBWTFromFMIndexB (FMIndexB (_,OccCKB DS.Empty)) = BWT DS.Empty 
bytestringBWTFromFMIndexB xs                             =
  BWT (seqFromFMIndexB xs)

-- | Takes a 'FMIndexB' and returns
-- the original 'Seq' of 'Text's.
textFromFMIndexB :: FMIndexB
                 -> Seq (Maybe Text)
textFromFMIndexB (FMIndexB (CcB DS.Empty,_))    = DS.Empty
textFromFMIndexB (FMIndexB (_,OccCKB DS.Empty)) = DS.Empty
textFromFMIndexB xs                             = do
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
bytestringFromFMIndexB (FMIndexB (CcB DS.Empty,_))    = DS.Empty
bytestringFromFMIndexB (FMIndexB (_,OccCKB DS.Empty)) = DS.Empty 
bytestringFromFMIndexB xs                             =
  seqFromFMIndexB xs

-- | Takes a 'FMIndexT' and returns
-- the original 'Seq' of 'Text's.
textFromFMIndexT :: FMIndexT
                 -> Seq (Maybe Text)
textFromFMIndexT (FMIndexT (CcT DS.Empty,_))    = DS.Empty
textFromFMIndexT (FMIndexT (_,OccCKT DS.Empty)) = DS.Empty
textFromFMIndexT xs                             =
  seqFromFMIndexT xs

-- | Takes a 'FMIndexT' and returns
-- the original 'Seq' of 'ByteString's.
bytestringFromFMIndexT :: FMIndexT
                       -> Seq (Maybe ByteString)
bytestringFromFMIndexT (FMIndexT (CcT DS.Empty,_))    = DS.Empty
bytestringFromFMIndexT (FMIndexT (_,OccCKT DS.Empty)) = DS.Empty
bytestringFromFMIndexT xs                             = do
  let originalb = seqFromFMIndexT xs
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    DTE.encodeUtf8 $
                    fromJust x
       ) originalb

{-------------------------}


{-Count operations.-}

-- | Takes a pattern ('ByteString')
-- and an input 'ByteString'
-- and returns the number of occurences of the pattern
-- in the input 'ByteString'.
bytestringFMIndexCount :: ByteString
                       -> ByteString
                       -> CIntB
bytestringFMIndexCount (BSC8.uncons -> Nothing) _                        = Nothing
bytestringFMIndexCount _                        (BSC8.uncons -> Nothing) = Nothing
bytestringFMIndexCount pat                      input                    = do
  let bytestringfmindex = bytestringToBWTToFMIndexB input
  let patternf          = fmap (BSC8.singleton) $
                          DS.fromList           $
                          BSC8.unpack pat
  runST $ countFMIndexB patternf
                        bytestringfmindex

-- | Takes a pattern ('Text')
-- and an input 'Text'
-- and returns the number of occurences of the pattern
-- in the input 'Text'.
textFMIndexCount :: Text
                 -> Text
                 -> CIntT
textFMIndexCount ""  _     = Nothing
textFMIndexCount _   ""    = Nothing
textFMIndexCount pat input = do
  let textfmindex = textToBWTToFMIndexT input
  let patternf    = fmap (DText.singleton) $
                    DS.fromList            $
                    DText.unpack pat
  runST $ countFMIndexT patternf
                        textfmindex

{-------------------}


{-Locate operations.-}

-- | Takes a pattern ('ByteString')
-- and an input 'ByteString'
-- and returns the indexe(s) of occurences of the pattern
-- in the input 'ByteString'.
-- The output is not sorted.
bytestringFMIndexLocate :: ByteString
                        -> ByteString
                        -> LIntB
bytestringFMIndexLocate (BSC8.uncons -> Nothing) _                        = DS.Empty
bytestringFMIndexLocate _                        (BSC8.uncons -> Nothing) = DS.Empty
bytestringFMIndexLocate pat                      input                    = do
  let bytestringsa      = createSuffixArray   $
                          fmap (BS.singleton) $
                          DS.fromList         $ 
                          BS.unpack input
  let bfmindex          = bytestringToBWTToFMIndexB input
  let patternf          = fmap (BSC8.singleton) $
                          DS.fromList           $
                          BSC8.unpack pat
  let indices           = runST $ locateFMIndexB patternf
                                                 bfmindex
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    suffixstartpos $
                    DS.index bytestringsa ((fromJust x) - 1)
       ) indices

-- | Takes a pattern ('ByteString'),
-- an input ('ByteString')
-- and a pre-computed 'FMIndexB' of the input 'ByteString'
-- and returns the indexe(s) of occurences of the pattern
-- using the pre-computed 'FMIndexB'
-- in the input 'ByteString'.
-- The output is not sorted.
bytestringPCFMIndexLocate :: ByteString
                          -> ByteString
                          -> FMIndexB
                          -> LIntB
bytestringPCFMIndexLocate (BSC8.uncons -> Nothing) _                        _        = DS.Empty
bytestringPCFMIndexLocate _                        (BSC8.uncons -> Nothing) _        = DS.Empty
bytestringPCFMIndexLocate pat                      input                    bfmindex = do
  let bytestringsa      = createSuffixArray   $
                          fmap (BS.singleton) $
                          DS.fromList         $
                          BS.unpack input
  let patternf          = fmap (BSC8.singleton) $
                          DS.fromList           $
                          BSC8.unpack pat
  let indices           = runST $ locateFMIndexB patternf
                                                 bfmindex
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    suffixstartpos $
                    DS.index bytestringsa ((fromJust x) - 1)
       ) indices

-- | Takes a pattern ('Text')
-- and an input 'Text'
-- and returns the indexe(s) of occurences of the pattern
-- in the input 'Text'.
-- The output is not sorted.
textFMIndexLocate :: Text
                  -> Text
                  -> LIntT
textFMIndexLocate ""  _     = DS.Empty
textFMIndexLocate _   ""    = DS.Empty
textFMIndexLocate pat input = do
  let textsa      = createSuffixArray      $
                    fmap (DText.singleton) $
                    DS.fromList            $
                    DText.unpack input
  let tfmindex    = textToBWTToFMIndexT input
  let patternf    = fmap (DText.singleton) $
                    DS.fromList            $
                    DText.unpack pat
  let indices     = runST $ locateFMIndexT patternf
                                           tfmindex
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    suffixstartpos $
                    DS.index textsa ((fromJust x) - 1)
       ) indices

-- | Takes a pattern ('Text'),
-- an input ('Text')
-- and a pre-computed 'FMIndexT' of the input 'Text
-- and returns the indexe(s) of occurences of the pattern
-- using the pre-computed 'FMIndexT'
-- in the input 'Text'.
-- The output is not sorted.
textPCFMIndexLocate :: Text
                    -> Text
                    -> FMIndexT
                    -> LIntT
textPCFMIndexLocate ""  _     _        = DS.Empty
textPCFMIndexLocate _   ""    _        = DS.Empty
textPCFMIndexLocate pat input tfmindex = do
  let textsa      = createSuffixArray      $
                    fmap (DText.singleton) $
                    DS.fromList            $
                    DText.unpack input
  let patternf    = fmap (DText.singleton) $
                    DS.fromList            $
                    DText.unpack pat
  let indices     = runST $ locateFMIndexT patternf
                                           tfmindex
  fmap (\x -> if | isNothing x
                 -> Nothing
                 | otherwise
                 -> Just           $
                    suffixstartpos $
                    DS.index textsa ((fromJust x) - 1)
       ) indices

{--------------------}
