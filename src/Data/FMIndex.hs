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
-- The count operation is supported by both serial, 'bytestringFMIndexCountS' and 'textFMIndexCountS'
-- and parallel, 'bytestringFMIndexCountP' and 'textFMIndexCountP' , implementations.
--
-- The count operations on 'ByteString', 'bytestringFMIndexCountS' and 'bytestringFMIndexCountP', are implemented using the 'countFMIndexB' function.
--
-- The count operations on 'Text', 'textFMIndexCountS' and 'textFMIndexCountP', are implemented using the 'countFMIndexT' function.
--
-- = Operation: Locate
--
-- The locate operation is supported by both serial, 'bytestringFMIndexLocateS' and 'textFMIndexLocateS'
-- and parallel, 'bytestringFMIndexLocateP' and 'textFMIndexLocateP' , implementations.
--
-- The locate operations on 'ByteString', 'bytestringFMIndexLocateS' and 'bytestringFMIndexLocateP', are implemented using the 'locateFMIndexB' function.
--
-- The locate operations on 'Text', 'textFMIndexLocateS' and 'textFMIndexLocateP', are implemented using the 'locateFMIndexT' function.
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
                      bytestringFMIndexCountS,
                      textFMIndexCountS,
                      bytestringFMIndexCountP,
                      textFMIndexCountP,
                      -- * Locate operations
                      bytestringFMIndexLocateS,
                      textFMIndexLocateS,
                      bytestringFMIndexLocateP,
                      textFMIndexLocateP
                    ) where

import Data.BWT
import Data.BWT.Internal
import Data.FMIndex.Internal

import Control.Monad()
import Control.Monad.ST as CMST
import Control.Monad.State.Strict()
import Control.Parallel.Strategies as CPS
import Data.ByteString as BS
import Data.ByteString.Char8 as BSC8 (singleton,uncons,unpack)
import Data.Char()
import Data.Foldable()
import Data.Maybe as DMaybe (isNothing,fromJust)
import Data.Sequence as DS (Seq(..),ViewL(..),fromList,index,viewl,(<|))
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

-- | Takes a list of pattern(s) of 'ByteString's
-- and an input 'ByteString'
-- and returns the number of occurences of the pattern(s)
-- in the input 'ByteString'.
bytestringFMIndexCountS :: [ByteString]
                        -> ByteString
                        -> Seq (ByteString,CIntB)
bytestringFMIndexCountS []      _                        = DS.Empty
bytestringFMIndexCountS _       (BSC8.uncons -> Nothing) = DS.Empty
bytestringFMIndexCountS allpats input                    = do
  let bfmindex = bytestringToBWTToFMIndexB input
  iBFMC allpats
        bfmindex
    where
      iBFMC []                      _    = DS.Empty
      iBFMC (currentpat:restofpats) bfmi = do 
        let patternf          = fmap (BSC8.singleton) $
                                DS.fromList           $
                                BSC8.unpack currentpat
        let countf            = runST $ countFMIndexB patternf
                                                      bfmi
        (currentpat,countf) DS.<| (iBFMC restofpats bfmi)

-- | Takes a list of pattern(s) of 'Text's
-- and an input 'Text'
-- and returns the number of occurences of the pattern(s)
-- in the input 'Text'.
textFMIndexCountS :: [Text]
                  -> Text
                  -> Seq (Text,CIntT)
textFMIndexCountS []      _     = DS.Empty 
textFMIndexCountS _       ""    = DS.Empty
textFMIndexCountS allpats input = do
  let tfmindex = textToBWTToFMIndexT input
  iTFMC allpats
        tfmindex
    where
      iTFMC []                      _    = DS.Empty
      iTFMC (currentpat:restofpats) tfmi = do
        let patternf    = fmap (DText.singleton) $
                          DS.fromList            $
                          DText.unpack currentpat
        let countf      = runST $ countFMIndexT patternf
                                                tfmi
        (currentpat,countf) DS.<| (iTFMC restofpats tfmi)

-- | Takes a list of pattern(s) of 'ByteString's
-- and an input 'ByteString'
-- and returns the number of occurences of the pattern(s)
-- in the input 'ByteString'.
-- Parallelized over all available cores.
bytestringFMIndexCountP :: [ByteString]
                        -> ByteString
                        -> Seq (ByteString,CIntB)
bytestringFMIndexCountP []      _                        = DS.Empty
bytestringFMIndexCountP _       (BSC8.uncons -> Nothing) = DS.Empty
bytestringFMIndexCountP allpats input                    = do
  let bfmindex        = bytestringToBWTToFMIndexB input
  (iBFMC allpats bfmindex) `CPS.using` (CPS.parTraversable CPS.rseq)
    where
      iBFMC []                      _    = DS.Empty
      iBFMC (currentpat:restofpats) bfmi = do 
        let patternf          = fmap (BSC8.singleton) $
                                DS.fromList           $
                                BSC8.unpack currentpat
        let countf            = runST $ countFMIndexB patternf
                                                      bfmi
        (currentpat,countf) DS.<| (iBFMC restofpats bfmi)

-- | Takes a list of pattern(s) of 'Text's
-- and an input 'Text'
-- and returns the number of occurences of the pattern(s)
-- in the input 'Text'.
-- Parallelized over all available cores.
textFMIndexCountP :: [Text]
                  -> Text
                  -> Seq (Text,CIntT)
textFMIndexCountP []      _     = DS.Empty 
textFMIndexCountP _       ""    = DS.Empty
textFMIndexCountP allpats input = do
  let tfmindex = textToBWTToFMIndexT input
  (iTFMC allpats tfmindex) `CPS.using` (CPS.parTraversable CPS.rseq)
    where
      iTFMC []                      _    = DS.Empty
      iTFMC (currentpat:restofpats) tfmi = do
        let patternf    = fmap (DText.singleton) $
                          DS.fromList            $
                          DText.unpack currentpat
        let countf      = runST $ countFMIndexT patternf
                                                tfmi
        (currentpat,countf) DS.<| (iTFMC restofpats tfmi)

{-------------------}


{-Locate operations.-}

-- | Takes a list of pattern(s) of 'ByteString's
-- and an input 'ByteString'
-- and returns the indexe(s) of occurences of the pattern(s)
-- in the input 'ByteString'.
-- The output indices are __1__-based,
-- and are __not__ sorted.
bytestringFMIndexLocateS :: [ByteString]
                         -> ByteString
                         -> Seq (ByteString,LIntB)
bytestringFMIndexLocateS []      _                        = DS.Empty
bytestringFMIndexLocateS _       (BSC8.uncons -> Nothing) = DS.Empty
bytestringFMIndexLocateS allpats input                    = do
  let bytestringsa      = createSuffixArray   $
                          fmap (BS.singleton) $
                          DS.fromList         $ 
                          BS.unpack input
  let bfmindex          = bytestringToBWTToFMIndexB input
  iBFML allpats
        bytestringsa
        bfmindex
    where
      iBFML []                      _   _    = DS.Empty
      iBFML (currentpat:restofpats) bsa bfmi = do
        let patternf    = fmap (BSC8.singleton) $
                          DS.fromList           $
                          BSC8.unpack currentpat
        let indices     = runST $ locateFMIndexB patternf
                                                 bfmi
        let indicesf    = fmap (\x -> if | isNothing x
                                         -> Nothing
                                         | otherwise
                                         -> Just           $
                                            suffixstartpos $
                                            DS.index bsa ((fromJust x) - 1)
                               ) indices
        (currentpat,indicesf) DS.<| (iBFML restofpats bsa bfmi)

-- | Takes a list of pattern(s) of 'Text's
-- and an input 'Text'
-- and returns the indexe(s) of occurences of the pattern(s)
-- in the input 'Text'.
-- The output indices are __1__-based,
-- and are __not__ sorted.
textFMIndexLocateS :: [Text]
                   -> Text
                   -> Seq (Text,LIntT)
textFMIndexLocateS []      _     = DS.Empty
textFMIndexLocateS _       ""    = DS.Empty
textFMIndexLocateS allpats input = do
  let textsa      = createSuffixArray      $
                    fmap (DText.singleton) $
                    DS.fromList            $
                    DText.unpack input
  let tfmindex    = textToBWTToFMIndexT input
  iTFML allpats
        textsa
        tfmindex
    where
      iTFML []                      _   _    = DS.Empty
      iTFML (currentpat:restofpats) tsa tfmi = do
        let patternf    = fmap (DText.singleton) $
                          DS.fromList            $
                          DText.unpack currentpat
        let indices     = runST $ locateFMIndexT patternf
                                                 tfmi
        let indicesf    = fmap (\x -> if | isNothing x
                                         -> Nothing
                                         | otherwise
                                         -> Just           $
                                            suffixstartpos $
                                            DS.index tsa ((fromJust x) - 1)
                               ) indices
        (currentpat,indicesf) DS.<| (iTFML restofpats tsa tfmi)

-- | Takes a list of pattern(s) of 'ByteString's
-- and an input 'ByteString'
-- and returns the indexe(s) of occurences of the pattern(s)
-- in the input 'ByteString'.
-- The output indices are __1__-based,
-- and are __not__ sorted.
-- Parallelized over all available cores.
bytestringFMIndexLocateP :: [ByteString]
                         -> ByteString
                         -> Seq (ByteString,LIntB)
bytestringFMIndexLocateP []      _                        = DS.Empty
bytestringFMIndexLocateP _       (BSC8.uncons -> Nothing) = DS.Empty
bytestringFMIndexLocateP allpats input                    = do
  let bytestringsa      = createSuffixArray   $
                          fmap (BS.singleton) $
                          DS.fromList         $ 
                          BS.unpack input
  let bfmindex          = bytestringToBWTToFMIndexB input
  (iBFML allpats bytestringsa bfmindex) `CPS.using` (CPS.parTraversable CPS.rseq)
    where
      iBFML []                      _   _    = DS.Empty
      iBFML (currentpat:restofpats) bsa bfmi = do
        let patternf    = fmap (BSC8.singleton) $
                          DS.fromList           $
                          BSC8.unpack currentpat
        let indices     = runST $ locateFMIndexB patternf
                                                 bfmi
        let indicesf    = fmap (\x -> if | isNothing x
                                         -> Nothing
                                         | otherwise
                                         -> Just           $
                                            suffixstartpos $
                                            DS.index bsa ((fromJust x) - 1)
                               ) indices
        (currentpat,indicesf) DS.<| (iBFML restofpats bsa bfmi)

-- | Takes a list of pattern(s) of 'Text's
-- and an input 'Text'
-- and returns the indexe(s) of occurences of the pattern(s)
-- in the input 'Text'.
-- The output indices are __1__-based,
-- and are __not__ sorted.
-- Parallelized over all available cores.
textFMIndexLocateP :: [Text]
                   -> Text
                   -> Seq (Text,LIntT)
textFMIndexLocateP []      _     = DS.Empty
textFMIndexLocateP _       ""    = DS.Empty
textFMIndexLocateP allpats input = do
  let textsa      = createSuffixArray      $
                    fmap (DText.singleton) $
                    DS.fromList            $
                    DText.unpack input
  let tfmindex    = textToBWTToFMIndexT input
  (iTFML allpats textsa tfmindex) `CPS.using` (CPS.parTraversable CPS.rseq)
    where
      iTFML []                      _   _    = DS.Empty
      iTFML (currentpat:restofpats) tsa tfmi = do
        let patternf    = fmap (DText.singleton) $
                          DS.fromList            $
                          DText.unpack currentpat
        let indices     = runST $ locateFMIndexT patternf
                                                 tfmi
        let indicesf    = fmap (\x -> if | isNothing x
                                         -> Nothing
                                         | otherwise
                                         -> Just           $
                                            suffixstartpos $
                                            DS.index tsa ((fromJust x) - 1)
                               ) indices
        (currentpat,indicesf) DS.<| (iTFML restofpats tsa tfmi)

{--------------------}
