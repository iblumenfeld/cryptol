{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen.CWord where

import Cryptol.Eval.Value (BitWord(..))
import Cryptol.Prims.Eval (BinOp, UnOp)
import Cryptol.Utils.Compare (OrderingSymbolic)
import Cryptol.Utils.Compare.Class (Comparable(..))
import Cryptol.Utils.Panic (panic)

import Cryptol.Symbolic.BitVector
import Cryptol.Utils.PP

import Data.SBV


-- CWord -----------------------------------------------------------------------

type CWord = BitVector

-- -- A type of words with statically-known bit sizes.
-- data CWord
--   = CWord8  SWord8
--   | CWord16 SWord16
--   | CWord32 SWord32
--   | CWord64 SWord64
--   | UnsupportedSize Int

-- instance BitWord SBool CWord where
--   packWord bs = case length bs of
--     8  -> CWord8  $ fromBitsBE bs
--     16 -> CWord16 $ fromBitsBE bs
--     32 -> CWord32 $ fromBitsBE bs
--     64 -> CWord64 $ fromBitsBE bs
--     n  -> UnsupportedSize n
--   unpackWord cw = case cw of
--     CWord8  w -> blastBE w
--     CWord16 w -> blastBE w
--     CWord32 w -> blastBE w
--     CWord64 w -> blastBE w
--     UnsupportedSize n -> panic "CodeGen.SBVC.unpackWord @SBool @CWord"
--       [ "Words of width " ++ show n ++ " are not supported." ]

-- instance Comparable CWord OrderingSymbolic where
--   cmp (CWord8  l) (CWord8  r) = cmp l r
--   cmp (CWord16 l) (CWord16 r) = cmp l r
--   cmp (CWord32 l) (CWord32 r) = cmp l r
--   cmp (CWord64 l) (CWord64 r) = cmp l r
--   cmp l r
--     | cWidth l == cWidth r = panic "CodeGen.SBVC.cmp @CWord"
--       [ "Can't compare words of unsupported size " ++ show (cWidth l) ]
--     | otherwise = panic "CodeGen.SBVC.cmp @CWord"
--       [ "Can't compare words of differing sizes:"
--       , show (cWidth l)
--       , show (cWidth r)
--       ]

-- mkCWord :: Integer -> Integer -> CWord
-- mkCWord width value = case width of
--   8  -> CWord8  $ fromInteger value
--   16 -> CWord16 $ fromInteger value
--   32 -> CWord32 $ fromInteger value
--   64 -> CWord64 $ fromInteger value
--   _  -> UnsupportedSize $ fromInteger width

-- cWidth :: CWord -> Int
-- cWidth CWord8 {} = 8
-- cWidth CWord16{} = 16
-- cWidth CWord32{} = 32
-- cWidth CWord64{} = 64
-- cWidth (UnsupportedSize n) = n

-- liftUnCWord
--   :: UnOp SWord8
--   -> UnOp SWord16
--   -> UnOp SWord32
--   -> UnOp SWord64
--   -> Integer -> UnOp CWord
-- liftUnCWord op8 op16 op32 op64 _ cw = case cw of
--   CWord8  w -> CWord8  (op8  w)
--   CWord16 w -> CWord16 (op16 w)
--   CWord32 w -> CWord32 (op32 w)
--   CWord64 w -> CWord64 (op64 w)
--   _ -> cw

-- liftBinCWord
--   :: BinOp SWord8
--   -> BinOp SWord16
--   -> BinOp SWord32
--   -> BinOp SWord64
--   -> Integer -> BinOp CWord
-- liftBinCWord op8 op16 op32 op64 _ cl cr = case (cl, cr) of
--   (CWord8  l, CWord8  r) -> CWord8  (op8  l r)
--   (CWord16 l, CWord16 r) -> CWord16 (op16 l r)
--   (CWord32 l, CWord32 r) -> CWord32 (op32 l r)
--   (CWord64 l, CWord64 r) -> CWord64 (op64 l r)
--   (UnsupportedSize l, UnsupportedSize r) | l == r -> UnsupportedSize l
--   _ -> panic "CodeGen.SBVC.liftBinCWord"
--     [ "size mismatch"
--     , show (cWidth cl)
--     , show (cWidth cr)
--     ]

-- | All the classes supported by the kinds of words contained in a 'CWord'.
-- Commented-out contexts are ones which we could support but don't for now
-- because we aren't using them yet and don't want to spuriously add imports.
type SBVWord a =
  ( SDivisible a
  , FromBits a
  , Polynomial a
  , Bounded a
  , Enum a
  , Eq a
  , Num a
  , Show a
  -- , Arbitrary a
  , Bits a
  -- , NFData a
  -- , Random a
  , SExecutable a
  , Data.SBV.HasKind a
  , PrettyNum a
  , Uninterpreted a
  , Mergeable a
  , OrdSymbolic a
  , EqSymbolic a
  )

-- liftBinSBVWord :: (forall a. SBVWord a => BinOp a) -> Integer -> BinOp CWord
-- liftBinSBVWord op = liftBinCWord op op op op

-- liftUnSBVWord :: (forall a. SBVWord a => UnOp a) -> Integer -> UnOp CWord
-- liftUnSBVWord op = liftUnCWord op op op op

-- instance Mergeable CWord where
--   symbolicMerge b sb = liftBinSBVWord
--     (symbolicMerge b sb)
--     (panic "CodeGen.SBVC.symbolicMerge @CWord" ["unused size argument was unexpectedly inspected"])


-- -- TODO: use hex or WithBase instead, and reflect the bit widths visually
-- instance PP CWord where
--   ppPrec _ cw = case cw of
--     CWord8  w -> ppw 8  w
--     CWord16 w -> ppw 16 w
--     CWord32 w -> ppw 32 w
--     CWord64 w -> ppw 64 w
--     UnsupportedSize n -> size n
--     where
--     size n = text $ "<[" ++ show (n :: Int) ++ "]>"
--     ppw  n = maybe (size n) (text . show) . unliteral

-- instance PP Value where
--   ppPrec n v = ppPrec n (WithBase defaultPPOpts v)

