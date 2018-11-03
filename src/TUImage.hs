{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module TUImage where

import Control.Monad
import Data.Binary
import qualified Data.Binary.Get as G
import qualified Data.ByteString as B
import Data.Bits
import Data.Maybe (isJust)
import Data.Word

type Width = Either Word8 Word16
type Height = Either Word8 Word16
type Palettes = Either Word8 Word16
newtype Colors = C Word8 deriving (Enum, Eq, Num, Integral, Ord, Real)
data BitDepth = Depth1 | Depth2 | Depth4 | Depth8 deriving Enum
type Palette = [Word32]

data Type = Type
          { exceeds :: Bool
          , depth   :: BitDepth
          , useAlpha :: Bool
          , maximumColors :: Colors
          }

instance Binary Type where
  get  = getType <$> getWord8
    where getType :: Word8 -> Type
          getType x = Type e d a m
            where e = x .&. 1 == 0b1
                  d = toEnum . fromIntegral $ (x `shiftR` 1) .&. 0b11
                  a = (x `shiftR` 3) == 1
                  m = C $ (x `shiftR` 4) .&. 0b1111
  put (Type e d a m) = putWord8 $ e' .|. d' .|. a' .|. m'
    where e' = if e then 1 else 0 :: Word8
          d' =  (fromIntegral $ fromEnum d)  `shiftL` 1
          a' = (if a then 1 else 0) `shiftL` 3 :: Word8
          m' = fromIntegral m `shiftL` 4

newtype Index = I [Word16]
newtype Block = B [Word8]
type Color_Rem = (Word8, Word8) -- (remaining bits, bitdepth)
data Frame = Frame
           { index :: Maybe Index
           , pixels :: [Block]
           , next   :: Maybe Animation
           }
data Animation = Animation
               { animationHeader :: Word8
               , frame           :: Frame
               }

data TUImage = TUImage
             { imageType        :: Type
             , width            :: Width
             , height           :: Height
             , numberOfPalettes :: Palettes
             , palettes         :: [Palette]
             , mainFrame        :: Frame
             , animationInfo    :: Maybe Animation
             }
instance Binary TUImage where
  get = do
    sig <- G.getByteString 4 -- sig must be the same as fileSignature
    unless (B.unpack sig == fileSignature) (fail "File Signature is not corect" >> return ())
    ver <- G.getByteString 1 -- sig must be the same as version
    unless (head (B.unpack ver) == version) (fail "Version is not correct" >> return ())
    t <- get :: Get Type
    let et = exceeds t
    w <- if et then Right <$> G.getWord16le else Left <$> getWord8
    h <- if et then Right <$> G.getWord16le else Left <$> getWord8
    n <- if et then Right <$> G.getWord16le else Left <$> getWord8
    ps <- getPalettes et (depth t) (maximumColors t) n
    return $ TUImage t w h n ps undefined Nothing
    where getPalettes :: Bool -> BitDepth -> Colors -> Palettes -> Get [Palette]
          getPalettes a d m p = replicateM p' (getPalette a d m)
            where p' = case p of
                         (Left l) -> fromEnum l
                         (Right r) -> fromEnum r
          getPalette :: Bool -> BitDepth -> Colors -> Get Palette
          getPalette a (fromEnum -> d) m = do
            b <- getWord8
            -- let (b1, b2) = breakBit bs b
            return undefined
            where bs = d * (if a then 4 else 3) :: Int
                  bs' = ceiling (fromIntegral bs / 8)
          getColor :: Int -> Get Word32
          getColor x = do
            w <- getWord8
            -- StateT Get
            return 0
          getW32 :: BitDepth -> Get Word32
          getW32 Depth1 = (`shiftL` 24) . fromIntegral <$> getWord8
          getW32 Depth2 = (`shiftL` 24) . fromIntegral <$> getWord8
          getW32 Depth4 = (`shiftL` 16) . fromIntegral <$> G.getWord16le
          getW32 Depth8 = G.getWord32le
          breakBit :: Int -> Word8 -> (Word8, Word8)
          breakBit b n | b >= 0 || b < 8 = let b' = bits b `shiftL` (8 - b)
                                               n' = n .&. b'
                                               n'' = n .&. (complement b')
                                           in (n', n'')
                       | otherwise       = error "Bits too large"
  put = undefined

fileSignature :: [Word8]
fileSignature = map (fromIntegral . fromEnum) "TUI "

version :: Word8
version = fromIntegral $ fromEnum '1'

bits :: (Bits a, Num a) => Int -> a
bits 0 = 0
bits b = (1 `shiftL` b) .|. (bits (b - 1))

toWord32 :: forall a. (Integral a, FiniteBits a, Num a) => Bool -> BitDepth -> a -> Word32
toWord32 a d v = replicateColor d r g b a'
  where bs = 2 ^ fromEnum d
        s = finiteBitSize v
        c = replicateBits d bs (1 :: a)
        r = (fromIntegral v `shiftR` (s - bs)) .&. c
        g = (fromIntegral v `shiftR` (s - bs * 2)) .&. c
        b = (fromIntegral v `shiftR` (s - bs * 3)) .&. c
        a' = if a then Just $ (fromIntegral v `shiftR` (s - bs * 4)) .&. c else Nothing

replicateBits :: (Integral a, FiniteBits a) => BitDepth -> Int -> a -> Word32
replicateBits _ 0 x = fromIntegral x
replicateBits d b x = fromIntegral x `shiftL` (fromEnum d  + 1) .|. replicateBits d (b-1) x

replicateColor :: (Integral a, FiniteBits a) => BitDepth -> a -> a -> a -> Maybe a -> Word32
replicateColor d r g b ma = r' .|. g' .|. b' .|. a'
  where r' = (replicateBits d bs r) `shiftL` if ima then 24 else 16
        g' = (replicateBits d bs g) `shiftL` if ima then 16 else 8
        b' = (replicateBits d bs b) `shiftL` if ima then 8 else 0
        a' = replicateBits d bs (maybe 0 id ma)
        bs = 8 `div` finiteBitSize r - 1
        ima = isJust ma


bitsRequired :: Int -> Int -> BitDepth -> Get [Word8]
bitRequired 0 _ _ = return []
bitRequired cnt el d = do
  
