module TUImage where

import Data.Bits
import Data.Word

fileSignature :: String
fileSignature = "TUI "

version :: Word8
version = 1

type Type = Word8

exceed256 :: Type -> Type
exceed256 = (.|. 0x01) . (.&. 0xfe)

withDepth1, withDepth2, withDepth4, withDepth8,withAlpha :: Type -> Type
withDepth1 = (.|. 0x00) . (.&. 0xf9)
withDepth2 = (.|. 0x02) . (.&. 0xf9)
withDepth4 = (.|. 0x04) . (.&. 0xf9)
withDepth8 = (.|. 0x06) . (.&. 0xf9)
withAlpha = (.|. 0x08) . (.&. 0xf7)

colorsOf :: Type -> Word8 -> Type
colorsOf t n | n >= 1 && n <= 16 = ((n-1) `shiftL` 4) .|. (t .&. 0x0f)
             | otherwise         = error $ "Number of colors must be in range of [1, 16], but got: " ++ show n
