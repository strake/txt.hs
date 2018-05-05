module Data.Text.Private where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF
import Data.String (IsString (..))
import Text.Printf (PrintfArg (..))
import Text.Read (Read (..))

newtype Text = Text { unText :: ByteString }
  deriving (Eq, Ord, Semigroup, Monoid, NFData)

instance IsString Text where fromString = Text . UTF.fromString
instance Read Text where readPrec = fromString <$> readPrec
instance Show Text where showsPrec n = showsPrec n . UTF.toString . unText
instance PrintfArg Text where
    formatArg = formatArg . UTF.toString . unText
    parseFormat = parseFormat . UTF.toString . unText
