module Data.Text (Text) where

import Data.ByteString
import Data.ByteString.UTF8
import qualified Data.String as S
import Text.Read (Read (..))

newtype Text = Text { unText :: ByteString }
  deriving (Eq, Ord, Semigroup, Monoid)

instance S.IsString Text where fromString = Text . fromString
instance Read Text where readPrec = S.fromString <$> readPrec
instance Show Text where showsPrec n = showsPrec n . toString . unText
