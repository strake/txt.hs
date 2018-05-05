module Data.Text.Lazy
                 (Text, fromChars, chars, cons, snoc, uncons, unsnoc, reverse, inits, tails,
                  break, span, breakEnd, spanEnd, breaks, splitAt, length, unfoldr,
                  findIndex, findIndexEnd, stripPrefix, stripSuffix, isPrefixOf, isSuffixOf,
                  fromStrict, toStrict, fromChunks, toChunks) where

import Prelude hiding (break, length, reverse, span, splitAt)
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.UTF8 as UTF
import Data.Foldable hiding (length)
import Data.Function (on)
import Data.Functor.Const (Const (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Sum (..))
import Data.String
import Data.Tuple (swap)
import Data.Word
import Util

import Data.Text.Lazy.Private
import qualified Data.Text.Private as Strict

chars :: Applicative p => (Char -> p Char) -> Text -> p Text
chars f = uncons & \ case Just (x, xs) -> cons <$> f x <*> chars f xs
                          Nothing      -> pure mempty

fromChars :: Foldable f => f Char -> Text
fromChars = Text . fromString . toList

cons :: Char -> Text -> Text
cons x xs = fromString [x] <> xs

snoc :: Text -> Char -> Text
snoc xs x = xs <> fromString [x]

uncons :: Text -> Maybe (Char, Text)
uncons = unText & UTF.uncons & (fmap . fmap) Text

unsnoc :: Text -> Maybe (Text, Char)
unsnoc (Text xs) = [(Text xs, x) | k <- B.findIndexEnd isStartByte xs
                                 , (xs, ys) <- Just $ B.splitAt k xs
                                 , (x, _) <- UTF.decode ys]

reverse :: Text -> Text
reverse = unfoldr (fmap swap . unsnoc)

unfoldr :: (a -> Maybe (Char, a)) -> a -> Text
unfoldr f = Text . B.unfoldr go . (,) mempty
  where go (bs, a) | Just (b, bs) <- B.uncons bs = Just (b, (bs, a))
                   | otherwise = f a >>= (UTF.fromString . pure *** id >>> go)

splitAt :: Word -> Text -> (Text, Text)
splitAt n (Text bs) = join (***) Text . go 0 n $ bs
  where go k 0 _   = B.splitAt k bs
        go k n bs' = case UTF.decode bs' of Nothing -> (bs, mempty)
                                            Just (_, δ) -> go (k+δ) (n-1) (B.drop δ bs')

break, span, breakEnd, spanEnd :: (Char -> Bool) -> Text -> (Text, Text)
break f = join (***) Text . UTF.break f . unText
span f = join (***) Text . UTF.span f . unText
breakEnd f = spanEnd (not . f)
spanEnd f xs | Just k <- findIndexEnd (not . f) xs = splitAt k xs
             | otherwise = (mempty, xs)

findIndex, findIndexEnd :: Alternative f => (Char -> Bool) -> Text -> f Word
findIndex f = go 0
  where go n = uncons & \ case Nothing -> empty
                               Just (x, xs) -> n <$ guard (f x) <|> go (n+1) xs
findIndexEnd f = flip go <*> \ xs -> length xs - 1
  where go n = unsnoc & \ case Nothing -> empty
                               Just (xs, x) -> n <$ guard (f x) <|> go (n-1) xs

breaks :: Text -> NonEmpty (Text, Text)
breaks = liftA2 NE.zip inits tails

inits, tails :: Text -> NonEmpty Text
inits = NE.unfoldr $ id &&& unsnoc & fmap fst
tails = NE.unfoldr $ id &&& uncons & fmap snd

length :: Text -> Word
length = getSum . getConst . chars (pure . Const $ Sum 1)

stripPrefix, stripSuffix :: Text -> Text -> Maybe Text
stripPrefix = fmap Text ∘∘ B.stripPrefix `on` unText
stripSuffix = fmap Text ∘∘ B.stripSuffix `on` unText

isPrefixOf, isSuffixOf :: Text -> Text -> Bool
isPrefixOf = B.isPrefixOf `on` unText
isSuffixOf = B.isSuffixOf `on` unText

isStartByte, isContByte :: Word8 -> Bool
isStartByte = not . isContByte
isContByte b = 0x80 == b .&. 0xC0

fromStrict :: Strict.Text -> Text
fromStrict = Text . B.fromStrict . Strict.unText

toStrict :: Text -> Strict.Text
toStrict = Strict.Text . B.toStrict . unText

fromChunks :: Foldable f => f Strict.Text -> Text
fromChunks = Text . B.fromChunks . fmap Strict.unText . toList

toChunks :: Text -> [Strict.Text]
toChunks = fmap Strict.Text . B.toChunks . unText
