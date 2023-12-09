{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , FlexibleContexts
           , FunctionalDependencies
           , RankNTypes
           , GADTs
           , LambdaCase
           , ConstraintKinds
           , OverloadedStrings
  #-}
{-|
Module      : PLHash.Short
Copyright   : (c) Samuel A. Yallop, 2020
Maintainer  : syallop@gmail.com
Stability   : experimental

-}
module PLHash.Short
  ( ShortHash ()
  , mkBase58ShortHash
  , unBase58ShortHash

  , Shortable (shortenAgainst, shortLength, isShortened, toShort)
  )
  where

import Prelude hiding (lookup)

import PLHash

-- Other
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base58 as B58
import qualified Data.Text as Text

-- | A Hash that may have had it's bytes truncated an arbitrary amount.
data ShortHash = ShortHash
  { _shortHashAlgorithm :: HashAlgorithm
  , _unShortHash        :: BS.ByteString
  }
  deriving (Eq, Ord)

instance Show ShortHash where
  show (ShortHash alg h) = Text.unpack . mconcat $
    [ hashIdentifier alg
    , "/"
    , decodeUtf8 . B58.encodeBase58 B58.bitcoinAlphabet $ h
    ]

-- | Attempt to construct a ShortHash from an optional algorithm and Base58
-- encoded Bytes.
--
-- Unlike a full Hash:
-- - If an algorithm is not provided, it is assumed to be SHA512
-- - The number of bytes may be less than expected (but not zero)
mkBase58ShortHash :: Maybe HashAlgorithm -> Text -> Maybe ShortHash
mkBase58ShortHash mAlg txt = case fromMaybe SHA512 mAlg of
  SHA512
    -> do bytes <- B58.decodeBase58 B58.bitcoinAlphabet . encodeUtf8 $ txt
          if BS.length bytes == 0  || 128 < BS.length bytes
            then Nothing
            else Just $ ShortHash SHA512 bytes

-- | Extract a Hashes algorithm (if it is not the default) and (possibly
-- truncated) base58 interpretation of the hash.
unBase58ShortHash :: ShortHash -> (Maybe HashAlgorithm, Text)
unBase58ShortHash (ShortHash alg bytes) =
  let mAlg = case alg of
               SHA512
                 -> Nothing
      txt  = decodeUtf8 . B58.encodeBase58 B58.bitcoinAlphabet $ bytes
   in (mAlg, txt)

-- Shortable maps a long type to a shorter version.
class Shortable long short | long -> short, short -> long where
  -- Longest shared prefix of two keys
  -- Shortest unambiguous key against a second key.
  -- E.G.
  -- aaaabcd
  -- aaaaxyz
  -- = aaaab

  -- Shorten something against another. E.G.:
  -- abcDEF
  -- abXYX
  -- =
  -- abc
  shortenAgainst :: long -> Maybe long -> short

  -- For comparing the length of shorts to each other
  shortLength :: short -> Int

  isShortened :: short -> long -> Bool

  -- Coerce a long thing into a short thing. Not guaranteed to actually make the
  -- thing shorter.
  toShort :: long -> short

instance Shortable BS.ByteString BS.ByteString where
  shortLength = BS.length

  toShort bs = bs

  isShortened = BS.isPrefixOf

  shortenAgainst sourceBs Nothing = case BS.uncons sourceBs of
    Nothing
      -> ""

    Just (w,_)
      -> BS.singleton w

  shortenAgainst sourceBs (Just againstBs) =
    let (common, uncommon) = span (\(a,b) -> a == b) $ BS.zip sourceBs againstBs
     in BS.pack . fmap fst $ case uncommon of
          -- Two bytestrings are the same.
          []
            -> common

          (u:_)
            -> common ++ [u]

instance Shortable Text Text where
  shortLength = shortLength . encodeUtf8

  isShortened short long = isShortened (encodeUtf8 short) (encodeUtf8 long)
  toShort = decodeUtf8 . toShort . encodeUtf8

  shortenAgainst sourceBs againstBs = decodeUtf8 $ shortenAgainst (encodeUtf8 sourceBs) (fmap encodeUtf8 againstBs)

-- Hashes are shortened by their textual base58 encoding NOT their underlying
-- bytes.
instance Shortable Hash ShortHash where
  shortLength = shortLength . _unShortHash

  toShort h = ShortHash (hashAlgorithm h) (hashBytes h)

  -- The smallest hash is an empty string if there are no characters and a single
  -- character otherwise.
  -- TODO: A larger minimum length would make printed output change less
  -- frequently which may be desirable.
  -- TODO: Should empty text be allowed?
  shortenAgainst sourceHash mAgainstHash = case mAgainstHash of
    Nothing
      -> let (sourceAlg, sourceText) = unBase58 sourceHash
             shortenedText           = case Text.uncons sourceText of
                                         Nothing
                                           -> ""
                                         Just (c,_)
                                           -> Text.singleton c
          in case mkBase58ShortHash (Just sourceAlg) shortenedText of
               Just shortenedHash
                 -> shortenedHash

               -- TODO: When can this actually happen?
               -- If we can fail, the interface should know about it.
               Nothing
                 -> error (mconcat ["Could not shorten ", show sourceHash, " against ", show sourceAlg, show sourceText])

    Just againstHash
      -- If the hashes use a different algorithm the bytes arent needed to
      -- identify them.
      | hashAlgorithm sourceHash /= hashAlgorithm againstHash
       -> ShortHash (hashAlgorithm sourceHash) ""

      -- If the hashes use the same algorithm, find the shortest sequence of
      -- unique base58 characters.
      | otherwise
       -> let (sourceAlg, sourceText)= unBase58 sourceHash
              (_, againstText) = unBase58 againstHash
              shortenedText = shortenAgainst sourceText (Just againstText)
           in case mkBase58ShortHash (Just sourceAlg) shortenedText of
                Just shortenedHash
                  -> shortenedHash

                -- TODO: When can this actually happen?
                -- If we can fail, the interface should know about it.
                Nothing
                  -> error (mconcat ["Could not shorten ", show sourceHash, " against ", show againstText])

  isShortened shortHash longHash
    = let (shortAlg,shortText) = unBase58ShortHash shortHash
          (longAlg, longText)  = unBase58ShortHash . toShort $ longHash
       in shortAlg == longAlg && Text.isPrefixOf shortText longText

