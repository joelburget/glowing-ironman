{-# OPTIONS_GHC -fno-warn-orphans #-}
module Instances where

import           Control.Monad
import           Data.Aeson
import           Data.ByteString.Lazy (fromStrict)
import qualified Data.Text.Encoding as T
import           Data.UUID

import           Database.PostgreSQL.Simple.Time

instance ToJSON a => ToJSON (Unbounded a) where
    toJSON (Finite a) = toJSON a

    -- json leaves out +/- infty as well as NaN
    toJSON _ = Null

instance FromJSON a => FromJSON (Unbounded a) where
    parseJSON = parseJSON

instance ToJSON UUID where
    toJSON = String . T.decodeUtf8 . toASCIIBytes

instance FromJSON UUID where
    parseJSON (String str) = do
        let mbstr = fromLazyASCIIBytes $ fromStrict $ T.encodeUtf8 $ str
        case mbstr of
            Just bstr -> return bstr
            _ -> mzero
    parseJSON _ = mzero
