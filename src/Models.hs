{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
module Models where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson.TH
import qualified Data.Text as T
import           Data.UUID
import           Database.PostgreSQL.Simple -- as P
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.Time
import           Snap.Snaplet.PostgresqlSimple

import           Options
import           Instances ()

data User = User
    { _name :: T.Text }
    deriving Show
makeLenses ''User
deriveJSON jsonOptions ''User

instance ToRow User where
    toRow u = [toField (u^.name)]

instance FromRow User where
    fromRow = User <$> field

data Event = Event
    { _uuid :: UUID
    , _author :: User
    , _title :: T.Text
    -- , _location ::
    , _address :: T.Text
    , _seats :: Int
    , _description :: T.Text
    -- , _reservations ::
    , _start :: ZonedTimestamp
    , _end :: Maybe ZonedTimestamp
    } deriving Show
makeLenses ''Event
deriveJSON jsonOptions ''Event

instance ToRow Event where
    toRow (Event uuid' author' title' address' seats' description' start' duration') =
        [toField uuid', toField (author'^.name), toField title', toField address', toField seats', toField description', toField start', toField duration']

instance FromRow Event where
    fromRow = Event <$> field <*> (User <$> field) <*> field <*> field <*> field <*> field <*> field <*> field
