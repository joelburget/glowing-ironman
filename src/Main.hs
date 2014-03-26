{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.UUID.V4

import           Snap
import           Snap.Extras.CoreUtils
import           Snap.Extras.JSON
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession

import           Database.PostgreSQL.Simple.Time
import           Database.PostgreSQL.Simple.SqlQQ
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.PostgresqlSimple

import Models

data App = App
    { _sess :: Snaplet SessionManager
    , _db   :: Snaplet Postgres
    , _auth :: Snaplet (AuthManager App)
    }
makeLenses ''App

instance HasPostgres (Handler b App) where
    getPostgresState = with db get

main :: IO ()
main = serveSnaplet defaultConfig app

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    s <- nestSnaplet "" sess $
         initCookieSessionManager "site_key.txt" "_cookie" Nothing
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    return $ App s d a

routes :: [(ByteString, Handler App App ())]
routes = [ ("/", writeBS "hello world")

         , ("api/events", method GET getEvents)
         , ("api/events", method POST createEvent)
         , ("api/events/:eventid", method POST getEvent)

         , ("api/users", method GET getUsers)
         , ("api/users", method POST createNewUser)

         , ("add/:uname", addHandler)

         , ("test/create-events", testCreateEvents)
         ]

createTables :: Handler App App ()
createTables = do
    usersTable <- query "select relname from pg_class where relname=(?)"
        ["users" :: ByteString]
    when (null (usersTable :: [Only T.Text])) $ do
        execute_ [sql| create table users
                           (name text not null,
                            constraint users_pkey primary key (name)) |]
        return ()

    eventsTable <- query "select relname from pg_class where relname=(?)"
        ["events" :: ByteString]
    when (null (eventsTable :: [Only T.Text])) $ do
        execute_ [sql| create table events
                           (uuid uuid not null,
                            author text not null,
                            title text not null,
                            address text not null,
                            seats smallint not null,
                            description text not null,
                            start timestamp with time zone not null,
                            "end" timestamp with time zone,
                            PRIMARY KEY (uuid)) |]
        return ()

testCreateEvents :: Handler App App ()
testCreateEvents = do
    now <- Finite <$> liftIO getZonedTime
    eventId <- liftIO nextRandom
    let users = [["dinojoel" :: ByteString], ["make_good_art"]]
        events = [ Event eventId (User "dinojoel") "House Party"
            "750 N Shoreline Blvd, Apt 20" 40 "wild and crazy house party"
            now Nothing]

    createTables
    execute "delete from users where name=(?)" ["dinojoel" :: ByteString]
    execute "delete from users where name=(?)" ["make_good_art" :: ByteString]
    execute "delete from events where uuid=(?)" [eventId]

    executeMany userInsert users
    executeMany eventInsert events
    writeBS "done!"

userInsert :: Query
userInsert = "insert into users (name) values (?)"

eventInsert :: Query
eventInsert = "insert into events (uuid, author, title, address, seats, description, start, \"end\") values (?, ?, ?, ?, ?, ?, ?, ?)"

createNewUser :: Handler App App ()
createNewUser = do
    mehbe <- getJSON
    case mehbe of
        Left str -> badReq $ pack str
        Right u -> do
            execute userInsert u
            writeJSON (u :: User)

createEvent :: Handler App App ()
createEvent = do
    mehbe <- getJSON
    case mehbe of
        Left str -> badReq $ pack str
        Right ev -> do
            execute eventInsert ev
            writeJSON (ev :: Event)

getEvents :: Handler App App ()
getEvents = do
    results <- query_ "select * from events" :: Handler App App [Event]
    writeJSON (results :: [Event])

getEvent :: Handler App App ()
getEvent = do
    Just eventid <- getParam "eventid"
    event <- query "select * from events where uuid=(?)" [eventid]
    writeJSON (event :: [Event])

getUsers :: Handler App App ()
getUsers = do
    results <- query_ "select * from users" :: Handler App App [User]
    writeJSON (results :: [User])

addHandler :: Handler App App ()
addHandler = do
    mname <- getParam "uname"
    let guestName = maybe "guest" T.decodeUtf8 mname
    u <- with auth $ createUser guestName ""
    liftIO $ print u
