{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

-- IMPORTS FOR MYSQL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (NoLoggingT (..))
import Control.Monad.Trans.Reader (runReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson as JSON
import Data.Char
import Data.Int (Int64 (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Database.Persist
import Database.Persist.MySQL
  ( ConnectInfo (..),
    SqlBackend (..),
    defaultConnectInfo,
    fromSqlKey,
    runMigration,
    runSqlPool,
    toSqlKey,
    withMySQLConn,
  )
import Database.Persist.Sql (SqlPersistT, runSqlConn)
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Database.Persist.Types (PersistValue (PersistInt64))
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant (Handler, throwError)
import Servant.API
import System.Environment (getArgs)

-- SCHEMA DEFINITIONS

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
UserReg json
    Id Int
    name Text
    mail  Text
    mob  Int
    pass  Text
    deriving Eq Show Generic
Movie json
    Id Int
    name Text
    rating Int
    genre Text
    deriving Eq Show Generic
|]

type Message = String
-- API DEFINITIONS

type API = UserAPI :<|> MovieAPI

type UserAPI =
  "register" :> ReqBody '[JSON] UserReg :> Post '[JSON] UserReg
    :<|> "login" :> QueryParam "mail" Text :> QueryParam "pass" Text :> Get '[JSON] [UserReg]
    :<|> "logout" :> Post '[JSON] Message

type MovieAPI =
  "addmovie" :> ReqBody '[JSON] Movie :> Post '[JSON] Movie
    :<|> "deletemovie" :> Capture "id" Int :> Delete '[JSON] Message
    :<|> "updatemovie" :> Capture "id" Int :> QueryParam "name" Text :> QueryParam "rating" Int :> QueryParam "genre" Text :> Post '[JSON] Movie
    -- :<|> "updatemovie" :> Capture "id" Int :> Post '[JSON] Message
    :<|> "getmovies" :> Get '[JSON] [Movie]

-- DATA DEFINITONS

-- PASSWORD VALIDATION FUNCTION

-- passValid :: UserReg -> [String]
-- passValid u1 =
--   if length (pass u1) < 7 then
--     return "Password length is too short"
--     -- else if ()
--     --   then return (Message "Password too short")
--     else
--       return "Password length is good"

-- SERVER DEFINITION
-- server1 :: Server API
-- server1 = register  :<|> signin
--   where
--     register :: UserReg -> Handler Message
--     register c = return (Message (passValid c))

--     signin :: Handler [UserReg]
--     signin = return user1

-- CONNECTION FOR MYSQL

runDB :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> Handler a
runDB a = liftIO $ runNoLoggingT $ runResourceT $ withMySQLConn connInfo $ runSqlConn a

-- Change these out to suit your local setup
connInfo :: ConnectInfo
connInfo = defaultConnectInfo {connectHost = "127.0.0.1", connectUser = "root", connectPassword = "", connectDatabase = "test"}

doMigration :: IO ()
doMigration = runNoLoggingT $ runResourceT $ withMySQLConn connInfo $ runReaderT $ runMigration migrateAll

-- SERVER DEFINITIONS 2

server :: Server API
server = serveruser :<|> servermovie

-- USER SERVERRRR........

serveruser :: Server UserAPI
serveruser =
  register
    :<|> login
    :<|> logout
  where
    register userregJson = createuser userregJson
    login mail pass = userlogin mail pass
    logout = userlogout

    -- THESE ARE FOR MY SQL
    createuser :: UserReg -> Handler UserReg
    createuser user = do
      attemptCreate <- runDB $ insert user
      case attemptCreate of
        UserRegKey k -> return user
        _ -> throwError err503 {errBody = "Could not create Person."}

    -- METHOD DEVICED
    userlogin :: Maybe Text -> Maybe Text -> Handler [UserReg]
    userlogin mail pass = do
      case mail of
        Nothing -> return []
        Just n1 -> do
          case pass of
            Nothing -> return []
            Just n2 -> do
              personList <- runDB $ selectList [UserRegMail ==. n1, UserRegPass ==. n2] []
              if (personList == []) then
                throwError err503 {errBody = "Wrong Credentials!!!"}
                else
                  return $ map (\(Entity _ u) -> u) personList
    -- sqlResult <- runDB $ get $ UserRegMail mail
    -- case sqlResult of
    --   Just user -> return user
    --   Nothing -> throwError err404 { errBody = "Person with ID not found." }

    userlogout :: Handler Message
    userlogout = return "Logged out sucessfully"

-- MOVIES APIS SEVRERRR.....

servermovie :: Server MovieAPI
servermovie =
  addmovie
    :<|> deletemovie
    :<|> updatemovie
    :<|> getmovies
  where
    -- FOR MYSQL
    addmovie movieJson = createmovie movieJson
    deletemovie id = moviedelete id
    updatemovie id name rating genre = movieupdate id name rating genre
    getmovies = allmovies

-- THE BELOW DEFINITIONS WILL BE FOR MYSQL
createmovie :: Movie -> Handler Movie
createmovie movie = do
  attemptCreate <- runDB $ insert movie
  case attemptCreate of
    MovieKey k -> return movie
    _ -> throwError err503 {errBody = "Could not add movie."}

moviedelete :: Int -> Handler Message
moviedelete id = do 
  sqlResult <- runDB $ get $ MovieKey id
  case sqlResult of 
    Just movie -> do
      result <- runDB $ delete $ MovieKey id
      return "Movie deleted succesfully"
    Nothing -> throwError err503 {errBody = "No such movie available to delete."}

-- case deletemovie of
--   MovieKey k -> return movie
--   _           -> throwError err503 { errBody = "Could not add movie." }

-- YET TO FIND THE ALGO-----------------------------------
-- ALGO FOUND
movieupdate :: Int -> Maybe Text -> Maybe Int -> Maybe Text -> Handler Movie
movieupdate id (Just name) (Just rating) (Just genre) = do
  sqlResult <- runDB $ get $ MovieKey id
  case sqlResult of
    Nothing -> throwError err404 {errBody = "Movie with ID not found."}
    Just movie -> do
      if (name == "" && genre == "" && rating == 0)
        then throwError err404 {errBody = "Provided updated movie info....Movie cant be updated"}
        else do
          runDB $ update (MovieKey id) [MovieName =. name, MovieRating =. rating, MovieGenre =. genre]
          return movie

-- case name of
--   Nothing -> do
--     runDB $ update $ MovieKey id [movieName =. movieName  , movieRating =. rating, movieGenre =. genre]
--     return movie
--   Just n1 -> do
--     case genre of
--       Nothing -> do
--         runDB $ update $ MovieKey id [movieName =. name  , movieRating =. rating, movieGenre =. movieGenre]
--         return movie
--       Just n2 -> do
--         case rating of
--           Nothing -> do
--             runDB $ update $ MovieKey id [movieName =. name  , movieRating =. movieRating, movieGenre =. genre]
--             return movie
--           Just n3 -> do
--             runDB $ update $ MovieKey id [movieName =. name  , movieRating =. rating, movieGenre =. genre]
--             return movie

allmovies :: Handler [Movie]
allmovies = do
  movieList <- runDB $ selectList [] []
  -- listofmovies <- map (\(Entity _ u) -> u) movieList
  if (movieList /= []) then
    return $ map (\(Entity _ u) -> u) movieList
  else
    throwError err404 {errBody = "Sorry!! No list fo movies available."}

-- Making configurations for running the servers

api :: Proxy API
api = Proxy

-- Making the virtual application &&&  Running the hosts

-- APPLICATIONS

-- app1 :: Application
-- app1 = serve userAPI1 server1

-- FOR MY SQL DBS
app :: Application
app = serve api server

-- main :: IO ()
-- main = run 8081 app1