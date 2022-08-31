{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Application
import Yesod
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql 
import Data.Text

newtype Name = Name { name :: Text } deriving newtype (Show, PersistField, PersistFieldSql)
newtype Email = Email { email :: Text } deriving newtype (Show, PersistField, PersistFieldSql)
newtype Password = Password { password :: Text } deriving newtype (Show, PersistField, PersistFieldSql)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name Name
    email Email
    UniqueEmail email
    hashedPassword Password
    deriving Show
|]

connStr :: ConnectionString
connStr = "host=localhost dbname=test user=test password=test port=5432"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    warp 3000 $ App pool