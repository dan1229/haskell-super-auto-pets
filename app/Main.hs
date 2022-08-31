module Main where


import Application
import Yesod
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql 

connStr :: ConnectionString
connStr = "host=localhost dbname=test user=test password=test port=5432"

main :: IO ()
main = runStderrLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do
    warp 3000 $ App pool