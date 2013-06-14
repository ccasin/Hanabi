{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai.Middleware.RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager, def)
import Control.Monad.Logger (runLoggingT)
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)

import Data.Conduit (runResourceT)
import Data.IORef (newIORef)
import Control.Concurrent.Chan (newChan)

-- for heroky postgres
import qualified Data.HashMap.Strict as H
import qualified Data.Aeson.Types as AT
#ifndef DEVELOPMENT
import qualified Web.Heroku
#endif

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Hanabi
import Handler.Infrastructure (getSetNameR,postSetNameR)
import Handler.Lobby (getHanabiLobbyR,getLobbyEventReceiveR)

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
       { outputFormat =
           if development
             then Detailed True
             else Apache FromSocket
       , destination = Logger $ appLogger foundation
       }

    app <- toWaiAppPlain foundation
    return $ logWare app

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager def
    s <- staticSite
    hconfig <- loadHerokuConfig
    dbconf <- withYamlEnvironment "config/postgres.yml" (appEnv conf)
              (Database.Persist.loadConfig . combineMappings hconfig) >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)
    chans <- newIORef []
    lchan <- newChan
    logger <- mkLogger True stdout
    let foundation = App conf s p manager dbconf logger lchan chans
    runResourceT $ runLoggingT
      (Database.Persist.runPool dbconf 
         (do runMigration migrateAll
             gms <- selectList ([] :: [Filter Game]) []
             mapM_ delete $ map entityKey gms)
         p)
      (messageLoggerSource foundation logger)
    return foundation

#ifndef DEVELOPMENT
canonicalizeKey :: (Text, val) -> (Text, val)
canonicalizeKey ("dbname", val) = ("database", val)
canonicalizeKey pair = pair

toMapping :: [(Text, Text)] -> AT.Value
toMapping xs = AT.Object $ H.fromList $ map (\(key, val) -> (key, AT.String val)) xs
#endif

combineMappings :: AT.Value -> AT.Value -> AT.Value
combineMappings (AT.Object m1) (AT.Object m2) = AT.Object $ m1 `H.union` m2
combineMappings _ _ = error "Data.Object is not a Mapping."

loadHerokuConfig :: IO AT.Value
loadHerokuConfig = do
#ifdef DEVELOPMENT
    return $ AT.Object H.empty
#else
    Web.Heroku.dbConnParams >>= return . toMapping . map canonicalizeKey
#endif


-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    do defaultDevelApp loader makeApplication
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }
