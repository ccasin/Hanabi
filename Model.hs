module Model where

import Prelude
import Yesod
import Data.Text (Text,pack,unpack,append)
import Safe (readMay)

import Database.Persist.Quasi
import Database.Persist.Store


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

data Color = Red | Blue | Green | Yellow | Black
    deriving (Show,Read,Eq)
derivePersistField "Color"

data Rank  = One | Two | Three | Four | Five
    deriving (Show,Read,Eq)
derivePersistField "Rank"

data Card  = Card Color Rank
    deriving (Show,Read,Eq)
derivePersistField "Card"

data Fact a = Mystery | Isnt [a] | Is a
    deriving (Show,Read,Eq)

instance (Show a,Read a) => PersistField (Fact a) where
    toPersistValue f = PersistText (pack $ show f)

    fromPersistValue (PersistText ft) =
        case readMay (unpack ft) of
          Nothing -> Left (append (append "Error: \"Fact\" " ft)
                                  " can't be read.")
          Just f  -> Right f
    fromPersistValue _ = Left "Error: \"Fact\"s in wrong database format."

    sqlType _ = SqlString

data Knowledge = Knowledge (Fact Color) (Fact Rank)
  deriving (Show,Read,Eq)
derivePersistField "Knowledge"

data Player = Player Text [(Card,Knowledge)]
  deriving (Show,Read,Eq)
derivePersistField "Player"

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
