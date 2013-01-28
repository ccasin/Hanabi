module Model where

import Prelude
import Yesod
import Data.Text (Text,pack,unpack,append,intercalate)
import Safe (readMay)

import Database.Persist.Quasi
import Database.Persist.Store

import Data.Aeson.TH

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

data Color = Red | Blue | Green | Yellow | Black
    deriving (Show,Read,Eq)
derivePersistField "Color"
$(deriveJSON id ''Color)

data Rank  = One | Two | Three | Four | Five
    deriving (Show,Read,Eq)
derivePersistField "Rank"
$(deriveJSON id ''Rank)

data Card  = Card {cardColor :: Color, cardRank :: Rank}
    deriving (Show,Read,Eq)
derivePersistField "Card"
$(deriveJSON (drop 4) ''Card)

data Fact a = Mystery | Isnt [a] | Is a
    deriving (Show,Read,Eq)
$(deriveJSON id ''Fact)

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
$(deriveJSON id ''Knowledge)

data Player = Player {playerName :: Text,
                      playerHand :: [(Card,Knowledge)]}
  deriving (Show,Read,Eq)
derivePersistField "Player"
$(deriveJSON (drop 6) ''Player)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

prettyNameList :: Game -> Text
prettyNameList g = intercalate ", " $ map (\(Player n _) -> n) $ gamePlayers g
