module ModelTypes where

import Prelude

import Data.Text (Text,pack,unpack,append)

import Safe (readMay)

import Yesod
import Data.Aeson.TH



class Eq a => Hintable a where
  describe :: a -> Text
  allHints :: [a]

data Color = Red | Blue | Green | Yellow | Pink
    deriving (Show,Read,Eq,Enum,Bounded,Ord)
derivePersistField "Color"
$(deriveJSON id ''Color)

instance Hintable Color where
  allHints = [Red,Blue,Green,Yellow,Pink]

  describe Red    = "red (circle)"
  describe Blue   = "blue (triangle)"
  describe Green  = "green (diamond)"
  describe Yellow = "yellow (square)"
  describe Pink   = "pink (star)"

instance PathPiece Color where
  toPathPiece = pack . show
  fromPathPiece = readMay . unpack

data Rank  = One | Two | Three | Four | Five
    deriving (Show,Read,Eq,Enum,Bounded,Ord)
derivePersistField "Rank"
$(deriveJSON id ''Rank)

instance Hintable Rank where
  allHints = [One,Two,Three,Four,Five]

  describe One = "1"
  describe Two = "2"
  describe Three = "3"
  describe Four = "4"
  describe Five = "5"

instance PathPiece Rank where
  toPathPiece = pack . show
  fromPathPiece = readMay . unpack

rankScore :: Rank -> Int
rankScore One   = 1
rankScore Two   = 2
rankScore Three = 3
rankScore Four  = 4
rankScore Five  = 5



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

data Knowledge = Knowledge {knownColor :: Fact Color,
                            knownRank  :: Fact Rank}
  deriving (Show,Read,Eq)
derivePersistField "Knowledge"
$(deriveJSON (drop 5) ''Knowledge)

data Player = Player {playerName   :: Text,
                      playerNum    :: Int,
                      playerHand   :: [(Card,Knowledge)],
                      playerChanId :: Text}
  deriving (Show,Read,Eq)
derivePersistField "Player"
$(deriveJSON (drop 6) ''Player)


data GameStatus = NotStarted | Running {currentP :: Int, -- index into player list
                                        finalP :: Maybe Int}
                             | Done
  deriving (Show,Read,Eq)
derivePersistField "GameStatus"

data Board = Board [(Color,Rank)]
  deriving (Show,Read)

instance Eq Board where
  (Board b1) == (Board b2) =
      all (\c -> lookup c b1 == lookup c b2) colors
    where
      colors = [minBound .. maxBound]
derivePersistField "Board"

data Discards = Discards [(Color,[(Rank,Int)])]
  deriving (Show,Read)
derivePersistField "Discards"

-- This stores a little more information than is really necessary to recreate the
-- actions.  It's just convenient for generating text descriptions of what happened
-- without reimplementing the actions themselves.
data ActionLog = ALPlay {alPlayPlayer  :: Int,
                         alPlayCard    :: Card,
                         alPlayCardPos :: Int,
                         alPlaySuccess :: Bool,
                         alPlayNewCard :: Maybe Card}
               | ALDisc {alDiscPlayer  :: Int,
                         alDiscCard    :: Card,
                         alDiscCardPos :: Int,
                         alDiscNewCard :: Maybe Card}
               | ALHint {alHintHinter  :: Int,
                         alHintHinted  :: Int,
                         alHintType    :: Either Color Rank}
  deriving (Show,Read)
derivePersistField "ActionLog"
$(deriveJSON (drop 4) ''ActionLog)
