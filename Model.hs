module Model where

import Prelude
import Yesod
import Data.Text (Text,pack,unpack,append,intercalate)
import Safe (readMay)

import Database.Persist.Quasi
import Database.Persist.Store

import Data.Aeson.TH
import Data.Array.IO

import Control.Monad (forM)

import System.Random (randomRIO)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

data Color = Red | Blue | Green | Yellow | Purple
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

data Knowledge = Knowledge {knownColor :: Fact Color,
                            knownRank  :: Fact Rank}
  deriving (Show,Read,Eq)
derivePersistField "Knowledge"
$(deriveJSON (drop 5) ''Knowledge)

data Player = Player {playerName :: Text,
                      playerHand :: [(Card,Knowledge)]}
  deriving (Show,Read,Eq)
derivePersistField "Player"
$(deriveJSON (drop 6) ''Player)

data GameStatus = NotStarted | Running | EndsAfter Text | Done
  deriving (Show,Read,Eq)
derivePersistField "GameStatus"

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


----------------
--- Hanabi Logic

prettyNameList :: Game -> Text
prettyNameList g = intercalate ", " $ map playerName $ gamePlayers g

allCards :: [Card]
allCards = [Card col rank | col  <- [Red, Blue, Green, Yellow, Purple],
                            rank <- [One,   One,   One,
                                     Two,   Two,
                                     Three, Three,
                                     Four,  Four,
                                     Five]]

-- shuffle stolen from haskell wiki.  imperative, but O(n)
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArr ln xs
        forM [1..ln] $ \i -> do
            j <- randomRIO (i,ln)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    ln :: Int
    ln = length xs

    newArr :: Int -> [a] -> IO (IOArray Int a)
    newArr n l =  newListArray (1,n) l


deal :: [Player] -> IO ([Player],[Card])
deal players = 
  do deck <- shuffle allCards
     return $ dealToPlayers players deck
  where
    handSize :: Int
    handSize = if length players < 4 then 5 else 4

    dealToPlayers :: [Player] -> [Card] -> ([Player],[Card])
    dealToPlayers []     cds = ([],cds)
    dealToPlayers (p:ps) cds = ((p {playerHand = hand}):ps',cds')
      where
        hand :: [(Card,Knowledge)]
        hand = zip (take handSize cds)
                   (repeat $ Knowledge Mystery Mystery)

        (ps',cds') = dealToPlayers ps (drop handSize cds)
