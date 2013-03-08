module Model where

import Prelude
import Yesod
import Data.Text (Text,pack,unpack,append,intercalate)
import qualified Data.Text as T (concat)
import Safe (readMay)

import Database.Persist.Quasi
import Database.Persist.Store

import Data.Aeson.TH
import Data.Array.IO

import Control.Monad.Error

import System.Random (randomRIO)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

data Color = Red | Blue | Green | Yellow | Pink
    deriving (Show,Read,Eq,Enum,Bounded)
derivePersistField "Color"
$(deriveJSON id ''Color)

describeColor :: Color -> Text
describeColor Red    = "red (circle)"
describeColor Blue   = "blue (triangle)"
describeColor Green  = "green (diamond)"
describeColor Yellow = "yellow (square)"
describeColor Pink   = "pink (star)"

data Rank  = One | Two | Three | Four | Five
    deriving (Show,Read,Eq,Enum,Bounded)
derivePersistField "Rank"
$(deriveJSON id ''Rank)

describeRank :: Rank -> Text
describeRank One = "1"
describeRank Two = "2"
describeRank Three = "3"
describeRank Four = "4"
describeRank Five = "5"

data Card  = Card {cardColor :: Color, cardRank :: Rank}
    deriving (Show,Read,Eq)
derivePersistField "Card"
$(deriveJSON (drop 4) ''Card)

describeCard :: Card -> Text
describeCard (Card {cardColor,cardRank})
  = T.concat [describeColor cardColor," ",describeRank cardRank]

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

data Player = Player {playerName   :: Text,
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

share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


----------------
--- Hanabi Logic

-- a function for working with association lists
--   use the provided function to update a value.  function must
--   provide a default value if passed Nothing, in case the key is missing
updateAL :: Eq a => [(a,b)] -> a -> (Maybe b -> b) -> [(a,b)]
updateAL []           k v = [(k,v Nothing)]
updateAL ((k',v'):al) k v = 
  if k == k' 
    then ((k,v $ Just v'):al)
    else (k',v'):(updateAL al k v)

nextPlayer :: Game -> Int -> Int
nextPlayer gm p = (p+1) `mod` (length $ gamePlayers gm)

updatePlayer :: MonadError String m => 
                   [Player] -> Int -> (Player -> m (a,Player))
                -> m (a,[Player])
updatePlayer []     _ _ = throwError "updatePlayer: not enough players"
updatePlayer (p:ps) 0 f = 
  do (x,p') <- f p
     return (x,p':ps)
updatePlayer (p:ps) n f = 
  do (x,ps') <- updatePlayer ps (n-1) f
     return $ (x, p:ps')

-- this draws a card if possible.  It also advances the game, setting next
-- player and ending game if necessary.
drawCard :: MonadError String m => Game -> m (Game,Maybe Card)
drawCard gm =
  case gameStatus gm of
    gs@(Running {currentP=cp,finalP=mfp}) ->
      case mfp of
        Just fp -> 
          return (if fp == cp
                      then gm {gameStatus=Done}
                      else gm {gameStatus=gs {currentP=next}},
                  Nothing)
        Nothing ->
          case gameDeck gm of
            [] ->
              return (gm {gameStatus=Running {currentP=next,
                                              finalP=Just cp}},
                      Nothing)
            (c:cs) ->
              do (_,players) <- updatePlayer (gamePlayers gm) cp
                                (\p -> return $ ( (),
                                   p {playerHand=playerHand p 
                                              ++ [(c,Knowledge Mystery Mystery)]}))
                 return (gm {gameStatus=gs {currentP=next},
                             gameDeck=cs,
                             gamePlayers=players},
                         Just c)
      where
        next :: Int
        next = nextPlayer gm cp
    _ -> throwError "drawCard: Attempted to draw a card from a non-running game."

-- Int input is the position in hand of the discarded card.
-- Returns the updated game, the discarded card, and the drawn card, if there is one.
discard :: MonadError String m => Game -> Int -> m (Game,Card,Maybe Card)
discard gm cnum =
  case gameStatus gm of
    (Running {currentP = cp}) ->
      do (oldcard,players) <-
           updatePlayer (gamePlayers gm) cp 
             (\p -> do ((c,_),cs) <- removeNth (playerHand p) cnum
                       return (c,p {playerHand=cs}))
         let discards = addDiscard (gameDiscards gm) oldcard
         (game,newcard) <- drawCard (gm {gamePlayers=players,
                                         gameDiscards=discards})
         return (game,oldcard,newcard)
    _ -> throwError "discard: called during non-running game"

  where
    removeNth :: MonadError String m => [a] -> Int -> m (a,[a])
    removeNth []     _ = throwError "discard: removeNth out of bounds"
    removeNth (x:xs) 0 = return (x,xs)
    removeNth (x:xs) n = 
      do (nth,xs') <- removeNth xs (n-1)
         return (nth,x:xs')

    addDiscard :: Discards -> Card -> Discards
    addDiscard (Discards ds) (Card {cardColor,cardRank}) =
      Discards $ updateAL ds cardColor $
        maybe [(cardRank,1)]
              (\rs -> updateAL rs cardRank (maybe 1 succ))

prettyNameList :: Game -> Text
prettyNameList g = intercalate ", " $ map playerName $ gamePlayers g

allCards :: [Card]
allCards = [Card col rank | col  <- [Red, Blue, Green, Yellow, Pink],
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
