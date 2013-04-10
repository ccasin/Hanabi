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
import Data.Maybe (fromMaybe)
import Data.List ((\\))

import Control.Monad.Error

import System.Random (randomRIO)


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/

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

prettyColor :: Color -> HtmlUrl a
prettyColor a = [hamlet| foo |]

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

describeCard :: Card -> Text
describeCard (Card {cardColor,cardRank})
  = T.concat [describe cardColor," ",describe cardRank]

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

getDiscards :: Game -> Color -> [(Rank,Int)]
getDiscards (Game {gameDiscards=Discards d}) c = 
  fromMaybe [] (lookup c d)
  {- fmap (sortBy (\a b -> compare (fst a) (fst b))) $ -}



----------------
--- Hanabi Logic

scoreBoard :: Board -> Int
scoreBoard (Board b) = sum $ map (\(_,r) -> rankScore r) b

scoreGame :: Game -> Int
scoreGame game = scoreBoard $ gameBoard game

maxHints :: Int
maxHints = 8

maxStrikes :: Int
maxStrikes = 2

removeNth :: MonadError String m => [a] -> Int -> m (a,[a])
removeNth []     _ = throwError "removeNth out of bounds"
removeNth (x:xs) 0 = return (x,xs)
removeNth (x:xs) n = 
  do (nth,xs') <- removeNth xs (n-1)
     return (nth,x:xs')

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


addDiscard :: Discards -> Card -> Discards
addDiscard (Discards ds) (Card {cardColor,cardRank}) =
  Discards $ updateAL ds cardColor $
    maybe [(cardRank,1)]
          (\rs -> updateAL rs cardRank (maybe 1 succ))

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
             hints = 1 + gameHints gm 
         (game,newcard) <- drawCard (gm {gamePlayers=players,
                                         gameDiscards=discards,
                                         gameHints=hints})
         return (game,oldcard,newcard)
    _ -> throwError "discard: called during non-running game"

-- Int input is the position of the played card.
-- Returns the updated game, whether the play was successful,
-- the played card, and the drawn card, if there is one.
play :: MonadError String m => Game -> Int -> m (Game,Bool,Card,Maybe Card)
play gm cnum =
  case gameStatus gm of
    (Running {currentP = cp}) ->
      do (playedcard,players) <-
           updatePlayer (gamePlayers gm) cp
             (\p -> do ((c,_),cs) <- removeNth (playerHand p) cnum
                       return (c,p {playerHand=cs}))
         let (success,discards,board) = case attemptPlay gm playedcard of
               Right ds -> (False, ds, gameBoard gm)
               Left bd -> (True, gameDiscards gm, bd)
             strikes = (if success then id else (1+)) $ gameStrikes gm
             hints = (if success && (cardRank playedcard == Five)
                         then (+1) else id) $ gameHints gm
         if (strikes > maxStrikes || scoreBoard board == 25)
           then return (gm {gameStrikes=strikes,gameStatus=Done,
                            gameDiscards=discards,gameBoard=board,
                            gamePlayers=players},
                        success,playedcard,Nothing)
           else do
             (game,newcard) <- drawCard (gm {gameStrikes=strikes,
                                             gameDiscards=discards,
                                             gamePlayers=players,
                                             gameHints=hints,
                                             gameBoard=board})
             return (game,success,playedcard,newcard)
    _ -> throwError "play: called during non-running game"
  where
    nextRank :: Maybe Rank -> Rank -> Bool
    nextRank Nothing      One   = True
    nextRank (Just One)   Two   = True
    nextRank (Just Two)   Three = True
    nextRank (Just Three) Four  = True
    nextRank (Just Four)  Five  = True
    nextRank _            _     = False

    attemptPlay :: Game -> Card -> Either Board Discards
    attemptPlay (Game {gameBoard=Board board,gameDiscards}) card =
      if nextRank (lookup color board) rank
         then Left $ Board $ updateAL board color (const rank)
         else Right $ addDiscard gameDiscards card

      where
        color = cardColor card
        rank  = cardRank card

-- Takes in the player the hint is being given to and what the hint is.
--
-- Returns 
--   1) the updated game
--   2) the hinted player (convenient so I don't have to find him again later)
--   3) a list indicating which of the player's cards have been singled out
--      by this hint (convenient for updating the view)
hint :: MonadError String m => Game -> Int -> Either Color Rank
     -> m (Game,Player,[Bool])
hint gm hp hintinfo = 
  case (gameStatus gm, hints > 0) of
    (gs@Running {currentP = cp},True) ->
       do (upCards,players) <-
              updatePlayer (gamePlayers gm) hp (return . updateP)
          return (gm {gameStatus=gs {currentP=nextPlayer gm cp}
                     ,gameHints = hints - 1
                     ,gamePlayers=players},
                  players !! hp,
                  upCards)
    (_,False) -> throwError "hint: no hints left"
    (_,_) -> throwError "hint: called during non-running game"
  where
    hints = gameHints gm

    updateP :: Player -> ([Bool],Player)
    updateP p = (map snd newKnowledge, p {playerHand=newHand})
      where
        oldHand,newHand :: [(Card,Knowledge)]
        newKnowledge    :: [(Knowledge,Bool)]
        oldHand      = playerHand p
        newKnowledge = updateHand oldHand
        newHand      = zipWith (\(c,_) (k,_) -> (c,k)) oldHand newKnowledge

    -- bool indicates whether the card was singled out by the hint
    updateHand :: [(Card,Knowledge)] -> [(Knowledge,Bool)]
    updateHand = map $ uncurry updateCard

    -- Given a card and what we used to know about it, return the updated
    -- knowledge and whether this card was pointed out in the hint.
    updateCard :: Card -> Knowledge -> (Knowledge,Bool)
    updateCard card k =
      case hintinfo of
        Left  c -> let (f,b) = updateK (cardColor card) c (knownColor k) in
                     (k {knownColor=f},b)
        Right r -> let (f,b) = updateK (cardRank card) r (knownRank k) in
                     (k {knownRank=f},b)
      where
        updateK :: Hintable a => a -> a -> Fact a -> (Fact a,Bool)
        updateK carda hinta knowl =
          (if carda == hinta
             then Is carda
             else case knowl of 
               Is a    -> Is a
               Mystery -> Isnt [hinta]
               Isnt as -> case (allHints \\ (hinta:as), hinta `elem` as) of
                            ([a'],_)  -> Is a'
                            (_,True)  -> Isnt as
                            (_,False) -> Isnt (hinta:as)
          ,carda == hinta)

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
