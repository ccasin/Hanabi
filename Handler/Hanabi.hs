
module Handler.Hanabi where

import Import
import Safe (readMay)

import Data.Text (pack,unpack)
import Data.Maybe (isJust)
import Data.IORef
import Data.List (intercalate)

import Control.Monad (liftM,when)
import Control.Concurrent.Chan

import Blaze.ByteString.Builder.Char.Utf8 (fromText)

import Text.Julius (rawJS)

import Network.Wai.EventSource

----
---- TODO
----
---- - store gameid in session so you can reconnect after closing tab
---- - game implementation...


keyToInt :: GameId -> Int
keyToInt gid = 
  case fromPersistValue $ unKey gid of
    Left _  -> 0
    Right i -> i


-----------------------------
------ The channel map ------

getChannel :: GameId -> Handler (Chan ServerEvent)
getChannel gid =
  do iochans <- liftM gameChannels getYesod
     chans <- liftIO $ readIORef iochans
     case lookup gid chans of
       Just chan -> return chan
       Nothing   -> notFound
                    -- XXX make a new channel?  remove game from DB?

newChannel :: GameId -> Handler ()
newChannel gid =
    do iochans <- liftM gameChannels getYesod
       newc    <- liftIO newChan
       liftIO $ modifyIORef iochans ((gid,newc):)

--------------------------
------ Session stuff -----
sname :: Text
sname = "name"

sgameid :: Text
sgameid = "gameid" 


lookupGame :: Handler (Maybe GameId)
lookupGame = do 
  mgidt <- lookupSession sgameid
  return $ do gidt <- mgidt
              readMay $ unpack gidt

requireName :: Handler Text
requireName = do
  mname <- lookupSession sname
  case mname of
    Nothing -> redirect HanabiLobbyR
    Just n -> return n

requireGame :: Handler (GameId,Game)
requireGame = do 
  mgid <- lookupGame
  (gid,mgame) <- case mgid of
                   Nothing  -> redirect HanabiLobbyR
                   Just gid -> liftM (gid,) $ runDB $ get gid
  case mgame of
    Nothing -> do deleteSession sgameid
                  redirect HanabiLobbyR
    Just g -> return (gid,g)

nameForm :: Form Text
nameForm = renderDivs $ areq textField "Please pick a name" Nothing
--------------------------
--------------------------


getSetNameR :: Handler ()
getSetNameR = 
  do mname <- lookupSession sname
     when (isJust mname) $ redirect HanabiLobbyR
     ((result, _), _) <- runFormGet nameForm
     case result of
       FormSuccess nm -> setSession sname nm
       _ -> setMessage "Please enter a valid name."
     redirect HanabiLobbyR

postCreateHanabiR :: Handler ()
postCreateHanabiR = do
  nm     <- requireName
  gameId <- runDB $ insert $ Game False [Player nm []]
  setSession sgameid (pack $ show gameId)
  newChannel gameId
  redirect PlayHanabiR


data JoinFailure = Success | FailGameGone | FailGameStarted | FailGameFull
--- XXX CHECK IF PLAYER IS ALREADY IN GAME TO MAKE IDEMPOTENT             
   
postJoinHanabiR :: GameId -> Handler ()
postJoinHanabiR gid = do
  nm <- requireName
  res <- runDB $ do 
    mgame <- get gid
    case mgame of
      Nothing -> return FailGameGone
      Just (Game {gameActive=True             }) -> return FailGameStarted
      Just (Game {gameActive=False,gamePlayers}) ->
        if (length gamePlayers >= 5) then return FailGameFull
           else do update gid [GamePlayers =. ((Player nm []):gamePlayers)]
                   return Success
  case res of
    Success -> 
      do chan <- getChannel gid
         setSession sgameid (pack $ show gid)
         liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $ fromText nm
         redirect PlayHanabiR
    FailGameGone ->
      do setMessage 
           "Sorry, that game was cancelled by its creator.  Please pick another."
         redirect HanabiLobbyR
    FailGameStarted ->
      do setMessage "Sorry, that game just started.  Please pick another."
         redirect HanabiLobbyR
    FailGameFull ->
      do setMessage "Sorry, that game just filled up.  Please pick another."
         redirect HanabiLobbyR


getStartHanabiR :: Handler ()
getStartHanabiR = do 
  (gid,_) <- requireGame
  --- XXX also need to shuffle and deal
  runDB $ update gid [GameActive =. True]
  redirect PlayHanabiR


playerListWidget :: String -> GameId -> Widget
playerListWidget initPlayers gid =
  do playerList <- lift newIdent
     toWidgetBody [julius|
        var list = document.getElementById("#{rawJS playerList}");
        var src = new EventSource("@{GameEventReceiveR gid}");
        src.onmessage = function(msg) {
          var oldList = list.innerHtml
          list.innerHTML(oldList + ", " + msg);
        }; |]
     [whamlet| <p id=#{playerList}>#{initPlayers}|]

getPlayHanabiR :: Handler RepHtml
getPlayHanabiR = do
  (gid,game) <- requireGame
  let names = intercalate ", " $ map (\(Player n _) -> unpack n) $ gamePlayers game
  case gameActive game of
    False -> defaultLayout [whamlet|
        <p> This game hasn't started yet.  These players have joined:
        
        ^{playerListWidget names gid}
      |]
-- Still need to add back start game button
    True -> defaultLayout [whamlet|Well, the game started.  But I haven't implemented that yet.  Bummer.|]

getHanabiLobbyR :: Handler RepHtml
getHanabiLobbyR =
  do mname <- lookupSession sname
     mgid  <- lookupSession sgameid
     when (isJust mgid) $ redirect PlayHanabiR
     case mname of
       Nothing -> do 
         ((_, widget),enctype) <- generateFormGet nameForm
         defaultLayout [whamlet|
             <p>Welcome to Hanabi.

             <p>
               <form method=get action=@{SetNameR} enctype=#{enctype}>
                 ^{widget}
                 <input type=submit>
           |]
       Just nm -> do
         games <- runDB $ selectList [GameActive ==. False] []
         defaultLayout [whamlet|
             <p>Welcome to Hanabi, #{nm}

             $if null games
               <p>There are no games waiting for players.
             $else 
               <p>You can join a game or create a new one.  These games haven't started yet:
                  
               <ul>
                 $forall Entity gid g <- games
                   <li>
                     <form method=post action=@{JoinHanabiR gid}>
                        <input type=submit value="Game #{keyToInt gid}">
                     #{prettyNames g}

             <p>
               <form method=post action=@{CreateHanabiR}>
                  <input type=submit value="Create a new game">
          |]
  where
    prettyNames :: Game -> String
    prettyNames g = intercalate ", " $ map (\(Player n _) -> unpack n) $ gamePlayers g

getGameEventReceiveR :: GameId -> Handler RepHtml
getGameEventReceiveR gid = do
  chan0 <- getChannel gid
  chan <- liftIO $ dupChan chan0
  req <- waiRequest
  res <- lift $ eventSourceAppChan chan req
  sendWaiResponse res
