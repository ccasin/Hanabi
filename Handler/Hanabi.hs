module Handler.Hanabi where

import Import
import Safe (readMay)

import Data.Text (pack,unpack)
import Data.Maybe (isJust)
import Data.IORef
import Data.List (intercalate)

import Data.Aeson
import Data.Aeson.TH

import Control.Monad (liftM,when)
import Control.Concurrent.Chan

import Blaze.ByteString.Builder (fromLazyByteString)

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
  case mgid of
    Nothing -> redirect HanabiLobbyR
    Just gid -> do 
      mgame <- runDB $ get gid
      case mgame of
        Nothing -> do deleteSession sgameid
                      redirect HanabiLobbyR
        Just g -> return (gid,g)

requireGameTransaction :: (GameId -> Game -> YesodDB App App a) -> Handler a
requireGameTransaction trans = do
  mgid <- lookupGame
  res <- case mgid of 
           Nothing  -> redirect HanabiLobbyR
           Just gid -> runDB $ do
             mgame <- get gid
             case mgame of
               Nothing -> return Nothing
               Just g -> liftM Just $ trans gid g
  case res of
    Nothing -> do deleteSession sgameid
                  redirect HanabiLobbyR
    Just g -> return g

nameForm :: Form Text
nameForm = renderDivs $ areq textField "Please pick a name" Nothing
--------------------------
--------------------------

--------------------------
----- EVENTS -------------

data GameListEvent = GLEAddGame   | GLEDeleteGame
                   | GLEUpdatePlayers
$(deriveJSON id ''GameListEvent)

data GameListMsg = GameListMsg {glmEType   :: GameListEvent,
                                glmGID     :: Text,
                                glmPlayers :: [Text]}
$(deriveJSON (drop 3) ''GameListMsg)



data PlayerListEvent = PLEJoin | PLELeave
$(deriveJSON id ''PlayerListEvent)

data PlayerListMsg = PlayerListMsg {plmEType :: PlayerListEvent,
                                    plmPlayer :: Text,
                                    plmPlayers :: [Text]}
$(deriveJSON (drop 3) ''PlayerListMsg)
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

data UnjoinResult = UnjoinSuccess | UnjoinFail | UnjoinEndGame GameId

postUnjoinHanabiR :: Handler ()
postUnjoinHanabiR = do
  nm  <- requireName
  lobbyChan <- liftM lobbyChannel getYesod
  res <- requireGameTransaction (\gid game ->
      let players = gamePlayers game in
      case (gameActive game, length players <= 1) of
        (True, _    ) -> return UnjoinFail -- unjoin is only for games that haven't started
        (False,True ) -> 
          do delete gid 
             return $ UnjoinEndGame gid
        (False,False) ->
          do update gid [GamePlayers =. filter (\(Player nm' _) -> nm /= nm') players]
             return UnjoinSuccess
    )
  case res of
    UnjoinSuccess     -> do deleteSession sgameid
                            redirect HanabiLobbyR -- XXX the lobby should update
    UnjoinFail        -> redirect PlayHanabiR -- XXX maybe I should alert the user or something
    UnjoinEndGame gid ->
      do liftIO $ writeChan lobbyChan $ ServerEvent Nothing Nothing $ return $ 
              fromLazyByteString $ encode (GameListMsg GLEDeleteGame ((pack . show . keyToInt) gid) [])
         redirect HanabiLobbyR

data JoinResult = JSuccess [Text]
                | JFailGameGone | JFailGameStarted | JFailGameFull
--- XXX CHECK IF PLAYER IS ALREADY IN GAME TO MAKE IDEMPOTENT             
   
postJoinHanabiR :: GameId -> Handler ()
postJoinHanabiR gid = do
  nm <- requireName
  res <- runDB $ do 
    mgame <- get gid
    case mgame of
      Nothing -> return JFailGameGone
      Just (Game {gameActive=True             }) -> return JFailGameStarted
      Just (Game {gameActive=False,gamePlayers}) ->
        if (length gamePlayers >= 5) then return JFailGameFull
           else do update gid [GamePlayers =. ((Player nm []):gamePlayers)]
                   return $ JSuccess $ nm : map (\(Player nm' _) -> nm') gamePlayers
  case res of
    JSuccess nms -> 
      do chan <- getChannel gid
         setSession sgameid (pack $ show gid)
         liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $ 
              fromLazyByteString $ encode (PlayerListMsg PLEJoin nm nms)
         redirect PlayHanabiR
    JFailGameGone -> -- XXX use somethign better than setmessage
      do setMessage 
           "Sorry, that game was cancelled by its creator.  Please pick another."
         redirect HanabiLobbyR
    JFailGameStarted ->
      do setMessage "Sorry, that game just started.  Please pick another."
         redirect HanabiLobbyR
    JFailGameFull ->
      do setMessage "Sorry, that game just filled up.  Please pick another."
         redirect HanabiLobbyR


getStartHanabiR :: Handler ()
getStartHanabiR = do 
  requireGameTransaction (\gid _ -> 
    --- XXX also need to shuffle and deal
    update gid [GameActive =. True])
  redirect PlayHanabiR


playerListWidget :: String -> GameId -> Widget
playerListWidget initPlayers gid =
  do playerList <- lift newIdent
     mbox <- lift newIdent
     [whamlet|
       <p id=#{mbox}>
       <p id=#{playerList}>#{initPlayers}
       
       <form method=post action=@{UnjoinHanabiR}>
          <input type=submit value="Leave this game">
     |]
     toWidgetBody [julius|
        var list = document.getElementById(#{toJSON playerList});
        var mbox = document.getElementById(#{toJSON mbox});
        var src = new EventSource("@{GameEventReceiveR gid}");
        src.onmessage = function(msg) {
          var event = JSON.parse(msg.data);
          var etype = event.EType;
          if ("PLEJoin" in etype) {
            mbox.innerHTML = event.Player + " joined the game.";
          } else if ("PLELeft" in etype) {
            mbox.innerHTML = event.Player + " left the game.";
          }

          var listHTML = "";
          for (i=0;i<event.Players.length;i++) {
            if (i > 0) {listHTML += ", ";};
            listHTML += event.Players[i];
          }
          list.innerHTML = listHTML;
        }; |]

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

gameListWidget :: [Entity Game] -> Widget
gameListWidget games = do
  do [whamlet|
       $if null games
         <p>There are no games waiting for players.
       $else 
         <p>You can join a game or create a new one.  These games haven't started yet:
            
         <ul>
           $forall Entity gid g <- games
             <li id="#{keyToInt gid}">
               <form method=post action=@{JoinHanabiR gid}>
                  <input type=submit value="Game #{keyToInt gid}">
               #{prettyNames g}
       |]
     toWidgetBody [julius|
        var src = new EventSource("@{LobbyEventReceiveR}");
        src.onmessage = function(msg) {
          var event = JSON.parse(msg.data);
          var etype = event.EType;
          if ("GLEAddGame" in etype) {
            // XXX unimplemented
          } else if ("GLEDeleteGame" in etype) {
            $("#"+event.GID).fadeOut();
          } else if ("GLEUpdatePlayers" in etype) {
            // XXX unimplemented
          }
        }; |]

  where
    prettyNames :: Game -> String
    prettyNames g = intercalate ", " $ map (\(Player n _) -> unpack n) $ gamePlayers g


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
                        
             ^{gameListWidget games}

             <p>
               <form method=post action=@{CreateHanabiR}>
                  <input type=submit value="Create a new game">
          |]

getGameEventReceiveR :: GameId -> Handler ()
getGameEventReceiveR gid = do
  chan0 <- getChannel gid
  chan <- liftIO $ dupChan chan0
  req <- waiRequest
  res <- lift $ eventSourceAppChan chan req
  sendWaiResponse res

getLobbyEventReceiveR :: Handler ()
getLobbyEventReceiveR = do
  chan0 <- liftM lobbyChannel getYesod
  chan <- liftIO $ dupChan chan0
  req <- waiRequest
  res <- lift $ eventSourceAppChan chan req
  sendWaiResponse res
