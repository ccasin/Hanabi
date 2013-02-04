module Handler.Hanabi where

import Import

import Data.Maybe (isJust)
import Data.IORef
import Data.Text (pack,append)
import qualified Data.Text as T (concat)

import Data.Aeson
import qualified Data.Aeson as J (Value)
import Data.Aeson.TH

import Control.Monad (liftM,when,unless)
import Control.Concurrent.Chan

import Blaze.ByteString.Builder (fromLazyByteString)

import Network.Wai.EventSource
import System.Random (randomRIO)
import Yesod.Auth

----  
---- TODO
----
---- FIX LEADER NONSENSE
----
---- - game implementation...

keyToInt :: GameId -> Int
keyToInt gid = 
  case fromPersistValue $ unKey gid of
    Left _  -> 0
    Right i -> i

-----------------------------
------ The channel map ------

getChannel :: Text -> Handler (Chan ServerEvent)
getChannel guid =
  do iochans <- liftM gameChannels getYesod
     chans <- liftIO $ readIORef iochans
     case lookup guid chans of
       Just chan -> return chan
       Nothing   -> notFound
           -- XXX make a new channel?  remove game from DB?

newChannel :: Handler Text
newChannel =
  do uniqueid <- liftIO $ liftM (pack . show) 
                        $ randomRIO (1 :: Int,1000000000)
     mdup <- runDB $ getBy (UniqueUid uniqueid)
     case mdup of
       Just _ -> newChannel
       Nothing -> do
         iochans  <- liftM gameChannels getYesod
         newc     <- liftIO newChan
         liftIO $ modifyIORef iochans ((uniqueid,newc):)
         return uniqueid


--------------------------
------ Session stuff -----
sgameid :: Text
sgameid = "gameid" 


lookupGame :: Handler (Maybe Text)
lookupGame = lookupSession sgameid

requireName :: Handler Text
requireName = do
  Entity _ user <- requireAuth
  case userName user of
    "" -> redirect SetNameR
    nm -> return nm

requireGame :: Handler Game
requireGame = do
  mguid <- lookupGame
  case mguid of
    Nothing -> redirect HanabiLobbyR
    Just guid -> do 
      mgame <- runDB $ getBy $ UniqueUid guid
      case mgame of
        Nothing -> do deleteSession sgameid
                      redirect HanabiLobbyR
        Just (Entity _ g) -> return g

requireGameTransaction :: (GameId -> Game -> YesodDB App App a) -> Handler a
requireGameTransaction trans = do
  mguid <- lookupGame
  res <- case mguid of 
           Nothing  -> redirect HanabiLobbyR
           Just guid -> runDB $ do
             mgame <- getBy $ UniqueUid guid
             case mgame of
               Nothing -> return Nothing
               Just (Entity gid g) -> liftM Just $ trans gid g
  case res of
    Nothing -> do deleteSession sgameid
                  redirect HanabiLobbyR
    Just g -> return g

nameForm :: Form Text
nameForm = renderDivs $ areq textField "" Nothing
--------------------------
--------------------------

--------------------------
----- EVENTS -------------

data GameListEvent = GLEAddGame   | GLEDeleteGame Bool -- Is this the last game?
                   | GLEUpdatePlayers
$(deriveJSON id ''GameListEvent)

data GameListMsg = GameListMsg {glmEType   :: GameListEvent,
                                glmGUID    :: Text,
                                glmPlayers :: [Player]}
$(deriveJSON (drop 3) ''GameListMsg)



data PlayerListEvent = PLEJoin | PLELeave
$(deriveJSON id ''PlayerListEvent)

pleJoin,pleLeave :: J.Value
pleJoin  = toJSON $ encode PLEJoin
pleLeave = toJSON $ encode PLELeave

data PlayerListMsg = PlayerListMsg {plmEType :: PlayerListEvent,
                                    plmPlayer :: Text,
                                    plmPlayers :: [Player]}
$(deriveJSON (drop 3) ''PlayerListMsg)
--------------------------
--------------------------

getSetNameR :: Handler RepHtml
getSetNameR =
  do Entity _ user <- requireAuth
     (widget,enctype) <- generateFormPost nameForm
     defaultLayout [whamlet|
       <p>Welcome to Hanabi <b>#{userCredID user}</b>.
            

       <p>Please chose a nickname.  This will be visible to other players.
                 
       <form method=post action=@{SetNameR} enctype=#{enctype}>
          ^{widget}
          <input type=submit value="Set nickname">
     |]

postSetNameR :: Handler ()
postSetNameR =
  do uid <- requireAuthId
     ((result, _), _) <- runFormPost nameForm
     case result of
       FormSuccess nm -> do 
         success <- runDB $ do 
           taken <- liftM isJust $ getBy $ UniqueName nm
           unless taken $ update uid [UserName =. nm]
           return $ not taken
         if success then redirect HanabiLobbyR
                    else do setMessage $ toHtml $
                                T.concat ["Sorry, ",nm," is taken."]
                            redirect SetNameR
       _ -> do setMessage "Please Enter a valid name."
               redirect SetNameR

postCreateHanabiR :: Handler ()
postCreateHanabiR = do
  nm     <- requireName
  guid   <- newChannel
  let newPlayer = Player nm []
  _      <- runDB $ insert $ Game False guid (playerName newPlayer) [newPlayer]
  setSession sgameid guid
  lobbyChan <- liftM lobbyChannel getYesod
  liftIO $ writeChan lobbyChan $ ServerEvent Nothing Nothing $ return $
     fromLazyByteString $ encode (GameListMsg GLEAddGame guid [newPlayer])
  redirect PlayHanabiR

data UnjoinResult =
    UnjoinSuccess {guid :: Text, players :: [Player]}
  | UnjoinFail
  | UnjoinEndGame {guid :: Text}

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
             return $ UnjoinEndGame (gameUid game)
        (False,False) ->
          let newPlayers = filter (\p -> nm /= playerName p) players in
          do update gid [GamePlayers =. newPlayers] -- XXX leader
             return (UnjoinSuccess (gameUid game) newPlayers)
    )
  case res of
    UnjoinSuccess guid ps -> 
      do deleteSession sgameid
         liftIO $ writeChan lobbyChan $ 
            ServerEvent Nothing Nothing $ return $ fromLazyByteString $ 
               encode (GameListMsg GLEUpdatePlayers guid ps)
         gchan <- getChannel guid
         liftIO $ writeChan gchan $ 
            ServerEvent Nothing Nothing $ return $ fromLazyByteString $ 
               encode (PlayerListMsg PLELeave nm ps)
         redirect HanabiLobbyR
    UnjoinFail         -> redirect PlayHanabiR -- XXX maybe I should alert the user or something
    UnjoinEndGame guid ->
      do deleteSession sgameid
         gamesLeft <- liftM null $ runDB $ selectList [GameActive ==. False] []
         liftIO $ writeChan lobbyChan $ ServerEvent Nothing Nothing $ return $ 
              fromLazyByteString 
              (encode (GameListMsg (GLEDeleteGame gamesLeft) guid []))
         redirect HanabiLobbyR

data JoinResult = JSuccess [Player]
                | JFailGameGone | JFailGameStarted | JFailGameFull
--- XXX CHECK IF PLAYER IS ALREADY IN GAME TO MAKE IDEMPOTENT             
   
postJoinHanabiR :: Text -> Handler ()
postJoinHanabiR guid = do
  nm <- requireName
  res <- runDB $ do 
    mgame <- getBy $ UniqueUid guid
    case mgame of
      Nothing -> return JFailGameGone
      Just (Entity _   (Game {gameActive=True             })) -> 
        return JFailGameStarted
      Just (Entity gid (Game {gameActive=False,gamePlayers})) ->
        if (length gamePlayers >= 5) then return JFailGameFull
           else let newPlayers = gamePlayers ++ [Player nm []] in
                do update gid [GamePlayers =. newPlayers]
                   return $ JSuccess newPlayers
  case res of
    JSuccess ps -> 
      do gchan <- getChannel guid
         lchan <- liftM lobbyChannel getYesod
         setSession sgameid guid
         liftIO $ writeChan gchan $ ServerEvent Nothing Nothing $ return $ 
               fromLazyByteString $ encode (PlayerListMsg PLEJoin nm ps)
         liftIO $ writeChan lchan $ ServerEvent Nothing Nothing $ return $ 
               fromLazyByteString $ 
               encode (GameListMsg GLEUpdatePlayers guid ps)
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
  -- XXX send event message to all players to start game
  requireGameTransaction (\gid _ -> 
    --- XXX also need to shuffle and deal
    update gid [GameActive =. True])
  redirect PlayHanabiR


playerListWidget :: Text -> Text -> Widget
playerListWidget initPlayers guid =
  do playerList <- lift newIdent
     mbox <- lift newIdent
     [whamlet|
       <p id=#{mbox}>
       <p id=#{playerList}>#{initPlayers}
       
       <table>
         <tr>
           <td>
             <form method=get action=@{StartHanabiR}>
               <input type=submit value="Start the game">
           <td>
             <form method=post action=@{UnjoinHanabiR}>
               <input type=submit value="Leave the game">
                       
     |] -- XXX only "leader" should be allowed to start
        -- XXX grey out start box
     toWidgetBody [julius|
        var list = document.getElementById(#{toJSON playerList});
        var mbox = document.getElementById(#{toJSON mbox});
        var src = new EventSource("@{GameEventReceiveR guid}");
        src.onmessage = function(msg) {
          var event = JSON.parse(msg.data);
          var etype = event.EType;
          if ("PLEJoin" in etype) {
            mbox.innerHTML = event.Player + " joined the game.";
          } else if ("PLELeave" in etype) {
            mbox.innerHTML = event.Player + " left the game.";
          }

          list.innerHTML = displayPlayerList(event.Players);
        }; |]

getPlayHanabiR :: Handler RepHtml
getPlayHanabiR = do
  game <- requireGame
  let names = prettyNameList game
  case gameActive game of
    False -> defaultLayout [whamlet|
        <p> This game hasn't started yet.  These players have joined:
        
        ^{playerListWidget names $ gameUid game}
      |]
    -- Still need to add back start game button
    True -> defaultLayout [whamlet|Well, the game started.  But I haven't implemented that yet.  Bummer.|]

gameListWidget :: [Entity Game] -> Widget
gameListWidget games = do
  do glistI <- lift newIdent
     instructionsI <- lift newIdent
     gplayersC <- lift newIdent
     let nogames,somegames :: Text
         nogames = "There are no games waiting for players."
         somegames = (         "You can join a game or create a new one.  "
                      `append` "These games haven't started yet:")
     toWidgetHead [lucius|
        .newGame {
          display: none;
        }
      |]
     [whamlet|
       <p id=#{instructionsI}>
         $if null games
           #{nogames}
         $else 
           #{somegames}
            
       <ul id=#{glistI}>
         $forall Entity gid g <- games
           <li id="#{gameUid g}">
             <form method=post action=@{JoinHanabiR (gameUid g)}>
                <input type=submit value="Game #{keyToInt gid}">
             <p class=#{gplayersC}>
               #{prettyNameList g}
       |]
     toWidgetBody [julius|
        var src = new EventSource("@{LobbyEventReceiveR}");
        src.onmessage = function(msg) {
          var event = JSON.parse(msg.data);
          var etype = event.EType;
          if ("GLEAddGame" in etype) {
            var newli = $('<li>');
            var newform = $('<form>');
            var newbutton = $('<input>');
            var newp = $('<p>');

            $(newli).attr('id',event.GUID);
            $(newli).attr('class','newGame');

            $(newform).attr('method','post');
            $(newform).attr('action','joinhanabi/'+event.GUID);

            $(newbutton).attr('type','submit');
            $(newbutton).attr('value','Game '+event.GUID);
            
            $(newp).attr('class',#{toJSON gplayersC});
            $(newp).append(displayPlayerList(event.Players));

            $(newform).append(newbutton);
            $(newli).append(newform).append(newp);
            $("#"+#{toJSON instructionsI}).text(#{toJSON somegames});
            $("#"+#{toJSON glistI}).prepend(newli);
            $(newli).slideDown(400,function() {$(newli).removeClass();});
          } else if ("GLEDeleteGame" in etype) { -- XXX
            if (etype.GLEDeleteGame) {
              $("#"+#{toJSON instructionsI}).text(#{toJSON nogames});
            };
            $("#"+event.GUID).fadeOut();
          } else if ("GLEUpdatePlayers" in etype) {
            $("#"+event.GUID+">."+#{toJSON gplayersC}).
                 text(displayPlayerList(event.Players));
          }
        }; |]


getHanabiLobbyR :: Handler RepHtml
getHanabiLobbyR =
  do nm <- requireName
     mguid <- lookupSession sgameid
     when (isJust mguid) $ redirect PlayHanabiR
     games <- runDB $ selectList [GameActive ==. False] []
     defaultLayout [whamlet|
         <p>Welcome to Hanabi, #{nm}
                    
         ^{gameListWidget games}

         <p>
           <form method=post action=@{CreateHanabiR}>
              <input type=submit value="Create a new game">
      |]

getGameEventReceiveR :: Text -> Handler ()
getGameEventReceiveR guid = do
  chan0 <- getChannel guid
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
