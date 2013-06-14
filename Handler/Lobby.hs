module Handler.Lobby
  (getHanabiLobbyR,getLobbyEventReceiveR)
where

import Data.Maybe (isJust)
import Data.Text (append)

import Control.Monad (when,liftM)
import Control.Concurrent.Chan

import Network.Wai.EventSource
import Import

import Handler.Infrastructure

gameListWidget :: [Entity Game] -> Widget
gameListWidget games = do
  do glistI <- newIdent
     instructionsI <- newIdent
     gplayersC <- newIdent
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
         $forall Entity _ g <- games
           <li id="#{gameUid g}">
             <form method=post action=@{JoinHanabiR (gameUid g)}>
                <input type=submit value="Game #{gameUid g}">
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
          } else if ("GLEDeleteGame" in etype) {
            if (etype.GLEDeleteGame) {
              $("#"+#{toJSON instructionsI}).text(#{toJSON nogames});
            };
            var gamediv = $("#"+event.GUID);
            gamediv.fadeOut("slow",function () {gamediv.remove()});
          } else if ("GLEUpdatePlayers" in etype) {
            $("#"+event.GUID+">."+#{toJSON gplayersC}).
                 text(displayPlayerList(event.Players));
          }
        }; |]


getHanabiLobbyR :: Handler Html
getHanabiLobbyR =
  do nm <- requireName
     mguid <- lookupSession sgameid
     when (isJust mguid) $ redirect PlayHanabiR
     games <- runDB $ selectList [GameStatus ==. NotStarted] []
     defaultLayout [whamlet|
         <p>Welcome to Hanabi, #{nm}.
                    
         ^{gameListWidget games}

         <p>
           <form method=post action=@{CreateHanabiR}>
              <input type=submit value="Create a new game">
      |]


-----------------------------------------
----- Event urls ------------------------
getLobbyEventReceiveR :: Handler ()
getLobbyEventReceiveR = do
  chan0 <- liftM lobbyChannel getYesod
  chan <- liftIO $ dupChan chan0
  req <- waiRequest
  res <- liftResourceT $ eventSourceAppChan chan req
  sendWaiResponse res
