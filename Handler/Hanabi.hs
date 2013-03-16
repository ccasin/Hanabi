module Handler.Hanabi where

import Import

import Data.Maybe (isJust)
import Data.IORef
import Data.Text (pack,append,strip)
import qualified Data.Text as T (concat,length)

import Data.Aeson hiding (object)
import Data.Aeson.TH

import Control.Monad (liftM,when,unless)
import Control.Concurrent.Chan

import Blaze.ByteString.Builder (fromLazyByteString)

import System.Random (randomRIO)
import Data.List (foldl',(\\),find)
-- import qualified Data.Traversable as Trav (mapM)

import Yesod.Auth
import Network.Wai.EventSource

----  
---- TODO
----
---- - game implementation...
----
---- - xxx can't change name while playing
---- - xxx dummy auth doesn't handle non-unique names right, and doens't restrict name length.
---- - xxx clean out dummy auths on startup? (timeout)
---- - xxx names with special characters are not handled correctly
----            are things being preescaped by form/db insertion and then it's messed up?
---- - xxx figure out when to delete channels
---- - xxx it would be "easy" for one player to listen to another player's events
---- - xxx you shouldn't be able to discard when you already have max hints
---- - xxx log errors
---- - xxx log actions


keyToInt :: GameId -> Int
keyToInt gid = 
  case fromPersistValue $ unKey gid of
    Left _  -> 0
    Right i -> i

toJSONT :: Text -> Value
toJSONT = toJSON

------------------------------------
------ Some constant strings -------

-- object fields

cardField, messagesField, errorField, newcardField, playerField, discardField, replacecardField  :: Text
cardField        = "card"           -- Int
playerField      = "player"    -- Int

discardField     = "discard"        -- object (cardField,playerField,newcardfield (opt))

messagesField    = "msgs"            -- [String]
errorField       = "error"          -- String

newcardField     = "newcard"        -- Route
replacecardField = "replacecard"    -- Bool


-- css ids
cardRowID, colorRowID, rankRowID :: Text
cardRowID  = "CardRow"
colorRowID = "ColorRow"
rankRowID  = "RankRow"



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

deleteChan :: Text -> Handler ()
deleteChan nm =
  do iochans <- liftM gameChannels getYesod
     liftIO $ modifyIORef iochans $
       foldl' (\cs (nm',c) -> if nm == nm' then cs else (nm',c):cs)
              []

sendMessage :: ToJSON a => Chan ServerEvent -> a -> Handler ()
sendMessage chan msg = liftIO $ writeChan chan 
                     $ ServerEvent Nothing Nothing 
                       [fromLazyByteString $ encode msg]

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
nameForm = renderDivs $ areq textField "" {fsAttrs = [("maxLength","20")]} Nothing
--------------------------
--------------------------

--------------------------
----- EVENTS -------------


-- events for the main list of games
data GameListEvent = GLEAddGame   
                   | GLEDeleteGame Bool -- Is this the last game?
                   | GLEUpdatePlayers 
$(deriveJSON id ''GameListEvent)

data GameListMsg = GameListMsg {glmEType   :: GameListEvent,
                                glmGUID    :: Text,
                                glmPlayers :: [Text]}
$(deriveJSON (drop 3) ''GameListMsg)


-- events for individual game lobbies
data PlayerListEvent = PLEJoin | PLELeave {pleNewLeader :: Maybe Text} | PLEStart
$(deriveJSON (drop 3) ''PlayerListEvent)

data PlayerListMsg = PlayerListMsg {plmEType :: PlayerListEvent,
                                    plmPlayer :: Text,
                                    plmPlayers :: [Text]}
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

data NameResult = NameSuccess | NameTaken | NameInvalid

--- XXX MUST REMOVE
getDumpTablesR :: Handler RepHtml
getDumpTablesR =
  do people <- runDB $ selectList ([] :: [Filter User]) []
     defaultLayout [whamlet|
       <p>table dump
       <ol>
         $forall Entity _ p <- people
           <li>
             <table>
               <tr><td>source</td><td>credID</td><td>name</td></tr>
               <tr>
                 <td>#{userCredSource p}
                 <td>#{userCredID p}
                 <td>#{userName p}
       |]
      

postSetNameR :: Handler ()
postSetNameR =
  do uid <- requireAuthId
     ((result, _), _) <- runFormPost nameForm
     case result of
       FormSuccess nmUnsanitized -> do
         let nm = strip nmUnsanitized
         nmResult <- if T.length nm < 1 then return NameInvalid else runDB $ do 
           taken <- liftM isJust $ getBy $ UniqueName nm
           unless taken $ update uid [UserName =. nm]
           return $ if taken then NameTaken else NameSuccess
         case nmResult of
           NameSuccess -> redirect HanabiLobbyR
           NameTaken -> do
            setMessage $ toHtml $ T.concat ["Sorry, ",nm," is taken."]
            redirect SetNameR
           NameInvalid -> do
            setMessage $ toHtml $ T.concat ["Sorry, ",nm," is not a valid name."]
            redirect SetNameR
       _ -> do setMessage "Please Enter a valid name."
               redirect SetNameR

postCreateHanabiR :: Handler ()
postCreateHanabiR = do
  nm     <- requireName
  guid   <- newChannel
  pchan  <- newChannel
  let newPlayer = Player nm [] pchan
  _      <- runDB $ insert $ 
              Game { gameStatus   = NotStarted
                   , gameUid      = guid
                   , gameLeader   = nm
                   , gamePlayers  = [newPlayer]
                   , gameHints    = maxHints
                   , gameStrikes  = 0
                   , gameBoard    = Board []
                   , gameDiscards = Discards []
                   , gameDeck     = [] }
  setSession sgameid guid
  lobbyChan <- liftM lobbyChannel getYesod
  sendMessage lobbyChan $ GameListMsg GLEAddGame guid [nm]
  redirect PlayHanabiR

data UnjoinResult =
    UnjoinSuccess {guid :: Text, players :: [Player], newLeader :: Maybe Text}
  | UnjoinFail
  | UnjoinEndGame {guid :: Text}

postUnjoinHanabiR :: Handler ()
postUnjoinHanabiR = do
  nm  <- requireName
  lobbyChan <- liftM lobbyChannel getYesod
  res <- requireGameTransaction (\gid game ->
      let players = gamePlayers game in
      case (gameStatus game, length players <= 1) of
        (NotStarted,True ) -> 
          do delete gid 
             return $ UnjoinEndGame (gameUid game)
        (NotStarted,False) ->
          let newPlayers = filter (\p -> nm /= playerName p) players
              replaceLeader = 
                case (newPlayers, nm == gameLeader game) of
                  (p : _, True) -> Just $ playerName p
                  _             -> Nothing
           in
          do update gid (  (GamePlayers =. newPlayers)
                         : maybe [] (\n -> [GameLeader =. n]) replaceLeader)
             return (UnjoinSuccess (gameUid game) newPlayers replaceLeader)
        (_,_) -> return UnjoinFail -- unjoin is only for games that haven't started
                                   -- XXX might want to return better error messages based on status
    )
  case res of
    UnjoinSuccess guid ps rleader -> 
      do deleteSession sgameid
         let names = map playerName ps
         sendMessage lobbyChan $ GameListMsg GLEUpdatePlayers guid names
         gchan <- getChannel guid
         sendMessage gchan $ PlayerListMsg (PLELeave rleader) nm names
         redirect HanabiLobbyR
    UnjoinFail         -> redirect PlayHanabiR -- XXX maybe I should alert the user or something
    UnjoinEndGame guid ->
      do deleteSession sgameid
         gamesLeft <- liftM null $ runDB $ selectList [GameStatus ==. NotStarted] []
         sendMessage lobbyChan $ GameListMsg (GLEDeleteGame gamesLeft) guid []
         redirect HanabiLobbyR

data JoinResult = JSuccess [Player]
                | JFailGameGone | JFailGameStarted | JFailGameFull
--- XXX CHECK IF PLAYER IS ALREADY IN GAME TO MAKE IDEMPOTENT             
   
postJoinHanabiR :: Text -> Handler ()
postJoinHanabiR guid = do
  nm <- requireName
  pchan <- newChannel
  res <- runDB $ do 
    mgame <- getBy $ UniqueUid guid
    case mgame of
      Nothing -> return JFailGameGone
      Just (Entity gid (Game {gameStatus=NotStarted,gamePlayers})) ->
        if (length gamePlayers >= 5) then return JFailGameFull
           else let newPlayers = gamePlayers ++ [Player nm [] pchan] in
                do update gid [GamePlayers =. newPlayers]
                   return $ JSuccess newPlayers
      Just _ -> return JFailGameStarted
  case res of
    JSuccess ps -> 
      do gchan <- getChannel guid
         lchan <- liftM lobbyChannel getYesod
         setSession sgameid guid
         let names = map playerName ps
         sendMessage gchan $ PlayerListMsg PLEJoin nm names
         sendMessage lchan $ GameListMsg GLEUpdatePlayers guid names
         redirect PlayHanabiR
    JFailGameGone -> -- XXX use somethign better than setmessage
      do setMessage 
           "Sorry, that game was cancelled by its creator.  Please pick another."
         deleteChan pchan
         redirect HanabiLobbyR
    JFailGameStarted ->
      do setMessage "Sorry, that game just started.  Please pick another."
         deleteChan pchan
         redirect HanabiLobbyR
    JFailGameFull ->
      do setMessage "Sorry, that game just filled up.  Please pick another."
         deleteChan pchan
         redirect HanabiLobbyR


data StartResult = StartSuccess Game | StartNeedPlayers | StartNotLeader

getStartHanabiR :: Handler ()
getStartHanabiR = do 
  nm <- requireName
  -- XXX send event message to all players to start game
  result <- requireGameTransaction (\gid game -> 
    case (gameLeader game == nm, length (gamePlayers game) >= 2) of
      (False, _)  -> return StartNotLeader
      (_, False)  -> return StartNeedPlayers
      (True,True) -> do 
        (dealtPlayers,deck) <- 
          liftIO $ do players <- shuffle $ gamePlayers game
                      deal players
             -- XXX I should really precompute some shuffles to avoid
             -- locking up the DB like this
        update gid [GameStatus  =. Running 0 Nothing,
                    GamePlayers =. dealtPlayers,
                    GameDeck    =. deck]
        return $ StartSuccess game)
  case result of
    StartNotLeader    -> setMessage "Sorry, you must be the leader to start the game."
    StartNeedPlayers  -> setMessage "Sorry, two players are needed for Hanabi."
    StartSuccess game -> 
      do gchan <- getChannel $ gameUid game
         sendMessage gchan $ PlayerListMsg PLEStart (gameLeader game) 
                                           (map playerName $ gamePlayers game)
  redirect PlayHanabiR

                 
playerListWidget :: Text -> Game -> Widget
playerListWidget nm game =
  let names = prettyNameList game
      guid = gameUid game
  in
  do playerList <- lift newIdent
     mbox <- lift newIdent
     startButton <- lift newIdent
     [whamlet|
       <p id=#{mbox}>
       <p id=#{playerList}>#{names}
       
       <table>
         <tr>
           $if nm == gameLeader game
             <td id=#{startButton}>
               <form method=get action=@{StartHanabiR}>
                   <input type=submit value="Start the game">
           $else
             <td id=#{startButton} class="hidden">
               <form method=get action=@{StartHanabiR}>
                   <input type=submit value="Start the game">
           <td>
             <form method=post action=@{UnjoinHanabiR}>
               <input type=submit value="Leave the game">
                       
     |] -- XXX grey out start box
     toWidgetBody [julius|
        var list = document.getElementById(#{toJSON playerList});
        var mbox = document.getElementById(#{toJSON mbox});
        var name = #{toJSON nm};
        var startButton = document.getElementById(#{toJSON startButton});

        var src = new EventSource("@{GameEventReceiveR guid}");

        src.onmessage = function(msg) {
          var event = JSON.parse(msg.data);
          var etype = event.EType;
          if ("PLEJoin" in etype) {
            mbox.innerHTML = event.Player + " joined the game.";
          } else if ("PLELeave" in etype) {
            mbox.innerHTML = event.Player + " left the game.";
            if (etype.PLELeave.NewLeader == name) {
               mbox.innerHTML += "  You are now the leader";
               $(startButton).fadeIn(400,function() {$(startButton).removeClass();});
            }
          } else if ("PLEStart" in etype) {
            location.reload(true);
          }

          list.innerHTML = displayPlayerList(event.Players);
        }; 
     |]


gameWidget :: Game -> Text -> Widget
gameWidget game nm = $(widgetFile "game")
  where
    players :: [(Int,Player)]
    players = zip [1..] $ gamePlayers game

    mynum :: Int
    mynum = 
      case find ((==nm) . playerName . snd) players of
         Nothing    -> 0 -- XXX return an error or something
         Just (n,_) -> n-1

    mychan :: Text
    mychan = playerChanId (gamePlayers game !! mynum)

    numCards :: Int
    numCards = if length players < 4 then 5 else 4

    playerDiv pnum (Player {playerName=name, playerHand=hand}) =
     let
       knowledge :: [Knowledge]
       knowledge = map snd hand

       colorKnowledge :: [Fact Color]
       colorKnowledge = map knownColor knowledge

       rankKnowledge :: [Fact Rank]
       rankKnowledge = map knownRank knowledge

       numText :: Text
       numText = pack $ show $ pnum - 1
     in
       [whamlet|
           <div id=#{append "player" numText} class="curvy">
             <p>
               <b> #{pnum}. #{name}
             <table id=#{append numText "cards"}>
               <tr id=#{append numText cardRowID}>
                 <td></td>
                 $if nm == name
                   $forall k <- knowledge
                     <td> <img src=@{StaticR $ knowledgeToRoute k}>
                 $else
                   $forall c <- map fst hand
                     <td> <img src=@{StaticR $ cardToRoute c}>
               <tr id=#{append numText colorRowID}>
                 <td rowspan="2">Knowledge:
                 $forall k <- colorKnowledge
                   <td>#{show k}
               <tr id=#{append numText rankRowID}>
                 <td class="hidden"></td>
                 $forall k <- rankKnowledge
                   <td>#{show k}
       |]

getPlayHanabiR :: Handler RepHtml
getPlayHanabiR = do
  nm <- requireName
  game <- requireGame
  case gameStatus game of
    NotStarted -> defaultLayout [whamlet|
        <p> This game hasn't started yet.  These players have joined:
        
        ^{playerListWidget nm game}
      |]
    _ -> defaultLayout $ gameWidget game nm
      -- XXX do I really want all other statuses to display gameWidget?

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


getHanabiLobbyR :: Handler RepHtml
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

getGameEventReceiveR :: Text -> Handler ()
getGameEventReceiveR guid = do
  chan0 <- getChannel guid
  chan <- liftIO $ dupChan chan0
  req <- waiRequest
  res <- lift $ eventSourceAppChan chan req
  sendWaiResponse res

getPlayerEventReceiveR :: Text -> Handler ()
getPlayerEventReceiveR uid = do
  chan0 <- getChannel uid
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


---------------------------------------
----- Game event handlers (AJAX) ------

postDiscardR :: Handler RepJson
postDiscardR = do
  nm <- requireName
  -- remember that any number sent back to javascript will need a +1
  card <- liftM pred $ runInputPost $ ireq intField cardField
  discardResult <- requireGameTransaction (\gid game ->
    case gameStatus game of
      NotStarted  -> return $ Left "The game isn't running."
      Done        -> return $ Left "The game has already ended."
      Running {currentP} ->
        let currentPName = playerName $ gamePlayers game !! currentP
            correctPlayer = nm == currentPName in
        if not correctPlayer 
          then return $ Left "You aren't the current player."
          else case discard game card of
            Left err -> return $ Left $ pack err -- XXX, do more in this case?
            Right (game',oldcard,newcard) -> 
              do replace gid game' 
                 return $ Right (currentP,game',oldcard,newcard))
  -- get the other players channels and send them an update message
  responseMessage <- case discardResult of
    Left err -> return [(errorField,toJSONT err)]
    Right (currentP,game,oldcard,newcard) -> do
      iochans <- liftM gameChannels getYesod
      chans <- liftIO $ readIORef iochans
      routeRenderer <- getUrlRender
      let currentPlayer :: Player
          currentPlayer = gamePlayers game !! currentP -- XXX error
                         
          otherPlayers :: [Text]
          otherPlayers = map playerChanId $ gamePlayers game \\ [currentPlayer]
          playerChans = map (flip lookup chans) otherPlayers -- XXX log if any nothings?
          newcardRoute = fmap (routeRenderer . StaticR . cardToRoute) newcard
          -- send msg to other players

          message :: Bool -> Text
          message me = T.concat $ 
               [if me then "You" else playerName currentPlayer,
                " discarded a ",describeCard oldcard]
             ++ case newcard of
                  Nothing -> ["."]
                  Just c  -> [" and drew a ",
                              if me then "new card" else describeCard c,
                              "."]
          
          event :: [(Text,Value)]
          event = [(discardField,object $
                         [(playerField,toJSON currentP)
                         ,(cardField,toJSON (card+1))]
                      ++ case newcardRoute of
                           Nothing -> []
                           Just r  -> [(newcardField,toJSONT r)])
                  ,(messagesField,toJSON [message False])
                  ]  -- XXX end game message

      mapM_ (maybe (return ()) (flip sendMessage $ object event)) playerChans


      return [(replacecardField, toJSON $ isJust newcard)
             ,(messagesField, toJSON $ message True)
                          -- XXX better message, end game
             ]

  -- send response back to original player
  jsonToRepJson $ object responseMessage

postPlayR :: Handler RepJson
postPlayR = do
  nm <- requireName
  -- remember that any number sent back to javascript will need a +1
  card <- liftM pred $ runInputPost $ ireq intField cardField
  playResult <- requireGameTransaction (\gid game ->
    case gameStatus game of
      NotStarted  -> return $ Left "The game isn't running."
      Done        -> return $ Left "The game has already ended."
      Running {currentP} ->
        let currentPName = playerName $ gamePlayers game !! currentP
            correctPlayer = nm == currentPName in
        if not correctPlayer 
          then return $ Left "You aren't the current player."
          else case play game card of
            Left err -> return $ Left $ pack err -- XXX, do more in this case?
            Right (game',success,oldcard,newcard) -> 
              do replace gid game' 
                 return $ Right (currentP,game',success,oldcard,newcard))
  -- get the other players channels and send them an update message
  responseMessage <- case playResult of
    Left err -> return [(errorField,toJSONT err)]
    Right (currentP,game,success,oldcard,newcard) -> do
      iochans <- liftM gameChannels getYesod
      chans <- liftIO $ readIORef iochans
      routeRenderer <- getUrlRender
      let currentPlayer :: Player
          currentPlayer = gamePlayers game !! currentP -- XXX error
                         
          otherPlayers :: [Text]
          otherPlayers = map playerChanId $ gamePlayers game \\ [currentPlayer]
          playerChans = map (flip lookup chans) otherPlayers -- XXX log if any nothings?
          newcardRoute = fmap (routeRenderer . StaticR . cardToRoute) newcard
          -- send msg to other players

          message :: Bool -> Text
          message me = T.concat $ (if me then "You" else playerName currentPlayer) :
                if success 
                   then [" played a ",describeCard oldcard]
                   else [" tried to play a ",describeCard oldcard,", causing a strike, "]
             ++ case newcard of
                  Nothing -> ["."]
                  Just c  -> [" and drew a ",
                              if me then "new card" else describeCard c,
                              "."]

          event :: [(Text,Value)]
          event = [(playField,object $
                         [(playerField,toJSON currentP)
                         ,(cardField,toJSON (card+1))]
                      ++ case newcardRoute of
                           Nothing -> []
                           Just r  -> [(newcardField,toJSONT r)])
                  ,(messagesField,toJSON [message False])
                  ]  -- XXX end game message

      mapM_ (maybe (return ()) (flip sendMessage $ object event)) playerChans


      return [(replacecardField, toJSON $ isJust newcard)
             ,(messagesField, toJSON $ message True)
              -- XXX better message, end game
             ]

  -- send response back to original player
  jsonToRepJson $ object responseMessage

