module Handler.Hanabi 
  (getHanabiLobbyR,getPlayHanabiR,getStartHanabiR
  ,postCreateHanabiR,postJoinHanabiR,postUnjoinHanabiR
  ,getGameEventReceiveR,getLobbyEventReceiveR,getPlayerEventReceiveR
  ,getSetNameR,postSetNameR
  ,getDumpTablesR  -- XXX
  ,postColorHintR,postRankHintR,postDiscardR,postPlayR)
   

where

import Import

import Data.Maybe (isJust,fromMaybe)
import Data.IORef
import Data.Text (pack,unpack,append,strip)
import qualified Data.Text as T (concat,length)

import Data.Aeson hiding (object)
import Data.Aeson.TH

import Control.Monad (liftM,when,unless)
import Control.Concurrent.Chan

import Blaze.ByteString.Builder (fromLazyByteString)
import Text.Blaze.Html.Renderer.String (renderHtml)

import System.Random (randomRIO)
import Data.List (foldl',find,(\\))
-- import qualified Data.Traversable as Trav (mapM)

import Yesod.Auth
import Network.Wai.EventSource

import Safe (readMay)

----  
---- TODO
----
---- - xxx can't change name while playing
---- - xxx dummy auth doesn't handle non-unique names right, and doens't restrict name length.
---- - xxx clean out dummy auths on startup? (timeout)
---- - xxx names with special characters are not handled correctly
----            are things being preescaped by form/db insertion and then it's messed up?
---- - xxx figure out when to delete channels
---- - xxx it would be "easy" for one player to listen to another player's events
---- - xxx you shouldn't be able to discard when you already have max hints
---- - xxx highlight current player
---- - xxx grey out non-selected player's actions
---- - xxx organize players better on screen
---- - xxx log errors
---- - xxx log actions
---- - xxx end game doesn't happen
---- - xxx hint chooser is horrible
---- - xxx highlight selected hints in red
---- - xxx some kind of nicer thing for when cards get updated as a result of hints
---- - xxx start game button should only show for leader


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

cardField, messagesField, errorField, newcardField, playerField :: Text
discardField, playField, colorField, rankField :: Text
replacecontentField, replaceidField, replacedataField  :: Text
replaceCardsField, cardsField, highlightplayerField :: Text
cardField        = "card"           -- Int
playerField      = "player"    -- Int

discardField     = "discard"        -- object (cardField,playerField,newcardfield (opt))
playField        = "play"           -- object (cardField,playerField,newcardfield (opt))

colorField       = "color"
rankField        = "rank"

messagesField    = "msgs"            -- [String]
errorField       = "error"          -- String

newcardField     = "newcard"        -- Route

replacecontentField = "replacecontent" -- object (replaceid,replacedata)
                                       -- an id, and the content
replaceidField = "replaceid"
replacedataField = "replacedata"

replaceCardsField = "replacecards"     -- object (playerField,cardsField)
cardsField        = "cards"            -- [html]
highlightplayerField = "highlightplayer" -- int


-- css ids
cardRowID, colorRowID, rankRowID :: Text
cardRowID  = "CardRow"
colorRowID = "ColorRow"
rankRowID  = "RankRow"

colorHintID,rankHintID :: Int -> Text
colorHintID i = append (pack $ show i) colorRowID
rankHintID  i = append (pack $ show i) rankRowID

---------------------------------------
------- How to display knowledge ------

htmlColor :: Color -> HtmlUrl (Route App)
htmlColor c = [hamlet|#{show c}|]

htmlRank :: Rank -> HtmlUrl (Route App)
htmlRank r = [hamlet|#{describe r}|]

dispKnowledge :: Hintable a => (a -> HtmlUrl (Route App)) 
                            -> Fact a -> HtmlUrl (Route App)
dispKnowledge d (Is a)    = d a
dispKnowledge _ Mystery   = [hamlet| |]
dispKnowledge d (Isnt nots) =
  if length nots > 2 then list (allHints \\ nots)
                     else [hamlet|not ^{list nots}|]
  where
    list []     = [hamlet| |]
    list [a]    = d a
    list (a:as) = [hamlet| ^{d a} or ^{list as} \|]

dispColorKnowledge :: Fact Color -> HtmlUrl (Route App)
dispColorKnowledge = dispKnowledge htmlColor

dispRankKnowledge :: Fact Rank -> HtmlUrl (Route App)
dispRankKnowledge = dispKnowledge htmlRank

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

----------------------------------------------------------
---- Small bits of HTML for various parts of the page ----

hintsTdContent :: Int -> HtmlUrl a
hintsTdContent h = [hamlet|<b>Hints:</b> #{h}|]

deckTdContent :: Int -> HtmlUrl a
deckTdContent d = [hamlet|<b>Deck:</b> #{d}|]

strikesTdContent :: Int -> HtmlUrl a
strikesTdContent i = [hamlet|<b>Strikes:</b> #{i}|]

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


discardTableId :: Color -> Text
discardTableId c = append (pack $ show c) "discardtable"

boardCellId :: Color -> Text
boardCellId c = append (pack $ show c) "board"

discardTable :: Color -> [(Rank,Int)] -> HtmlUrl (Route App)
discardTable color discards =
  [hamlet|
    <table class="discardtable">
         <tr class="discardrow">
           <td colspan="2" class="discardimage">
             <img src=@{StaticR (smallColorImage color)}>
       $forall r <- [One,Two,Three,Four,Five]
         <tr class="discardrow">
           $maybe i <- lookup r discards
             <td>#{show r ++ "s"}
             <td>#{i}
           $nothing
             <td>
             <td>
  |]

knowledgeRow :: Hintable a => (Int -> Text) -> (Fact a -> HtmlUrl (Route App))
             -> [Fact a] -> HtmlUrl (Route App)
knowledgeRow ids disp ks = [hamlet|
    $forall (n,f) <- numberedKs
      <td class="knowledgecell" id=#{ids n}>
        ^{disp f}
  |]
  where
    numberedKs = zip [1..] ks

-- Constructs the TD showing a player's own card
playerSecretCardTd :: Knowledge -> HtmlUrl (Route App)
playerSecretCardTd k = [hamlet|
    <td>
      <img src=@{StaticR $ knowledgeToRoute k}>
  |]

gameWidget :: Game -> Text -> Widget
gameWidget game nm = $(widgetFile "game")
  where
    players,otherPlayers :: [(Int,Player)]
    players      = zip [0..] $ gamePlayers game
    otherPlayers = filter (\(_,p) -> nm /= playerName p) players
            
    allRanks :: [Rank]
    allRanks = allHints
    allColors :: [Color]
    allColors = allHints -- XXX remove once no longer ambiguous

    mynum :: Int
    mynum = 
      case find ((==nm) . playerName . snd) players of
         Nothing    -> 0 -- XXX return an error or something
         Just (n,_) -> n

    mychan :: Text
    mychan = playerChanId (gamePlayers game !! mynum)

    discards :: [(Color,[(Rank,Int)])]
    discards = map (\c -> (c,getDiscards game c)) allHints

    board :: [(Color,Rank)]
    board = (\(Board bd) -> bd) $ gameBoard game

    numCards :: Int
    numCards = if length players < 4 then 5 else 4

    playerDiv pnum (Player {playerName=name, playerHand=hand}) =
     let
       knowledge :: [Knowledge]
       knowledge = map snd hand

       numText :: Text
       numText = pack $ show $ pnum

       colorKIds, rankKIds :: Int -> Text
       colorKIds n = append (colorHintID pnum) $ pack $ show n
       rankKIds n  = append (rankHintID pnum) $ pack $ show n

       borderColor :: Text
       borderColor = case gameStatus game of
                       Running {currentP} -> if currentP == pnum then "red" else "black"
                       _ -> "black"
     in
       [whamlet|
           <div id=#{append "player" numText} class="curvy" style="border-color: #{borderColor};">
             <p>
               <b> #{succ pnum}. #{name}
             <table id=#{append numText "cards"}>
               <tr id=#{append numText cardRowID}>
                 $if nm == name
                   $forall k <- knowledge
                      ^{playerSecretCardTd k}
                 $else
                   $forall c <- map fst hand
                     <td> <img src=@{StaticR $ cardToRoute c}>
               <tr id=#{colorHintID pnum} class="knowledgerow">
                 ^{knowledgeRow colorKIds dispColorKnowledge $ map knownColor knowledge}
               <tr id=#{rankHintID pnum} class="knowledgerow">
                 ^{knowledgeRow rankKIds dispRankKnowledge $ map knownRank knowledge}
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
      -- XXX check that this player is actually in the game - people could pass around cookies

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


-----------------------------------------
----- Event urls ------------------------

{-
newtype RepEventSource = RepEventSource Content

instance HasReps RepEventSource where
  chooseRep (RepEventSource c) = const $ return ("text/event-stream", c)
-}
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

-- XXX check for game end
hintHandler :: Int -> Either Color Rank -> Handler RepJson
hintHandler hintedPN e = do
  nm         <- requireName
  renderParams  <- getUrlRenderParams
  let render h = pack $ renderHtml $ h renderParams
  (g,result) <- requireGameTransaction (\gid game ->
    case gameStatus game of
      NotStarted  -> return $ (game,Left "The game isn't running.")
      Done        -> return $ (game,Left "The game has already ended.")
      Running {currentP} ->
        let currentPName = playerName $ gamePlayers game !! currentP
            correctPlayer = nm == currentPName in
        if not correctPlayer 
          then return $ (game,Left "You aren't the current player.")
          else case hint game hintedPN e of
            Left err               -> return (game,Left err)
            Right (game',hplayer,highlightedCards) -> 
              do replace gid game'
                 return (game',Right (hplayer,highlightedCards)))
  case result of
    Left err            -> jsonToRepJson $ object [(errorField,err)]
    Right (hintedP,hintedCards) -> do 
       iochans <- liftM gameChannels getYesod
       chans <- liftIO $ readIORef iochans
       let
         otherPlayerChans = map (flip lookup chans) otherPlayers -- XXX log if any nothings?
         hintedPlayerChan = lookup (playerChanId hintedP) chans
       mapM_ (maybe (return ()) (flip sendMessage $ object (actions messageOther)))
             otherPlayerChans
       maybe (return ()) 
             (flip sendMessage 
                   (object (hintedPlayerCardUpdates : actions messageHinted)))
             hintedPlayerChan
       jsonToRepJson $ object $ actions messageHinter
      where
        hintIdField :: Int -> Text
        hintIdField card = 
          append (case e of
            Left _ -> colorHintID hintedPN
            Right _ -> rankHintID hintedPN) $ pack $ show card
        
        htmlFact :: Knowledge -> HtmlUrl (Route App)
        htmlFact = case e of
          Left _ -> dispColorKnowledge . knownColor
          Right _ -> dispRankKnowledge . knownRank

        dispFact :: Knowledge -> Text
        dispFact k = pack $ renderHtml $ htmlFact k renderParams
        
        knowlUpdates :: [[(Text,Value)]]
        knowlUpdates =
          map (\(cnum,(_,k)) -> [(replaceidField, toJSONT $ hintIdField cnum),
                                 (replacedataField, toJSONT $ dispFact k)])
              (zip [1..] $ playerHand hintedP)

        contentUpdates :: (Text,Value)
        contentUpdates = (replacecontentField, toJSON $ map object $
             [(replaceidField, toJSONT "hintstd")
             ,(replacedataField, toJSON $ render $ hintsTdContent $ gameHints g)]
           : knowlUpdates)

        hintedPlayerCardUpdates :: (Text,Value)
        hintedPlayerCardUpdates = (replaceCardsField, object $
           [(playerField, toJSON hintedPN),
            (cardsField, toJSON $ 
               map (\(_,k) -> pack $ renderHtml $
                       playerSecretCardTd k renderParams)
                 $ playerHand hintedP)
            ])

        otherPlayers :: [Text]
        otherPlayers = map playerChanId $
          filter (\p -> (playerName p /= nm) && (playerName p /= playerName hintedP))
                 (gamePlayers g)

        hintDesc :: Text
        hintDesc = case e of
                     Left c  -> append (describe c) " cards"
                     Right r -> append (describe r) "s"

        messageHinted,messageHinter,messageOther :: Text
        messageHinted = T.concat [nm, " gave you a hint about your ",
                                  hintDesc,"."]
        messageHinter = T.concat ["You gave ", playerName hintedP,
                                  " a hint about their ", hintDesc,"."]
        messageOther = T.concat [nm, " gave ", playerName hintedP,
                                 " a hint about their ", hintDesc,"."]

        actions :: Text -> [(Text,Value)]
        actions message = [contentUpdates,(messagesField,toJSON message)]
        
postColorHintR :: Handler RepJson
postColorHintR = do
  (p,ctext) <- runInputPost iform
  case readMay $ unpack ctext of
    Just c  -> hintHandler p $ Left c
    Nothing -> 
      jsonToRepJson $ object $ [(errorField,
        toJSONT "Invalid hint input.  Please try refreshing your page")] -- XXX
  where
    iform :: FormInput App App (Int,Text)
    iform = (,) <$> ireq intField playerField
                <*> ireq textField colorField

postRankHintR :: Handler RepJson
postRankHintR = do
  (p,ctext) <- runInputPost iform
  $(logDebug) $ ctext
  case readMay $ unpack ctext of
    Just r  -> hintHandler p $ Right r
    Nothing -> 
      jsonToRepJson $ object $ [(errorField,
        toJSONT "Invalid hint input.  Please try refreshing your page")] -- XXX
  where
    iform :: FormInput App App (Int,Text)
    iform = (,) <$> ireq intField playerField
                <*> ireq textField rankField

-- XXX these can be better cleaned up and combined
postActionHandler :: Show a => FormInput App App a  -- XXX show
                  -> (a -> Game -> Either String (Game,b))
                  -> (Game -> Int -> b -> Handler (Maybe Text -> [(Text,Value)]))
                             -- int is current player
                  -> Handler RepJson
postActionHandler inputform attemptaction handleresult = do
  nm <- requireName
  -- remember that any number sent back to javascript will need a +1
  content <- runInputPost inputform
  (g,result) <- requireGameTransaction (\gid game ->
    case gameStatus game of
      NotStarted  -> return $ (game,Left "The game isn't running.")
      Done        -> return $ (game,Left "The game has already ended.")
      Running {currentP} ->
        let currentPName = playerName $ gamePlayers game !! currentP
            correctPlayer = nm == currentPName in
        if not correctPlayer 
          then return $ (game,Left "You aren't the current player.")
          else case attemptaction content game of
            Left err    -> return $ (game,Left $ pack err)
            Right (g,b) -> do replace gid g
                              return $ (g,Right (currentP,b)))
  messages <- 
    case result of 
      Left err -> return $ const [(errorField,toJSONT err)]
      Right (cp,b)  -> handleresult g cp b
  iochans <- liftM gameChannels getYesod
  chans <- liftIO $ readIORef iochans
  let otherPlayers :: [Text]
      otherPlayers = map playerChanId $
         filter ((/= nm) . playerName) $ gamePlayers g
      playerChans = map (flip lookup chans) otherPlayers -- XXX log if any nothings?
  mapM_ (maybe (return ()) (flip sendMessage $ object (messages $ Just nm))) playerChans
  jsonToRepJson $ object $ messages Nothing
        

postDiscardR :: Handler RepJson
postDiscardR = 
  let
    inputForm :: FormInput App App Int
    inputForm = ireq intField cardField

    attemptAction :: Int -> Game -> Either String (Game,(Int,Card,Maybe Card))
    attemptAction card g =
      case discard g (pred card) of
        Left err -> Left err
        Right (g',oldcard,newcard) -> Right (g',(card,oldcard,newcard))

    handleResult :: Game -> Int -> (Int,Card,Maybe Card) 
                 -> Handler (Maybe Text -> [(Text,Value)])
    handleResult g currentP (oldcardi,oldcard,newcard) = do
      routeRenderer <- getUrlRender
      renderParams  <- getUrlRenderParams
      let
        render h = pack $ renderHtml $ h renderParams

        message :: Maybe Text -> Text
        message me = T.concat $
             [fromMaybe "You" me," discarded a ",describeCard oldcard]
          ++ case newcard of
               Nothing -> ["."]
               Just c  -> [" and drew a ",
                           if isJust me then describeCard c else "new card",
                           "."]

        newcardRoute :: Maybe Text -> Maybe Text
        newcardRoute me = 
          fmap (if isJust me 
                  then routeRenderer . StaticR . cardToRoute
                  else const $ routeRenderer . StaticR $
                         knowledgeToRoute (Knowledge Mystery Mystery))
          newcard

        color :: Color
        color = cardColor oldcard
    
        newDiscardTable :: Text
        newDiscardTable = render $ discardTable color (getDiscards g color)

        replaceContent = (replacecontentField, toJSON $ map object $
          [ [(replaceidField,toJSON $ discardTableId (cardColor oldcard))
            ,(replacedataField, toJSON newDiscardTable)]
           ,[(replaceidField,toJSONT "hintstd")
            ,(replacedataField, toJSON $ render $ hintsTdContent $ gameHints g)]
           ,[(replaceidField,toJSONT "decksizetd")
            ,(replacedataField, toJSONT $ render $ deckTdContent $ length $ gameDeck g)]
          ])
      return (\me ->
        [(discardField,object $
              [(playerField,toJSON currentP)
              ,(cardField,toJSON (oldcardi))]
           ++ maybe [] (\r -> [(newcardField, toJSONT r)]) (newcardRoute me))
        ,(messagesField,toJSON [message me])
        ,replaceContent])
  in
    postActionHandler inputForm attemptAction handleResult

postPlayR :: Handler RepJson
postPlayR = 
  let
    inputForm :: FormInput App App Int
    inputForm = ireq intField cardField

    attemptAction :: Int -> Game
                  -> Either String (Game,(Int,Bool,Card,Maybe Card))
    attemptAction card g =
      case play g (pred card) of
        Left err -> Left err
        Right (g',success,oldcard,newcard) -> Right (g',(card,success,oldcard,newcard))

    handleResult :: Game -> Int -> (Int,Bool,Card,Maybe Card) 
                 -> Handler (Maybe Text -> [(Text,Value)])
    handleResult g currentP (oldcardi,success,oldcard,newcard) = do
      routeRenderer <- getUrlRender
      renderParams  <- getUrlRenderParams
      let
        render :: HtmlUrl (Route App) -> Text
        render h = pack $ renderHtml $ h renderParams

        message :: Maybe Text -> Text
        message me = T.concat $ (fromMaybe "You" me) :
              if success 
                 then [" played a ",describeCard oldcard]
                 else [" tried to play a ",describeCard oldcard,", causing a strike, "]
           ++ case newcard of
                Nothing -> ["."]
                Just c  -> [" and drew a ",
                            if isJust me then "new card" else describeCard c,
                            "."]

        newcardRoute :: Maybe Text -> Maybe Text
        newcardRoute me = 
          fmap (if isJust me 
                  then routeRenderer . StaticR . cardToRoute
                  else const $ routeRenderer . StaticR $
                         knowledgeToRoute (Knowledge Mystery Mystery))
          newcard

        color :: Color
        color = cardColor oldcard
                
        newBoardCell :: Text
        newBoardCell = render [hamlet|
            <img src=@{StaticR (cardToRoute oldcard)}>
          |]

        newDiscardTable :: Text
        newDiscardTable = render $ discardTable color (getDiscards g color)

--        highlightPlayer :: [(Text,Value)]
--        highlightPlayer = case gameStatus g of
--                            Running {newCP} -> [(highlightplayer,to

        replaceContent = (replacecontentField, toJSON $ map object $
            (if success
              then [(replaceidField,toJSON $ boardCellId color)
                    ,(replacedataField, toJSON newBoardCell)]
              else [(replaceidField,toJSON $ discardTableId color)
                    ,(replacedataField, toJSON newDiscardTable)])
                   
          : [[(replaceidField,toJSONT "hintstd")
             ,(replacedataField,
               toJSONT $ render $ hintsTdContent $ gameHints g)]
            ,[(replaceidField,toJSONT "decksizetd")
             ,(replacedataField,
               toJSONT $ render $ hintsTdContent $ length $ gameDeck g)]
            ,[(replaceidField,toJSONT "strikestd")
             ,(replacedataField,
               toJSONT $ render $ strikesTdContent $ gameStrikes g)]
              ])

      return (\me ->
        [(playField,object $
              [(playerField,toJSON currentP)
              ,(cardField,toJSON (oldcardi))]
           ++ maybe [] (\r -> [(newcardField, toJSONT r)]) (newcardRoute me))
        ,(messagesField,toJSON [message me])
        ,replaceContent])
  in
    postActionHandler inputForm attemptAction handleResult
