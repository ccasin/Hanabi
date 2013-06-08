module Handler.Hanabi 
  (getPlayHanabiR,getStartHanabiR
  ,postCreateHanabiR,postJoinHanabiR,postUnjoinHanabiR
  ,getGameEventReceiveR,getPlayerEventReceiveR
  ,getDumpTablesR  -- XXX
  ,postColorHintR,postRankHintR,postDiscardR,postPlayR,postChatR)
   

where

import Import

import Data.Maybe (isNothing,fromMaybe)
import Data.Text (pack,unpack,append)
import qualified Data.Text as T (concat)

import Data.Aeson.TH

import Control.Monad (liftM)
import Control.Concurrent.Chan

import Text.Blaze.Html.Renderer.String (renderHtml)

import Data.List (find,(\\))
-- import qualified Data.Traversable as Trav (mapM)

import Yesod.Auth
import Network.Wai.EventSource

import Safe (readMay)

import Handler.Infrastructure

----  
---- TODO
----
---- - BEFORE FIRST RELEASE
----
---- - xxx clean out dummy auths on startup? (timeout)
---- - xxx names with special characters are not handled correctly
----            are things being preescaped by form/db insertion and then it's messed up?
---- - xxx figure out when to delete channels
---- - xxx log errors
---- - xxx log actions
---- - xxx end game doesn't happen
---- - xxx hint chooser is horrible
---- - xxx highlight selected hints in red
---- - xxx some kind of nicer thing for when cards get updated as a result of hints
---- - xxx start game button should only show for leader
---- - xxx actions should go away after turn
---- - xxx what to do if channel lookup fails
---- - XXX errors go to all players right now
---- - XXX more sophisticated white space in chat
---- - xxx you shouldn't be able to discard when you already have max hints
----
---- - LATER:
---- - xxx can't change name while playing
---- - xxx it would be "easy" for one player to listen to another player's events

toJSONT :: Text -> Value
toJSONT = toJSON


---------------------------------------
------- How to display knowledge ------

htmlColor :: Color -> HtmlUrl (Route App)
htmlColor c = [hamlet|<img src=@{StaticR (smallColorImage c)}>|]

htmlRank :: Rank -> HtmlUrl (Route App)
htmlRank r = [hamlet|#{describe r}|]

dispKnowledge :: Hintable a => (a -> HtmlUrl (Route App)) 
                            -> Fact a -> HtmlUrl (Route App)
dispKnowledge d (Is a)    = d a
dispKnowledge _ Mystery   = [hamlet| |]
dispKnowledge d (Isnt nots) =
  if length nots > 2 then listYes (allHints \\ nots)
                     else [hamlet|not ^{listNot nots}|]
  where
    listNot []     = [hamlet| |]
    listNot [a]    = d a
    listNot (a:as) = [hamlet|^{d a} ^{listNot as}|]

    listYes []     = [hamlet| |]
    listYes [a]    = d a
    listYes (a:as) = [hamlet| ^{d a} or ^{listYes as}|]

dispColorKnowledge :: Fact Color -> HtmlUrl (Route App)
dispColorKnowledge = dispKnowledge htmlColor

dispRankKnowledge :: Fact Rank -> HtmlUrl (Route App)
dispRankKnowledge = dispKnowledge htmlRank

describeCard :: Card -> HtmlUrl (Route App)
describeCard (Card {cardColor,cardRank})
  = [hamlet|#{describe cardRank}^{htmlColor cardColor}|]

----------------------------------------------------------
---- Small bits of HTML for various parts of the page ----

hintsTdContent :: Int -> HtmlUrl a
hintsTdContent h = [hamlet|<b>Hints:</b> #{h}|]

deckTdContent :: Int -> HtmlUrl a
deckTdContent d = [hamlet|<b>Deck:</b> #{d}|]

strikesTdContent :: Int -> HtmlUrl a
strikesTdContent i = [hamlet|<b>Strikes:</b> #{i}|]

--------------------------
--------------------------

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
      

postCreateHanabiR :: Handler ()
postCreateHanabiR = do
  nm     <- requireName
  guid   <- newChannel
  pchan  <- newChannel
  let newPlayer = Player nm 0 [] pchan
  _      <- runDB $ insert $ 
              Game { gameStatus   = NotStarted
                   , gameUid      = guid
                   , gameLeader   = nm
                   , gamePlayers  = [newPlayer]
                   , gameHints    = maxHints
                   , gameStrikes  = 0
                   , gameBoard    = Board []
                   , gameDiscards = Discards []
                   , gameDeck     = []
                   , gameActions  = []}
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
           else let newPlayers = gamePlayers 
                              ++ [Player nm (length gamePlayers) [] pchan] in
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
         deleteChannel pchan
         redirect HanabiLobbyR
    JFailGameStarted ->
      do setMessage "Sorry, that game just started.  Please pick another."
         deleteChannel pchan
         redirect HanabiLobbyR
    JFailGameFull ->
      do setMessage "Sorry, that game just filled up.  Please pick another."
         deleteChannel pchan
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
          liftIO $ do 
            players <- shuffle $ gamePlayers game
            let players' = zipWith (\i p -> p {playerNum=i}) [0..] players
            deal players'
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
             ^{htmlColor color}
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
secretCardImg :: Knowledge -> HtmlUrl (Route App)
secretCardImg k = [hamlet|<img src=@{StaticR $ knowledgeToRoute k}> |]

gameWidget :: Game -> Text -> Widget
gameWidget game nm = do $(widgetFile "game")
                        case length $ gamePlayers game of
                          2 -> addStylesheet $ StaticR css_2players_css
                          3 -> addStylesheet $ StaticR css_3players_css
                          4 -> addStylesheet $ StaticR css_4players_css
                          _ -> addStylesheet $ StaticR css_5players_css
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
           <div id=#{append "player" numText} class="curvy playerdiv" style="border-color: #{borderColor};">
             <p>
               <b> #{succ pnum}. #{name}
             <table id=#{append numText "cards"} class="cardtable">
               <tr id=#{append numText cardRowID}>
                 $if nm == name
                   $forall k <- knowledge
                     <td class="cardrow"> ^{secretCardImg k}
                 $else
                   $forall c <- map fst hand
                     <td class="cardrow"> <img src=@{StaticR $ cardToRoute c}>
               <tr id=#{colorHintID pnum} class="knowledgerow">
                 ^{knowledgeRow colorKIds dispColorKnowledge $ map knownColor knowledge}
               <tr id=#{rankHintID pnum} class="knowledgerow">
                 ^{knowledgeRow rankKIds dispRankKnowledge $ map knownRank knowledge}
       |]


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


---------------------------------------
----- Game event handlers (AJAX) ------

-- The "GameEvent" type describes any kind of state change that we
-- want to tell the clients about.  We send these at two times:
-- 1) in response to AJAX game action requests
-- 2) asynchonously via an EventSource when one player does something
--    and the other players' views need to be updated.
data ContentUpdate = ContentUpdate {cuReplaceId   :: Text,
                                    cuReplaceData :: Text}
$(deriveJSON (drop 2) ''ContentUpdate)

data GameEvent = GEHighlightPlayer Int
               | GEUnhighlightPlayer
               | GEDiscard {geDiscPlayer  :: Int,
                            geDiscCard    :: Int,
                            geDiscNewCard :: Maybe Text}
               | GEPlay {gePlayPlayer  :: Int,
                         gePlayCard    :: Int,
                         gePlayNewCard :: Maybe Text}
               | GEMessages [Text]
               | GEError Text
               | GEReplaceContent [ContentUpdate]
               | GEReplaceCards {geReplPlayer :: Int,
                                 geReplCards  :: [Text]}
$(deriveJSON (drop 6) ''GameEvent)


-- This takes in the log of an action and the player to whom I'm going
-- to describe the action.  It returns a description of the action.
-- XXX do "describe last action" instead, to handle head
descLastAction :: Maybe Int -> Game 
               -> (HtmlUrl (Route App) -> Text) -> Text
descLastAction msgp game render =
  case gameActions game of
    [] -> "Error: no actions taken"
    (ALPlay {alPlayPlayer=p, alPlayCard=card,
             alPlayNewCard=newcard, alPlaySuccess=success}:_) -> T.concat $
         (fromMaybe "You" $ name p msgp)
      :  (if success 
            then [" played a ",render $ describeCard card]
            else [" tried to play a ",render (describeCard card),
                  ", causing a strike"])
      ++ [describeDraw (isNothing $ name p msgp) newcard]
    (ALDisc {alDiscPlayer=p, alDiscCard=card,
            alDiscNewCard=newcard}:_) -> T.concat
         [fromMaybe "You" (name p msgp),
          " discarded a ", render $ describeCard card,
          describeDraw (isNothing $ name p msgp) newcard]
    (ALHint {alHintHinter=hinter,alHintHinted=hinted,
             alHintType=htype}:_) ->
        T.concat [nameHinter," gave ",nameHinted," a hint about ",
                  proHinted,hintDesc]
      where
        nameHinter, nameHinted, proHinted :: Text
        nameHinter = fromMaybe "You" $ name hinter msgp
        nameHinted = fromMaybe "you" $ name hinted msgp
        proHinted = case name hinted msgp of
                      Nothing -> "your"
                      Just _  -> "their"

        hintDesc :: Text
        hintDesc = render $ 
          case htype of
            Left c -> [hamlet|^{htmlColor c}s.|]
            Right r -> [hamlet|^{htmlRank r}s.|]
  where
    -- Bool is "Am I the player whose turn it is"
    describeDraw :: Bool -> Maybe Card -> Text
    describeDraw _  Nothing  = "."
    describeDraw me (Just c) =
        T.concat [", and drew a ",
          if me then "new card" else render (describeCard c),
          "."]

    -- The player you want the name of, and maybe your own number
    name :: Int -> Maybe Int -> Maybe Text
    name p me = if Just p == me then Nothing
                  else Just $ playerName $ (gamePlayers game !! p)
                    


postChatR :: Handler RepJson
postChatR = do
  msg <- runInputPost $ ireq textField "content"
  nm  <- requireName
  g   <- requireGame
  let otherPlayers :: [Text]
      otherPlayers = map playerChanId $
         filter ((/= nm) . playerName) $ gamePlayers g

      message=T.concat ["<b>&lt;",nm,"&gt;</b> ",msg]
  playerChans <- getChannels otherPlayers
  mapM_ (flip sendMessage $ [GEMessages [message]]) playerChans
  jsonToRepJson $ [GEMessages [message]]


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
                 return (game',Right (hplayer,highlightedCards,currentP)))
  case result of
    Left err            -> jsonToRepJson [GEError $ pack err]
    Right (hintedP,hintedCards,cpnum) -> do 
       otherPlayerChans <- getChannelsP $ otherPlayers
       mapM_ (\(pix,pc) -> sendMessage pc $ actions pix) otherPlayerChans
       jsonToRepJson $ actions cpnum
      where
        hintIdField :: Int -> Text
        hintIdField card = 
          append (case e of
            Left _ -> colorHintID hintedPN
            Right _ -> rankHintID hintedPN) $ pack $ show card
        
        htmlFact :: Knowledge -> Text
        htmlFact = case e of
          Left _ -> render . dispColorKnowledge . knownColor
          Right _ -> render . dispRankKnowledge . knownRank

        hintedPlayerCardUpdates :: GameEvent
        hintedPlayerCardUpdates = 
          GEReplaceCards {geReplPlayer=hintedPN,
                          geReplCards=
            map (render . secretCardImg . snd) $ playerHand hintedP}

        contentUpdates :: GameEvent
        contentUpdates = GEReplaceContent $
             (ContentUpdate "hintstd" (render $ hintsTdContent $ gameHints g))
           : map (\(cnum,(_,k)) -> ContentUpdate (hintIdField cnum) (htmlFact k))
                 (zip [1..] $ playerHand hintedP)

        highlightPlayer :: GameEvent
        highlightPlayer = case gameStatus g of
                            Running {currentP=newCP} -> GEHighlightPlayer newCP
                            _ -> GEUnhighlightPlayer

        otherPlayers :: [(Int,Text)]
        otherPlayers = map (\p -> (playerNum p,playerChanId p)) $
          filter (\p -> (playerName p /= nm) && (playerName p /= playerName hintedP))
                 (gamePlayers g)

        actions :: Int -> [GameEvent]
        actions pnum =
            (if pnum == hintedPN then (hintedPlayerCardUpdates:) else id)
            [highlightPlayer, contentUpdates, GEMessages [message]]
          where
            message = descLastAction (Just pnum) g render


hintForm :: Text -> FormInput App App (Int,Text)
hintForm hintField = (,) <$> ireq intField playerField
                         <*> ireq textField hintField

postColorHintR :: Handler RepJson
postColorHintR = do
  (p,ctext) <- runInputPost $ hintForm colorField
  case readMay $ unpack ctext of
    Just c  -> hintHandler p $ Left c
    Nothing -> jsonToRepJson $ GEError
        "Invalid hint input.  Please try refreshing your page" -- XXX

postRankHintR :: Handler RepJson
postRankHintR = do
  (p,ctext) <- runInputPost $ hintForm rankField
  case readMay $ unpack ctext of
    Just r  -> hintHandler p $ Right r
    Nothing -> jsonToRepJson $ GEError
       "Invalid hint input.  Please try refreshing your page" -- XXX

-- XXX these can be better cleaned up and combined
postActionHandler :: Show a => FormInput App App a  -- XXX show
                  -> (a -> Game -> Either String (Game,b))
                  -> (Game -> Int -> b -> Handler (Bool -> [GameEvent]))
                             -- int is current player
                  -> Handler RepJson
postActionHandler inputform attemptaction handleresult = do
  nm <- requireName
  -- remember that any number sent back to javascript will need a +1
  content <- runInputPost inputform
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
          else case attemptaction content game of
            Left err    -> return $ (game,Left $ pack err)
            Right (g,b) -> do replace gid g
                              return $ (g,Right (currentP,b)))
  let otherPlayers :: [(Int,Text)]
      otherPlayers = map (\p -> (playerNum p,playerChanId p)) $
         filter ((nm /=) . playerName) (gamePlayers g)

      actionDesc :: Int -> GameEvent
      actionDesc pnum = GEMessages [descLastAction (Just pnum) g render]
  playerChans <- getChannelsP otherPlayers
  case result of 
    Left err -> do
      mapM_ (\(_,pc) -> sendMessage pc [GEError err]) playerChans
      jsonToRepJson [GEError err]
    Right (cp,b) -> do 
      hres <- handleresult g cp b
      mapM_ (\(pix,pc) -> sendMessage pc $ actionDesc pix : hres False)
            playerChans
      jsonToRepJson $ actionDesc cp : hres True

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
                 -> Handler (Bool -> [GameEvent])
    handleResult g currentP (oldcardi,oldcard,newcard) = do
      routeRenderer <- getUrlRender
      renderParams  <- getUrlRenderParams
      let
        render h = pack $ renderHtml $ h renderParams

        newcardRoute :: Bool -> Maybe Text
        newcardRoute me = 
          fmap (if me
                  then const $ routeRenderer . StaticR $
                         knowledgeToRoute (Knowledge Mystery Mystery)
                  else routeRenderer . StaticR . cardToRoute)
          newcard

        color :: Color
        color = cardColor oldcard

        highlightPlayer :: GameEvent
        highlightPlayer = case gameStatus g of
                            Running {currentP=newCP} -> GEHighlightPlayer newCP
                            _ -> GEUnhighlightPlayer

        newDiscardTable :: Text
        newDiscardTable = render $ discardTable color (getDiscards g color)

        replaceContent :: GameEvent
        replaceContent = GEReplaceContent [
            ContentUpdate (discardTableId (cardColor oldcard)) newDiscardTable
           ,ContentUpdate "hintstd" 
                          (render $ hintsTdContent $ gameHints g)
           ,ContentUpdate "decksizetd" 
                          (render $ deckTdContent $ length $ gameDeck g)
          ]

      return (\me ->
        [highlightPlayer
        ,GEDiscard {geDiscPlayer = currentP, geDiscCard = oldcardi,
                    geDiscNewCard = newcardRoute me}
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
                 -> Handler (Bool -> [GameEvent])
    handleResult g currentP (oldcardi,success,oldcard,newcard) = do
      routeRenderer <- getUrlRender
      renderParams  <- getUrlRenderParams
      let
        render :: HtmlUrl (Route App) -> Text
        render h = pack $ renderHtml $ h renderParams

        newcardRoute :: Bool -> Maybe Text
        newcardRoute me = 
          fmap (if me 
                  then const $ routeRenderer . StaticR $
                         knowledgeToRoute (Knowledge Mystery Mystery)
                  else routeRenderer . StaticR . cardToRoute)
          newcard

        color :: Color
        color = cardColor oldcard
                
        newBoardCell :: Text
        newBoardCell = render [hamlet|
            <img src=@{StaticR (cardToRoute oldcard)}>
          |]

        newDiscardTable :: Text
        newDiscardTable = render $ discardTable color (getDiscards g color)

        highlightPlayer :: GameEvent
        highlightPlayer = case gameStatus g of
                            Running {currentP=newCP} -> GEHighlightPlayer newCP
                            _ -> GEUnhighlightPlayer

        replaceContent = GEReplaceContent [
           if success
              then ContentUpdate (boardCellId color) newBoardCell
              else ContentUpdate (discardTableId color) newDiscardTable
          ,ContentUpdate "hintstd" (render $ hintsTdContent $ gameHints g)
          ,ContentUpdate "decksizetd" (render $ hintsTdContent $ length $ gameDeck g)
          ,ContentUpdate "strikestd" (render $ strikesTdContent $ gameStrikes g)
          ]

      return (\me ->
        [highlightPlayer
        ,GEPlay {gePlayPlayer = currentP, gePlayCard = oldcardi,
                 gePlayNewCard = newcardRoute me}
        ,replaceContent])
  in
    postActionHandler inputForm attemptAction handleResult


