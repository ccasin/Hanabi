module Handler.Infrastructure
  (cardField, playerField, colorField, rankField,
   cardRowID, colorRowID, rankRowID,
   colorHintID, rankHintID,
   getChannel, getChannels, newChannel, deleteChannel, sendMessage,
   sgameid, requireName, requireGame, requireGameTransaction,
   getSetNameR, postSetNameR,
   GameListEvent(..), GameListMsg(..), PlayerListEvent(..), PlayerListMsg(..))   
where

import Data.IORef
import Data.Text (pack,append,strip)
import qualified Data.Text as T (length,concat)
import Data.List (foldl')
import Data.Maybe (isJust)

import System.Random (randomRIO)

import Control.Concurrent.Chan
import Control.Monad (liftM,unless)

import Network.Wai.EventSource
import Data.Aeson hiding (object)
import Data.Aeson.TH
import Blaze.ByteString.Builder (fromLazyByteString)
import Yesod.Auth
import Import

------------------------------------
------ Some constant strings -------

-- object fields

cardField, playerField, colorField, rankField :: Text
cardField        = "card"           -- Int
playerField      = "player"    -- Int
colorField       = "color"
rankField        = "rank"

-- css ids
cardRowID, colorRowID, rankRowID :: Text
cardRowID  = "CardRow"
colorRowID = "ColorRow"
rankRowID  = "RankRow"

colorHintID,rankHintID :: Int -> Text
colorHintID i = append (pack $ show i) colorRowID
rankHintID  i = append (pack $ show i) rankRowID


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

getChannels :: [Text] -> Handler [Chan ServerEvent]
getChannels ids =
  do iochans <- liftM gameChannels getYesod
     chans <- liftIO $ readIORef iochans
     return $ foldl (\cs uid -> case lookup uid chans of 
                                 Nothing -> cs
                                 Just c -> c:cs)
                    [] ids
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

deleteChannel :: Text -> Handler ()
deleteChannel nm =
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

------------------------
----- Name handlers ----

getSetNameR :: Handler RepHtml
getSetNameR =
  do Entity _ user <- requireAuth
     (widget,enctype) <- generateFormPost nameForm
     defaultLayout [whamlet|
       <div style="width:490px;padding:5px;margin:auto;border:1px solid black;">
         <p>Welcome to Hanabi.
  
         <p>Please chose a nickname.  This will be visible to other players.
                   
         <form method=post action=@{SetNameR} enctype=#{enctype}>
            ^{widget}
            <input type=submit value="Set nickname">
     |]

data NameResult = NameSuccess | NameTaken | NameInvalid

postSetNameR :: Handler ()
postSetNameR =
  do uid <- requireAuthId
     ((result, _), _) <- runFormPost nameForm
     case result of
       FormSuccess nmUnsanitized -> do
         let nm = strip nmUnsanitized
         nmResult <- if T.length nm < 1 then return NameInvalid else runDB $ do 
           taken <- liftM (not . null) $ selectList [UserName ==. nm] []
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
