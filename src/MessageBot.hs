module MessageBot (main, go) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM
import Data.Function
import Data.Maybe
import Data.Text (Text)
import Reddit
import Reddit.Types.Flair (Flair(Flair))
import Reddit.Types.Message
import System.Environment
import System.Exit
import qualified Data.Text as Text

data Action = ChangeFlair Text
            | StickyPost PostID
  deriving (Show, Read, Eq)

main :: IO ()
main = getArgs >>= \case
  (map Text.pack -> [user, pass]) ->
    go user pass
  _ -> do
    putStrLn "Invalid arguments"
    exitFailure

go :: Text -> Text -> IO ()
go user pass = do
  (output, input) <- split <$> newTChanIO
  as <- forM [messageLoop output, handleLoop input] $ \x -> async $ forever $ do
    res <- runReddit user pass x
    print res
    threadDelay $ 60 * 1000 * 1000
  mapM_ wait as

messageLoop :: ((Maybe Username, Action) -> IO ()) -> Reddit ()
messageLoop output = ($ Nothing) $ fix $ \loop x -> do
  Listing b _ ms <- getUnread' False (Options (Before <$> x) (Just 100))
  forM_ (filter new $ reverse ms) $ \m ->
    when (isPrivateMessage m) $
      case subject m of
        "flairtext" -> do
          markRead $ messageID m
          liftIO $ output (from m, ChangeFlair (Text.strip $ body m))
        "sticky" -> do
          markRead $ messageID m
          liftIO $ output (from m, StickyPost $ PostID $ Text.strip $ body m)
        _ -> return ()
  liftIO $ threadDelay $ 60 * 1000 * 1000
  case b of
    Just next -> loop $ Just next
    Nothing ->
      case ms of
        m:_ -> loop $ Just $ messageID m
        [] -> loop x

handleLoop :: IO (Maybe Username, Action) -> Reddit ()
handleLoop input = forever $ liftIO input >>= \case
  (Just user, ChangeFlair newFlair) ->
    if Text.length newFlair > 64
      then
        sendMessage user "There was a problem with your flairtext change" $ mconcat
          [ "The new flairtext you requested (\""
          , newFlair
          , "\") is unfortunately too long. You'll need to send me a new message "
          , "with a shorter text if you want it changed." ]
      else do
        Flair _ _ c <- lookupUserFlair (R "dota2") user
        setUserFlair (R "dota2") user newFlair $ fromMaybe "" c
  (Just (Username "D2TournamentThreads"), StickyPost pid) ->
    nest (stickyPost pid Nothing) >>= \case
      Right _ -> return ()
      Left err -> liftIO $ print err
  _ -> return ()

split :: TChan a -> (a -> IO (), IO a)
split chan = (atomically . writeTChan chan, atomically $ readTChan chan)
