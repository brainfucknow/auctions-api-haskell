module AuctionSite.Worker where
import qualified AuctionSite.Domain.Commands as C
import           Control.Concurrent.STM
import           Control.Concurrent.Async
import           Control.Monad (forever)
import           Control.Exception (catch, SomeException)
import           System.IO (hPutStrLn, stderr)

startEventWorker :: ([C.Event] -> IO ()) -> TBQueue C.Event -> IO (Async a)
startEventWorker writeEvents queue = async $ forever $ do
    events <- atomically $ do
        event <- readTBQueue queue
        rest <- flushTBQueue queue
        return (event : rest)
    writeEvents events `catch` \(e :: SomeException) ->
        hPutStrLn stderr $ "Error writing events: " ++ show e

createEventHandler :: TBQueue C.Event -> IO (C.Event -> IO ())
createEventHandler queue = do
    return $ \event -> atomically $ writeTBQueue queue event
