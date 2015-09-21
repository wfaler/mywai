
import Web.MyWai
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal as WI
import Data.Aeson
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.IO.Class
--import Control.Monad.State
import Control.Monad

main :: IO ()
main = do
    let port = 3000
--    pool <- createPool prodConn close 4 100 15
    putStrLn $ "Listening on port " ++ show port
    run port $ app-- $ pool
    
app ::  Application
app req f = do
  httpReq <- parseRequest req
--  putStrLn $ show httpReq
  putStrLn $ "****"
--  putStrLn $ show $ (jsonBody httpReq :: Maybe Value)
  f $ jsonOk "foo"

m :: IO ()
m = do
  res <- join $ runReaderT r2 "bar"
  putStrLn $ show $ res

--r2 :: ReaderT String IO (Maybe String)
r2 = do
  e <- ask
  return $ runMaybeT $ runReaderT respond e
  
  

respond :: ReaderT String (MaybeT IO) String
respond = do 
  e <- ask :: ReaderT String (MaybeT IO) String
  liftIO $ putStrLn $ "inside0, e: " ++ show e
  return ("foo" ++ e)

