
import Web.MyWai
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)


import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

import Control.Monad.IO.Class
import Control.Monad.Trans
--import Control.Monad.State


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
  runReaderT r2 "bar"
  putStrLn "done"
  
r2 :: ReaderT String IO ()
r2 = do
  res <- runMaybeT $ do
    r <- respond2
    b <- respond3
    return (r ++ b)
  lift $ putStrLn $ (show $ res) ++ " in r2"
  
  
respond2 :: MaybeT (ReaderT String IO) String
respond2 = do
  e <- lift $ (ask :: (ReaderT String IO) String)
  liftIO $ putStrLn "in respond2"
  bar <- MaybeT $ return $ Just ("foo" ++ e)
  return bar

respond3 :: MaybeT (ReaderT String IO) String
respond3 = do
  e <- lift $ (ask :: (ReaderT String IO) String)
  liftIO $ putStrLn "in respond3"
  bar <- MaybeT $ return $ Just ("foo" ++ e)
  return bar
