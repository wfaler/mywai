
import Web.MyWai
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)


import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Either

import Control.Monad.IO.Class
import Control.Monad.Trans
--import Control.Monad.State


runFoo :: IO (Maybe String)
runFoo = runMaybeT foo

foo :: MaybeT IO String
foo = do
  fmap ("f" ++) ((MaybeT . return . Just) "5")
  

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
  runReaderT r2 5
  putStrLn "done"
  
r2 :: ReaderT Int IO ()
r2 = do
  res <- runMaybeT $ do
    r <- respond2
    b <- respond3
    return (r ++ b)
  list <- respond4
  lift $ putStrLn $ show list
  lift $ putStrLn $ (show $ res) ++ " in r2"

  
respond2 :: MaybeT (ReaderT Int IO) String
respond2 = do
  e <- lift $ (ask :: (ReaderT Int IO) Int)
  liftIO $ putStrLn "in respond2"
  bar <- MaybeT $ return $ Just ("foo" ++ (show e))
  return bar

respond3 :: MaybeT (ReaderT Int IO) String
respond3 = do
  e <- lift $ (ask :: (ReaderT Int IO) Int)
  liftIO $ putStrLn "in respond3"
  bar <- MaybeT $ return $ Just ("foo" ++ (show e))
  return bar


-- how to do with other than transformers?
respond4 :: (ReaderT Int IO) [String]
respond4 = do
  e <- (ask :: (ReaderT Int IO) Int)
  return $ ["foo", "bar", show e]
