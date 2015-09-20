
import Web.MyWai
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal as WI
import Data.Aeson

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
