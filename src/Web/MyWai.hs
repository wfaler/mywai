{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
module Web.MyWai(
  ContentType(..),Cookie(..),HttpRequest(..),cookieHeader,contentType, removeCookie,bodyParams,jsonBody,jsonOk, parseRequest
  )
where

import Network.Wai (responseLBS)
import Network.HTTP.Types (status201, status204, status303, status404, status400,status401, status200, Header, Status)
import Network.HTTP.Types.URI
import Network.HTTP.Types.Header (hContentType,hLocation, HeaderName)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set as SET
import Data.Maybe
import Data.Aeson
import Network.Wai.Internal
import qualified Data.Map as M
import Network.HTTP.Types.Method

data ContentType = JsonContent | TxtContent | HtmlContent

data Cookie  = Cookie { cookieName :: T.Text, cookieValue :: T.Text }deriving(Show,Eq)

data HttpRequest = HttpRequest { path :: [T.Text], method :: StdMethod,
                                 urlParams :: [(T.Text, [T.Text])],
                                 headers :: [(HeaderName, T.Text)], cookies :: [Cookie],
                                 body :: Maybe B.ByteString } deriving (Show,Eq)
                                             
-- redirect, notfound, nocontent, unauthorized, internalerror

redirect303 :: C8.ByteString -> [Header] -> IO Response
redirect303 redirectTo hdrs = return $ responseLBS status303 ((hLocation, redirectTo) : hdrs) ""

jsonOk :: ToJSON a => a -> Response
jsonOk obj = responseLBS status200 [contentType JsonContent] (encode obj)

cookieHeader :: Cookie -> Header
cookieHeader cookie = ("Set-Cookie", TE.encodeUtf8 (T.concat [(cookieName cookie), "=", (cookieValue cookie), "; Path=/"]))

removeCookie :: T.Text -> Header
removeCookie name = ("Set-Cookie", TE.encodeUtf8 (T.concat [name, "=deleted; path=/; expires=Thu, 01 Jan 1970 00:00:00 GMT"]))

contentType :: ContentType -> Header
contentType JsonContent = (hContentType, "application/json; charset=utf-8")
contentType TxtContent = (hContentType, "text/plain; charset=utf-8")
contentType HtmlContent = (hContentType, "text/html; charset=utf-8")

jsonBody :: FromJSON a => HttpRequest -> Maybe a
jsonBody req = do
  bdy <- body req
  (decode . BL.fromStrict) bdy

--param :: T.Text -> HttpRequest -> Maybe [T.Text]
--param name req = findurlParams

--bodyParam :: T.Text -> HttpRequest -> Maybe [T.Text]
--bodyParam name req = findurlParams

bodyParams :: HttpRequest -> Maybe [(T.Text,[T.Text])]
bodyParams = (fmap (getParams . parseQuery)) . body

parseRequest :: Request -> IO HttpRequest
parseRequest req = let path = pathInfo req
                       method = read (C8.unpack (requestMethod req)) :: StdMethod
                       urlParams = getParams $ queryString req
                       headers = fmap (\h -> (fst h, TE.decodeUtf8 $ snd h)) (requestHeaders req)
                       cookies = getCookies headers
                   in
                    do
                      maybeReqBody <- asRequestBody req method
                      return $ HttpRequest path method urlParams headers cookies maybeReqBody
                        where
                          asRequestBody r PUT = bodyChunks r []
                          asRequestBody r POST = bodyChunks r []
                          asRequestBody _ m = return  Nothing
                          
-- private functions
getCookies :: [(HeaderName, T.Text)]  -> [Cookie]
getCookies headers =  catMaybes $ fmap (parseCookie . (T.split (== '='))) $ fromMaybe [] $ fmap (T.split (== ';')) (M.lookup "Cookie" (M.fromList headers))
  where
    parseCookie (name : xs) =  Just $ Cookie (T.strip name) (T.strip (mconcat xs))
    parseCookie [] = Nothing

getParams :: Query -> [(T.Text, [T.Text])]
getParams query = let paramMaybes = fmap (\paramPair -> ((TE.decodeUtf8 (fst paramPair)), fmap (TE.decodeUtf8 . (urlDecode True)) (snd paramPair))) query
                      keys = (SET.toList . SET.fromList . (fmap fst)) paramMaybes
                  in
                   fmap
                     (\key ->
                       (key, (catMaybes $ fmap snd (filter (\k -> (fst k) == key) paramMaybes))))
                     keys                     

bodyChunks :: Request -> [C8.ByteString] -> IO (Maybe B.ByteString)
bodyChunks req list = do
  chunk <- requestBody req
  if chunk == B.empty
    then maybeB (reverse list)
    else bodyChunks req (chunk : list)
  where
    maybeB [] = return Nothing
    maybeB l = (return . Just . B.concat) l
