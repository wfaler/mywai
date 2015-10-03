{-# LANGUAGE OverloadedStrings #-}
module Web.Mail(MailConfig(..),MailConfigurator(..),sendEmail,mail) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Data.Text
import qualified Data.Text.Lazy as TL

import qualified Network.Mail.SMTP as ML
import Network.Mail.Mime
import Control.Concurrent

data MailConfig = MailConfig { server :: String, port :: Integer, login :: String, password :: String } deriving (Show, Eq)

class MailConfigurator a where
  mailConfig :: a -> MailConfig

instance MailConfigurator MailConfig where
  mailConfig = id

sendEmail :: MailConfigurator a => Mail -> (ReaderT a IO) ThreadId
sendEmail mail = do
  config <- fmap mailConfig (ask :: (ReaderT a IO) a)
  liftIO $ forkIO $ ML.sendMailWithLogin' (server config) (fromInteger (port config)) (login config) (password config) mail

from       = Address (Just "Code Qualified")  "hello@codequalified.com"
to         = [Address (Just "Wille Faler") "wille.faler@gmail.com"]
cc         = []
bcc        = []
subject    = "email subject"
body       = ML.plainTextPart "email body"
html       = ML.htmlPart "<h1>HTML</h1>"

mail = ML.simpleMail from to cc bcc subject [body, html]

mkMail :: Address -> [Address] -> Text -> TL.Text -> Mail
mkMail from to subject body = ML.simpleMail from to [] [] subject [ML.plainTextPart body, ML.htmlPart body]


