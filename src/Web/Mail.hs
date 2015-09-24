{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
module Web.Mail(MailConfig(..),MailConfigurator(..),sendEmail) where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

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


