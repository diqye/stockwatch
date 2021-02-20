{-# LANGUAGE OverloadedStrings #-}

module Message.Jin10 (jin10sockets) where
import qualified Data.Text           as T
import qualified Data.ByteString as B
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Wuss
import qualified Control.Exception as E





jin10sockets :: IO ()
jin10sockets = runSecureClient "wss-flash-1.jin10.com" 443 "/" $ app

app :: WS.ClientApp ()
app conn = do
  putStrLn "Connected"
  E.try $ app' conn :: IO (Either E.SomeException ())
  jin10sockets

app' :: WS.ClientApp ()
app' conn =  do
  msg <- WS.receiveData conn
  C.putStrLn $ B.drop 3 msg
  app' conn