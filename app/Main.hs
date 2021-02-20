{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LB
import Network.HTTP.Client
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.TLS as CT
import Data.String(fromString)
import Data.String.Conversions(cs)
import qualified Text.Parsec as P
import Control.Concurrent(threadDelay)
import qualified Web.Telegram.API.Bot as B
import Control.Monad.IO.Class(liftIO)
import qualified Data.Aeson as A
import Control.Monad(guard)
import Control.Applicative
import Message.Input
import Control.Concurrent
import Control.Monad.Trans.Maybe
import NatureLang
import Control.Monad
import System.IO.Unsafe
import Control.Concurrent.MVar

threadId :: MVar ThreadId
threadId =  unsafePerformIO $ newEmptyMVar 

main :: IO ()
main  = do
  a <- forkIO $ runClient $ longPolling 0
  putMVar threadId a
  print a
  forever $ do
    update <- readMessageChan
    runMaybeT $ handle update
    pure ()

handle :: B.Update -> MaybeT IO ()
handle update = do
  (chatId,chatText,userId,updateId) <- MaybeT $ pure $ smpleInfo update
  analysisResult <- eitherToMaybeT $ P.parse analysis "diqye" chatText
  -- liftIO $ print analysisResult
  case analysisResult of
    Song -> liftIO $ sendSong (chatId,updateId)
    StockNow stock -> sendStockNow stock chatId 

sendSong :: (Int,Int) -> IO ()
sendSong (chatId,updateId) = runClient $ do
  let text = songlist !! (updateId  `mod` length songlist)
  B.sendMessageM $ B.sendMessageRequest (B.ChatId $ is' chatId) text

songlist = 
  [ "https://www.youtube.com/watch?v=8rZydHFUmFk&t=22s"
  , "https://www.youtube.com/watch?v=lnWzPKhYFhU&list=RDlnWzPKhYFhU&start_radio=1"
  , "https://www.youtube.com/watch?v=ncPPgXKGsp4"
  , "https://www.youtube.com/watch?v=hKwf4JI0hII"
  , "https://www.youtube.com/watch?v=bL9krjcyK3w"
  , "https://www.youtube.com/watch?v=d647pqMq3V4"
  , "https://www.youtube.com/watch?v=3BfOmIPTnuI"
  , "https://www.youtube.com/watch?v=pk9gPL1g5Wo"
  ]

sendStockNow :: Stock -> Int -> MaybeT IO ()
sendStockNow Gold = sendStockNow' "https://m.investing.com/currencies/xau-usd-historical-data"
sendStockNow Silver = sendStockNow' "https://m.investing.com/currencies/xag-usd-historical-data"
sendStockNow ZW = sendStockNow' "https://m.investing.com/commodities/us-wheat-historical-data"
sendStockNow DXY = sendStockNow' "https://m.investing.com/indices/usdollar-historical-data"
sendStockNow A50 = sendStockNow' "https://m.investing.com/indices/china-a50-historical-data"
sendStockNow CNH = sendStockNow' "https://m.investing.com/currencies/usd-cnh-historical-data"
sendStockNow TESLA = sendStockNow' "https://m.investing.com/equities/tesla-motors-historical-data"

sendStockNow' :: String -> Int -> MaybeT IO ()
sendStockNow' url chatId = do
  content <- liftIO $ fetchWithChromeAgent url
  stockView <- eitherToMaybeT $ P.parse parsecStockViewFromInvesing "invesing" content
  liftIO $ runClient $ do
    let requestParam = B.sendMessageRequest (B.ChatId $ is' chatId) $ fromString $ prettyStock stockView url
    let requestP = requestParam {B.message_parse_mode=Just B.Markdown,B.message_disable_web_page_preview=Just True}
    B.sendMessageM requestP 
    pure ()


-- main = do
--   watchList
--    [ "https://m.investing.com/currencies/nzd-usd-historical-data"
--    -- , "https://m.investing.com/commodities/us-wheat-historical-data"
--    ]

testupdates :: Int -> B.TelegramClient ()
testupdates offset = do
  (B.Response updates _) <- B.getUpdatesM $ B.GetUpdatesRequest (Just offset) Nothing (Just 20) Nothing
  let simples = map simpleFormat'' updates
  liftIO $ print $ simples
  let newOffset = if length simples == 0 then offset else fst (last simples) + 1
  testupdates newOffset


simpleFormat'' :: B.Update -> (Int,Maybe T.Text)
simpleFormat'' B.Update{B.update_id=id,B.message=(Just B.Message{B.text=text})} = (id,text)

runClient :: B.TelegramClient a -> IO ()
runClient clientMonad = do
  let token = B.Token "bot1449676439:AAHBpnIYURQT237qU4U01R2Lct3twjDm1ZA" -- entire Token should be bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11
  manager <- HC.newManager CT.tlsManagerSettings
  r <- B.runTelegramClient token manager clientMonad
  pure ()

sendMeAMessage :: T.Text -> B.TelegramClient ()
sendMeAMessage text = do
   let msg = B.sendMessageRequest (B.ChatId 1129803474) text
   B.sendMessageM msg
   pure ()


robotaction :: B.TelegramClient ()
robotaction = do
  -- b <- B.getWebhookInfoM
  -- a <- B.getMeM
  -- c <- B.getUpdatesM'
  -- liftIO $ print $ A.encode $ c
  -- let msg = B.sendMessageRequest (B.ChatId 1129803474) "Hello"
  -- B.sendMessageM msg
  a <- B.getChatM "1129803474"
  liftIO $ print a
  pure ()


prettyStock :: StockView -> String -> String
prettyStock stock url= 
    takeWhile (/= '@') (stockName stock) 
    <> " [" <> show (stockPrice stock) <> "](" <> url <> ") "
    <>  "_" <> stockChange stock <> "_"

data StockView = StockView 
  { stockName :: String
  , stockTimeDescription :: String
  , stockChange :: String
  , stockPrice :: Double
  , stockRateChange :: String
  } deriving Show


fetchWithChromeAgent :: String -> IO String
fetchWithChromeAgent url = do
  let req = (fromString url) { HC.requestHeaders = [("User-Agent","Mozilla/5.0 (iPhone; CPU iPhone OS 13_2_3 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/13.0.3 Mobile/15E148 Safari/604.1")] }
  manager <- HC.newManager CT.tlsManagerSettings
  text <- pure HC.responseBody <*> HC.httpLbs req manager
  pure $ cs $ text


parsecStockViewFromInvesing :: P.Parsec String () StockView
parsecStockViewFromInvesing  = do
  stockName' <- readATagText "h1"
  stockPrice' <- readStockPrice
  stockChange' <- readStockChange
  stockRateChange' <- readStockRateChange
  stockTimeDescription' <- readStockTimeDescription
  pure $ StockView 
    { stockName = filter (`notElem` (" \t\n"::String)) stockName'
    , stockTimeDescription = stockTimeDescription'
    , stockChange = stockChange'
    , stockPrice = stockPrice'
    , stockRateChange = stockRateChange'
    }


consumeToString :: String -> P.Parsec String () String
consumeToString str = do
  success <- successMatched P.<|> anyCharMatched
  if success then pure "" else (pure (:) <*> P.anyChar <*> consumeToString str)
  where successMatched = (P.try $ P.lookAhead $ P.string str) *> pure True
        anyCharMatched = (P.lookAhead $ P.anyChar) *> pure False

readATagText :: String -> P.Parsec String () String
readATagText tag = do
  let prefixTag = '<':tag
  let suffixTag = '<':'/':tag
  consumeToString prefixTag
  P.many $ P.noneOf ">"
  P.anyChar
  r <- consumeToString suffixTag
  P.string suffixTag
  P.anyChar
  pure r



readStockPrice :: P.Parsec String () Double
readStockPrice = do
  consumeToString "quotesBoxTop"
  digit <- readATagText "span"
  pure $  read $ filter (/= ',') digit



readStockChange :: P.Parsec String () String
readStockChange = do
  consumeToString "quotesChange"
  a <- readATagText "i"
  pure $ filter (`notElem` (" \n\r"::String)) a

readStockRateChange :: P.Parsec String () String
readStockRateChange  = do
  a <- readATagText "i"
  pure $ filter (`notElem` (" \n\r"::String)) a

readStockTimeDescription :: P.Parsec String () String
readStockTimeDescription = do
  consumeToString "clockPos"
  time <- readATagText "i"
  desc <- consumeToString "<span"
  pure $ time ++ " " ++ desc

