{-# LANGUAGE OverloadedStrings #-}
module NatureLang where

import Control.Monad.Trans.Maybe

import Data.String(fromString)
import Data.String.Conversions(cs)
import Text.Parsec((<|>))
import qualified Text.Parsec as P
import Control.Monad
import Control.Monad.Trans.Maybe

{-
  词法分析
  1. 比特币多少钱
    a. 可恶的比特币多少钱了 Trigger
    b. 比特币昨天多少钱     
  2. 特斯拉在1000以上提醒我
-}
eitherToMaybeT :: Either a b -> MaybeT IO b
eitherToMaybeT (Left _) = MaybeT $ pure Nothing
eitherToMaybeT (Right b) = MaybeT $ pure $ Just b

data Stock = 
    Gold |
    Silver |
    ZW |
    DXY |
    A50 |
    CNH |
    TESLA |
    Unknow String
    deriving Show

data Analysis = Song | StockNow Stock deriving Show


analysis :: P.Parsec String () Analysis
analysis = P.try stockAnalysis <|> P.try songP

stockAnalysis :: P.Parsec String () Analysis
stockAnalysis = stockAnalysis1 <|> stockAnalysis2

stockAnalysis1 :: P.Parsec String () Analysis
stockAnalysis1 = do
    P.try (P.string "我想看") <|> P.string "看一下" <|> P.string "Show" <|> P.string "show"
    P.spaces
    stockP

stockAnalysis2 :: P.Parsec String () Analysis
stockAnalysis2 = do
  a <- stockP
  P.spaces
  P.string "现在什么价位"
  pure a

songP :: P.Parsec String () Analysis
songP = do
  consumeToString1 "想听歌"
  pure Song

stockP :: P.Parsec String () Analysis
stockP = msum 
  [ P.string "黄金" >> stockNow Gold
  , (P.string "Gold" <|> P.string "gold") >> stockNow Gold
  , (P.string "Silver" <|> P.string "Silver") >> stockNow Silver
  , P.string "白银" >> stockNow Silver
  , P.string "小麦" >> stockNow ZW
  , P.string "美元" >> stockNow DXY
  , P.string "ZW" >> stockNow ZW
  , P.string "zw" >> stockNow ZW
  , P.string "DXY" >> stockNow DXY
  , P.string "dxy" >> stockNow DXY
  , P.string "A50" >> stockNow A50
  , P.string "富时中国" >> stockNow A50
  , P.string "人民币" >> stockNow CNH
  , P.string "TSLA" >> stockNow TESLA
  , P.string "特斯拉" >> stockNow TESLA
 
  ]
  where stockNow a = pure $ StockNow a


consumeToString1 :: String -> P.Parsec String () String
consumeToString1 str = do
  success <- successMatched P.<|> anyCharMatched
  if success then pure "" else (pure (:) <*> P.anyChar <*> consumeToString1 str)
  where successMatched = (P.try $ P.lookAhead $ P.string str) *> pure True
        anyCharMatched = (P.lookAhead $ P.anyChar) *> pure False