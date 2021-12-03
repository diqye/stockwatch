{-# LANGUAGE OverloadedStrings #-}
module Gold where

import Data.Aeson
import Data.String.Conversions
-- import Data.HashMap.Strict 

data TimeKind = H1 | H3 | Day deriving Show
data Candle = Candle
  { candle_time_kind :: TimeKind
  , candle_desc::String
  , candle_close :: Float
  , candle_open :: Float
  , candle_high :: Float
  , candle_low :: Float
  } deriving Show

data OrderType = Buy | Sell deriving Show
data OpenOrder = OpenOrder
  { open_order_price :: Float
  , open_order_stop_limit :: Float
  , open_order_target_profit :: Float
  , open_order_desc :: String
  , open_order_type :: OrderType
  } deriving Show

data CloseOrder = CloseOrder
  { close_order_price :: Float
  , close_order_profit :: Float
  , close_order_desc :: String
  , close_order_open_order :: OpenOrder
  } deriving Show

perfect :: String -> String
perfect line = "[" ++ line ++ "]"

decodeJust :: Maybe a -> a
decodeJust (Just a) = a


-- type OriginData = (String,String,String,String,String,String)
type OriginData = [String]
toCandle ::  OriginData -> Candle
toCandle (date:price:open:high:low:_) =
  Candle Day date (toFloat price) (toFloat open) (toFloat high) (toFloat low)
  where toFloat :: String -> Float
        toFloat str = read $ filter (/= ',') $ str

readCandleFromInvesting :: IO [Candle]
readCandleFromInvesting = do
  goldcsv <- readFile "data/eth.csv"
  let goldlines = drop 1 $ lines goldcsv
  let goldlines' = reverse goldlines
  let decodeLine = toCandle . decodeJust . decode . cs . perfect
  let xs = map decodeLine goldlines' :: [Candle]
  -- let decodeLine = decode . cs . perfect
  -- let xs = map decodeLine goldlines' :: [Maybe OriginData]
  -- print xs
  pure xs


readCandleFromTradingview :: IO [Candle]
readCandleFromTradingview = do
  json <- decodeFileStrict "data/tvgold.json"
  let decodeA = toCandle . trans 
  let xs = map decodeA $ decodeJust json :: [Candle]
  -- let decodeLine = decode . cs . perfect
  -- let xs = map decodeLine goldlines' :: [Maybe OriginData]
  -- print xs
  pure xs
  where trans :: [Float] -> [String]
        trans (x0:x1:x2:x3:x4:x5:xs) = [show x0,show x4,show x1,show x2,show x3]

-- 日线 P 34 S 34 == 54%
-- H1  P 13 S 8  == 52%
-- D1  P 0.08 S 0.05
{- 
-- A50 Day
profitOffset price = price * 0.08
stopOffset price = price * 0.03
-}


-- Gold H1  P 0.006 S 0.005
profitOffset price = price * 0.006
stopOffset price = price * 0.005

goldMain = do
  -- candles <- readCandleFromInvesting
  candles <- readCandleFromTradingview
  traceCandle candles (Nothing,Nothing) (Watching [])
  -- xs <- rrf candles
  -- putStrLn $ show (length $ filter (\x->fst x == IdeaRise) xs)
  -- putStrLn $ show (length $ filter (\x->fst x == IdeaFall) xs)
  -- putStrLn $ show (foldl (+) 0 $ map (\(_,b)->b) xs)

-- 涨涨跌后面涨的概率
rrf :: [Candle] -> IO [(IdeaType,Float)]
rrf (a:b:c:xs) = case (testMyIdea a,testMyIdea b) of
  (IdeaFall,IdeaFall) -> do
    let a = (testMyIdea c,candle_close b - candle_close c)
    putStrLn $ show $ a
    xs <- rrf (b:c:xs)
    pure (a:xs)
  _ -> rrf (b:c:xs)
rrf _ = pure []

data TraceState = 
  Watching [CloseOrder]| 
  Doing OpenOrder [CloseOrder] deriving Show

traceCandle :: [Candle] -> (Maybe Candle,Maybe Candle) -> TraceState-> IO ()
traceCandle [] _ state = do
  let len = length closedOrders
  let wins =  filter isWin closedOrders
  let loses = filter isLose closedOrders
  let winLen = length wins
  let loseLen = length loses
  let winProfit = foldl (\s o->s + close_order_profit o) 0 wins
  let loseProfit = foldl (\s o->s + close_order_profit o) 0 loses
  putStrLn $ "Total : " ++ show len ++ "\n" ++
    "Win / Lose : " ++ show winLen ++  "/" ++ show loseLen ++ "\t Ratio " ++ show (fromIntegral winLen / fromIntegral len * 100) ++ "%\n" ++
    "Win profit / Lose Profit : " ++ show winProfit ++ "/" ++ show loseProfit ++ "\tWin+Lose:" ++ (show (winProfit + loseProfit))
  where closedOrders = case state of
          Watching closeds -> closeds
          Doing _ closeds -> closeds 
        isWin order =  close_order_profit order > 0
        isLose order = not $ isWin order

traceCandle (x:xs) (a,Nothing) state = traceCandle xs (a,Just x) state
traceCandle (x:xs) (Nothing,_) state = traceCandle xs (Just x,Nothing) state
traceCandle (x:xs) (Just aCandle,Just bCandle) (Watching closedOrders)= do
  let a = openOrderNow (aCandle,bCandle)
  case a of
    Nothing -> traceCandle xs (Just bCandle,Just x) (Watching closedOrders)
    Just a -> do
      -- printOpenOrder a
      traceCandle (x:xs) (Just aCandle,Just bCandle) $ (Doing a closedOrders)
traceCandle (x:xs) (Just aCandle,Just bCandle) (Doing order closedOrders) = do
  case open_order_type order of
    Buy -> buyCheck
    Sell -> sellCheck
  where buyCheck | open_order_stop_limit order >= candle_low x = do
          let closedOrder = CloseOrder (open_order_stop_limit order) (open_order_stop_limit order - open_order_price order) (candle_desc x) order
          -- putStrLn "--------------Close by SL------------------------"
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
                 | open_order_target_profit order <= candle_high x = do
          let closedOrder = CloseOrder (open_order_target_profit order) (open_order_target_profit order - open_order_price order) (candle_desc x) order
          -- putStrLn "--------------Close by TP--------------------------"
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
          {-
                 | candle_close x < candle_close aCandle && (candle_close bCandle < candle_close aCandle || isBadCandle bCandle)= do
          let closedOrder = CloseOrder (candle_close x) (candle_close x - open_order_price order) (candle_desc x) order
          -- putStrLn "----------------Close by NT------------------------"
          -- printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
          -}
                 | otherwise = traceCandle xs (Just bCandle, Just x) (Doing order closedOrders)
        sellCheck | open_order_stop_limit order <= candle_high x = do
          let closedOrder =  CloseOrder (open_order_stop_limit order) (open_order_price order - open_order_stop_limit order) (candle_desc x) order
          -- putStrLn "--------------Close by SL--------------------------"
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
                 | open_order_target_profit order >= candle_low x = do
          -- putStrLn "----------------Close by TP------------------------"
          let closedOrder = CloseOrder (open_order_target_profit order) (open_order_price order - open_order_target_profit order) (candle_desc x) order
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
          {-
                 | candle_close x > candle_close aCandle && (isBadCandle bCandle || candle_close bCandle > candle_close aCandle) = do
          let closedOrder = CloseOrder (candle_close x) (open_order_price order - candle_close x) (candle_desc x) order
          -- putStrLn "----------------Close by NT-----------------------"
          -- printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
          -}
                 | otherwise = traceCandle xs (Just bCandle, Just x) (Doing order closedOrders)

printOpenOrder :: OpenOrder -> IO ()
printOpenOrder order = do
  putStrLn $ (show $ open_order_type $ order) ++ "\t" ++ (show $ open_order_price  $ order) ++ "\tSL " ++ 
    (show $ open_order_stop_limit  $ order) ++ "\t@ " ++ (open_order_desc order)
printOrder :: CloseOrder -> IO ()
printOrder order = do
  putStrLn $ 
    (show $ open_order_type $ close_order_open_order order) ++ "\t" ++ (show $ open_order_price $ close_order_open_order $ order) ++ "\tSL " ++
    (show $ open_order_stop_limit $ close_order_open_order $ order) ++ "\t@ " ++ (open_order_desc $ close_order_open_order order) ++
    "\nClose\t" ++ show (close_order_price order) ++ "\tP  " ++ show (close_order_profit order) ++ "\t@ " ++ close_order_desc order ++
    "\n---------------------------------------------------------"

-- 是否是健康的K线 ---- 影线不大于实线
isBadCandle :: Candle -> Bool
isBadCandle candle | candle_open candle == candle_close candle =  True
                   | candle_open candle < candle_close candle = a' < candle_high candle - candle_close candle || a' < candle_open candle - candle_low candle
                   | otherwise =  b' < candle_high candle - candle_open candle || b' < candle_close candle - candle_low candle
  where a' = candle_close candle - candle_open candle
        b' = candle_open candle - candle_close candle


data IdeaType = IdeaRise | IdeaFall | IdeaOther deriving (Show,Eq)

testMyIdea :: Candle -> IdeaType
testMyIdea candle = 
  let a -- | isBadCandle candle = IdeaOther
        | change > 0 = IdeaRise
        | change < 0 = IdeaFall
        | otherwise = IdeaOther
  in a
  where change = candle_close candle - candle_open candle

        
openOrderNow :: (Candle,Candle) -> Maybe OpenOrder
openOrderNow (recent2,recent1) = 
  case (testMyIdea recent2,testMyIdea recent1) of
    (IdeaRise,IdeaRise) ->
      Nothing
      -- if candle_close recent1 > candle_close recent2 then Just buyOrder else Nothing
    (IdeaFall,IdeaFall) -> 
      if candle_close recent1 < candle_close recent2 then Just sellOrder else Nothing
    _ -> Nothing
  where buyOrder = OpenOrder
          { open_order_desc = candle_desc recent1
          , open_order_price = price
          , open_order_stop_limit = stopLimit
          , open_order_target_profit = price + profitOffset price
          , open_order_type = Buy
          }
          where price = candle_close recent1 
                -- stopLimit = candle_low recent2
                stopLimit = price - stopOffset price
        sellOrder = OpenOrder
          { open_order_desc = candle_desc recent1
          , open_order_price = price
          , open_order_stop_limit = stopLimit
          , open_order_target_profit = price - profitOffset price
          , open_order_type = Sell
          }
          where price = candle_close recent1
                -- stopLimit = candle_high recent2
                stopLimit = price + stopOffset price





goldH1PS = (0.006,0.005)
a50DayPS = (0.08,0.03)
longOrder :: (Double,Double) -> Double -> IO ()
longOrder (profit,stopLimit) price = do
  putStrLn $ "Buy " ++ show price
  putStrLn $ "SL " ++ (show $ price -  stopLimit * price)
  putStrLn $ "TP " ++ (show $ price + profit * price)
shortOrder :: (Double,Double) -> Double -> IO ()
shortOrder (profit,stopLimit) price= do
  putStrLn $ "Sell " ++ show price
  putStrLn $ "SL " ++ (show $ price + stopLimit * price)
  putStrLn $ "TP " ++ (show $ price - profit * price)