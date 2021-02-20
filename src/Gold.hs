{-# LANGUAGE OverloadedStrings #-}
module Gold where

import Data.Aeson
import Data.String.Conversions

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

readCandle :: IO [Candle]
readCandle = do
  goldcsv <- readFile "data/gold.csv"
  let goldlines = drop 1 $ lines goldcsv
  let goldlines' = reverse goldlines
  let decodeLine = toCandle . decodeJust . decode . cs . perfect
  let xs = map decodeLine goldlines' :: [Candle]
  -- let decodeLine = decode . cs . perfect
  -- let xs = map decodeLine goldlines' :: [Maybe OriginData]
  -- print xs
  pure xs

goldMain = do
  candles <- readCandle
  traceCandle candles (Nothing,Nothing) (Watching [])

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
  putStrLn $ "Total : " ++ show len ++ "\n" ++
    "Win / Lose : " ++ show winLen ++  "/" ++ show loseLen ++ "\t Ratio " ++ show (fromIntegral winLen / fromIntegral len * 100) ++ "%\n" ++
    "Win profit / Lose Profit : " ++ (show $ foldl (\s o->s + close_order_profit o) 0 wins) ++ "/" ++ (show $ foldl (\s o->s + close_order_profit o) 0 loses)

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
      traceCandle (x:xs) (Just aCandle,Just bCandle) $ (Doing a closedOrders)
traceCandle (x:xs) (Just aCandle,Just bCandle) (Doing order closedOrders) = do
  case open_order_type order of
    Buy -> buyCheck
    Sell -> sellCheck
  where buyCheck | open_order_stop_limit order >= candle_low x = do
          let closedOrder = CloseOrder (open_order_stop_limit order) (open_order_stop_limit order - open_order_price order) (candle_desc x) order
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
                 | open_order_target_profit order <= candle_high x = do
          let closedOrder = CloseOrder (open_order_target_profit order) (open_order_target_profit order - open_order_price order) (candle_desc x) order
          putStrLn "TP"
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))

                 | candle_close x < candle_close aCandle && (candle_close bCandle < candle_close aCandle || isBadCandle bCandle)= do
          let closedOrder = CloseOrder (candle_close x) (candle_close x - open_order_price order) (candle_desc x) order
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
                 | otherwise = traceCandle xs (Just bCandle, Just x) (Doing order closedOrders)
        sellCheck | open_order_stop_limit order <= candle_high x = do
          let closedOrder =  CloseOrder (open_order_stop_limit order) (open_order_price order - open_order_stop_limit order) (candle_desc x) order
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
                 | open_order_target_profit order >= candle_low x = do
          putStrLn "TP"
          let closedOrder = CloseOrder (open_order_target_profit order) (open_order_price order - open_order_target_profit order) (candle_desc x) order
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
                 | candle_close x > candle_close aCandle && (isBadCandle bCandle || candle_close bCandle > candle_close aCandle) = do
          let closedOrder = CloseOrder (candle_close x) (open_order_price order - candle_close x) (candle_desc x) order
          printOrder closedOrder
          traceCandle xs (Just bCandle, Just x) (Watching (closedOrder:closedOrders))
                 | otherwise = traceCandle xs (Just bCandle, Just x) (Doing order closedOrders)

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


data IdeaType = IdeaRise | IdeaFall | IdeaOther deriving Show

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
  case (testMyIdea recent1,testMyIdea recent2) of
    (IdeaRise,IdeaRise) ->
      -- Nothing
      if candle_close recent1 > candle_close recent2 then Just buyOrder else Nothing
    (IdeaFall,IdeaFall) -> 
      -- Nothing
      if candle_close recent1 < candle_close recent2 then Just sellOrder else Nothing
    _ -> Nothing
  where buyOrder = OpenOrder
          { open_order_desc = candle_desc recent1
          , open_order_price = price
          , open_order_stop_limit = candle_low recent2
          , open_order_target_profit = 1000000
          , open_order_type = Buy
          }
          where price = candle_close recent1 
        sellOrder = OpenOrder
          { open_order_desc = candle_desc recent1
          , open_order_price = price
          , open_order_stop_limit = candle_high recent2
          , open_order_target_profit = 0
          , open_order_type = Sell
          }
          where price = candle_close recent1