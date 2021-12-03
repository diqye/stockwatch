{-# LANGUAGE OverloadedStrings #-}
module Ploy.PloyMain where 

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

  