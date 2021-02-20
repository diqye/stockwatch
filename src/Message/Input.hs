{-# LANGUAGE OverloadedStrings #-}
module Message.Input where

import System.IO.Unsafe
import Control.Concurrent.Chan
import qualified Web.Telegram.API.Bot as B
import Control.Concurrent
import qualified Data.List as L
import qualified Data.Text as T
import Data.String
import Control.Monad.IO.Class(liftIO)
import OutputStr

messageChan :: Chan B.Update
messageChan = unsafePerformIO $ newChan

readMessageChan :: IO B.Update
readMessageChan = readChan messageChan

is' :: (Integral a, Num b) => a -> b
is' = fromInteger . toInteger

type ChatId = Int
type ChatText = String
type UserId = Int
type UpdateId = Int

smpleInfo :: B.Update -> Maybe (ChatId,ChatText,UserId,UpdateId)
smpleInfo update  = do
  message <- B.message update
  from <- B.from message
  text <- B.text message
  let updateId = B.update_id update
  let chat = B.chat message
  pure $ ((is' . B.chat_id) chat,T.unpack text,(is' . B.user_id) from, is' updateId)

 

simpleFormat' :: B.Update -> String
simpleFormat' B.Update{B.update_id=id',B.message=Nothing} = show id' ++ " no message" 
simpleFormat' B.Update{B.update_id=id',B.message=(Just message)} = show id' ++ " " ++ T.unpack name ++ " in " ++ maybe "Private chat" T.unpack chatName ++ " says " ++ T.unpack text
  where text = maybe "" id $ B.text message
        chatName = do
          chatTitle <- B.chat_title chat
          pure $ chatTitle <> "(" <> fromString (show $ B.chat_id chat) <> ")"
        name = case from of
                Nothing -> "NoFrom"
                (Just from') ->B.user_first_name from' <>  " " <> (maybe "" id $ B.user_last_name from') <> "(" <> fromString (show $ B.user_id from') <> ")"
        chat = B.chat message
        from = B.from message


longPolling :: Int -> B.TelegramClient ()
longPolling offset = do
  (B.Response updates _) <- B.getUpdatesM $ B.GetUpdatesRequest (Just offset) Nothing (Just 20) Nothing
  let logs = map simpleFormat' updates 
  let log = liftIO $ output $ "longPolling::" ++ L.intercalate "\n" logs 
  liftIO $ do
    writeList2Chan messageChan $ updates
  if null updates then pure () else log
  let newOffset = if null updates then offset else B.update_id (last updates) + 1
  longPolling newOffset




{- 
data B.Message
  = B.Message {B.message_id :: Int,
               B.from :: Maybe B.User,
               B.date :: Int,
               B.chat :: B.Chat,
               B.forward_from :: Maybe B.User,
               B.forward_from_chat :: Maybe B.Chat,
               B.forward_from_message_id :: Maybe Int,
               B.forward_signature :: Maybe T.Text,
               B.forward_date :: Maybe Int,
               B.reply_to_message :: Maybe B.Message,
               B.edit_date :: Maybe Int,
               B.media_group_id :: Maybe T.Text,
               B.author_signature :: Maybe T.Text,
               B.text :: Maybe T.Text,
               B.entities :: Maybe [B.MessageEntity],
               B.caption_entities :: Maybe [B.MessageEntity],
               B.audio :: Maybe B.Audio,
               B.document :: Maybe B.Document,
               B.game :: Maybe B.Game,
               B.photo :: Maybe [B.PhotoSize],
               B.sticker :: Maybe B.Sticker,
               B.video :: Maybe B.Video,
               B.voice :: Maybe B.Voice,
               B.video_note :: Maybe B.VideoNote,
               B.caption :: Maybe T.Text,
               B.contact :: Maybe B.Contact,
               B.location :: Maybe B.Location,
               B.venue :: Maybe B.Venue,
               B.new_chat_member :: Maybe B.User,
               B.new_chat_members :: Maybe [B.User],
               B.left_chat_member :: Maybe B.User,
               B.new_chat_title :: Maybe T.Text,
               B.new_chat_photo :: Maybe [B.PhotoSize],
               B.delete_chat_photo :: Maybe Bool,
               B.group_chat_created :: Maybe Bool,
               B.supergroup_chat_created :: Maybe Bool,
               B.channel_chat_created :: Maybe Bool,
               B.migrate_to_chat_id :: Maybe GHC.Int.Int64,
               B.migrate_from_chat_id :: Maybe GHC.Int.Int64,
               B.pinned_message :: Maybe B.Message,
               B.invoice :: Maybe B.Invoice,
               B.successful_payment :: Maybe B.SuccessfulPayment}
data B.User
  = B.User {B.user_id :: Int,
            B.user_is_bot :: Bool,
            B.user_first_name :: T.Text,
            B.user_last_name :: Maybe T.Text,
            B.user_username :: Maybe T.Text,
            B.user_language_code :: Maybe B.LanguageCode}
-}