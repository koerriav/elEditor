module ElParse where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Word
import Data.Int
import Lib

data Section = Section { entries :: L.ByteString, count :: Int64 } deriving (Show)

parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ -> identity byte
        where newState = initState { string = remainder,
                                     offset = offset initState + 1 }

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

parseDWORD :: Parse Word32
parseDWORD =
    getState ==> \initState ->
        case get 4 (string initState) of
          Nothing ->
              bail "no more input"
          Just (byte,remainder) ->
              putState newState ==> \_ -> identity (toWord32 $ L.unpack byte)
            where newState = initState { string = remainder,offset = offset initState + 4 }

peekDWORD :: Parse (Maybe Word32)
peekDWORD = (fmap fst . convert . (get 4) . string) <$> getState
    where
        convert :: Maybe (L.ByteString,L.ByteString) -> Maybe (Word32,L.ByteString)
        convert (Just (a,b))    = Just (toWord32 $ L.unpack a,b)
        convert Nothing         = Nothing

parseEntry :: Parse L.ByteString
parseEntry =
    getState ==> \initState ->
        case get 84 (string initState) of
          Nothing -> bail "no more input"
          Just (head,remainder) -> putState newState ==> \_ -> identity head
              where
                newState = initState {string = remainder,offset = offset initState + 84 }

peekEntry :: Parse (Maybe L.ByteString)
peekEntry = (fmap fst . (get 84) . string) <$> getState

parseHead :: Parse L.ByteString
parseHead =
    getState ==> \initState ->
    case get 8 (string initState) of
      Nothing -> bail "no more input"
      Just (head,remainder) -> putState newState ==> \_ -> identity head
          where
            newState = initState {string = remainder,
                                  offset = offset initState + 8 }

get :: Int64 -> L.ByteString -> Maybe (L.ByteString,L.ByteString)
get size str = if L.length str > size then Just (L.take size str,L.drop size str) else Nothing

parseSection :: Parse Section
parseSection =
    getState ==> \initState ->
        let count   = fromIntegral $ peekCount $ string initState
            size    = 84 in
        case get (size*count) (string initState) of
          Nothing -> bail "no more input"
          Just (head,remainder) -> putState newState ==> \_ -> identity (Section {entries = head,count = count})
              where
                newState = initState { string = remainder, offset = offset initState + (size*count) }
        where
          peekCount :: L.ByteString -> Word32
          peekCount ls =  case parse peekDWORD ls of
                            Left err -> 0
                            Right (Just r) -> r

parse :: Parse a -> L.ByteString -> Either String a
parse parser initState = case runParse parser (ParseState initState 0) of
    Left err            -> Left err
    Right (result,_)    -> Right result