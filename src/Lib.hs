module Lib where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Int
import Data.Word
import Data.Bits
import Control.Applicative

data ParseState = ParseState {
    string :: L.ByteString,
    offset :: Int64
} deriving (Show)

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset state = \new -> state { offset = new}

modifyString :: ParseState -> L.ByteString -> ParseState
modifyString state = \string -> state { string = string}

newtype Parse a = Parse {
    runParse :: ParseState -> Either String (a,ParseState)
}

identity :: a -> Parse a
identity a = Parse (\state -> Right (a,state))

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState

instance Functor Parse where
   fmap f parser = parser ==> \result -> identity (f result)

(|>) :: Parse a -> Parse b -> Parse b
(|>) pa pb = pa ==> (\_ -> pb)

toWord32 :: [Word8] -> Word32
toWord32 (a:b:c:d:[]) = foldr (\a b -> (shiftL a 8) + b) (fromIntegral a) (map fromIntegral (b:c:d:[]))
toWord32 _ = error "size error !"