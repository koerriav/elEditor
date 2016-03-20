module Main where

import qualified Data.ByteString.Lazy as L
--import Control.Applicative
import ElParse
import Lib((==>),(|>))

main = do
    file <- L.readFile "el.data"
    print (fmap L.unpack $ parse parseHead file)
    print (parse (parseHead |> parseByte) file)
    print (fmap L.unpack $ parse (parseHead |> parseDWORD |> parseEntry |> parseEntry) file)
    print (fmap ( fmap L.unpack . parse (parseDWORD |> parseEntry ) . entries) $ parse (parseHead |> parseSection) file)