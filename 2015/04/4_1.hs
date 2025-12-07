{-# LANGUAGE OverloadedStrings #-}

import Crypto.Hash (hash, Digest)
import Crypto.Hash.Algorithms (MD5)
import qualified Data.ByteString.Char8 as B
import Data.Bits ( (.&.) )

prefix :: B.ByteString
prefix = "iwrupvqb"

isGood :: B.ByteString -> Bool
isGood bs =
    let d = hash bs :: Digest MD5       
        h = B.pack (show d)             
    in  B.isPrefixOf "00000" h          

main :: IO ()
main = print (go 0)
  where
    go !n =
        let s = prefix <> B.pack (show n)
        in  if isGood s
            then n
            else go (n+1)
