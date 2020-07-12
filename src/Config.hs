module Config where

import Data.ByteString (ByteString)
data Config = 
    Config
    { dbHost :: ByteString
    , dbPort :: ByteString
    , dbUsername:: ByteString
    , dbUserPass::ByteString
    }