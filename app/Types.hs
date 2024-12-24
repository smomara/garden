module Types where

import Data.Map (Map)

data Page = Page
    { pageTitle   :: String
    , pageHtml    :: String
    , pageLinks   :: [String]
    } deriving Show

type PageMap = Map String Page
