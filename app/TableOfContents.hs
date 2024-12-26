{-# LANGUAGE OverloadedStrings #-}

module TableOfContents 
    ( generateTOC
    , PageAnalytics(..)
    ) where

import qualified Data.Map as M
import Data.Map (Map)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Types (Page(..))

data PageAnalytics = PageAnalytics 
    { incomingLinks :: Int
    , outgoingLinks :: Int
    , totalLinks    :: Int
    } deriving Show

analyzePages :: Map String [String] -> [Page] -> Map String PageAnalytics
analyzePages backlinks pages = M.fromList
    [ (pageTitle page, analyzeOnePage page)
    | page <- pages
    ]
  where
    analyzeOnePage page = 
        let incoming = length $ M.findWithDefault [] (pageTitle page) backlinks
            outgoing = length $ pageLinks page
        in PageAnalytics incoming outgoing (incoming + outgoing)

generateTOC :: [Page] -> Map String [String] -> String
generateTOC pages backlinks = unlines
    [ "# Site Map"
    , ""
    , "## Starting Points"
    , "Pages that link to many other topics:"
    , unlines startingPointsList
    , ""
    , "## Most Connected"
    , "Pages with the most total connections:"
    , unlines mostConnectedList
    , ""
    , "## Least Connected"
    , "Pages that could use more connections:"
    , unlines leastConnectedList
    ]
  where
    analytics = analyzePages backlinks pages

    formatLink title count suffix = 
        "- [" ++ title ++ "](" ++ title ++ ".html) (" ++ show count ++ suffix ++ ")"

    startingPointsList =
        [ formatLink title count " outgoing links"
        | (title, count) <- take 3 $ sortOn (Down . snd)
            [(title, outgoingLinks ana) | (title, ana) <- M.toList analytics, outgoingLinks ana > 0]
        ]

    mostConnectedList =
        [ formatLink title count " total links"
        | (title, count) <- take 5 $ sortOn (Down . snd)
            [(title, totalLinks ana) | (title, ana) <- M.toList analytics]
        ]

    leastConnectedList =
        [ formatLink title count " total links"
        | (title, count) <- take 5 $ sortOn snd
            [(title, totalLinks ana) | (title, ana) <- M.toList analytics]
        ]
