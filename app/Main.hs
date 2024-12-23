{-# LANGUAGE OverloadedStrings, TypeApplications #-}

module Main where

import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath (takeBaseName, takeExtension, (</>))
import qualified Data.Map as M
import qualified Commonmark as CM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.List (isSuffixOf)

data Page = Page
    { pageTitle :: String
    , pageHtml :: String
    , pageLinks :: [String]
    } deriving Show

findLinks :: String -> [String]
findLinks str = go str []
  where
    go "" acc = acc
    go s acc = case break (=='[') s of
        (_, "") -> acc
        (_, _:rest) -> case break (==']') rest of
            (_, "") -> acc
            (_, ']':'(':url) -> case break (==')') url of
                (link, rest') | ".html" `isSuffixOf` link -> 
                    go rest' (takeBaseName link : acc)
                (_, rest') -> go rest' acc
            (_, rest') -> go rest' acc

markdownToHtml :: String -> String
markdownToHtml content = case CM.commonmark @(CM.Html ()) "doc" (T.pack content) of
    Left err -> "Error: " ++ show err
    Right html -> TL.unpack $ CM.renderHtml html

processFile :: FilePath -> IO Page
processFile path = do
    content <- readFile path
    pure $ Page (takeBaseName path) (markdownToHtml content) (findLinks content)

generateHTML :: M.Map String [String] -> Page -> String
generateHTML backlinks page = unlines
    [ "<!DOCTYPE html>"
    , "<html><head><title>" ++ pageTitle page ++ "</title><style>"
    , "body { font-family: monospace; max-width: 650px; margin: 40px auto; padding: 20px; }"
    , "a { color: #333; }"
    , ".backlinks { margin-top: 2em; border-top: 1px solid #eee; padding-top: 1em; }"
    , "</style></head><body>"
    , pageHtml page
    , "<div class='backlinks'><h2>See also:</h2>"
    , unlines [makeLink title | title <- M.findWithDefault [] (pageTitle page) backlinks]
    , "</div></body></html>"
    ]
  where
    makeLink title = "<a href='" ++ title ++ ".html'>" ++ title ++ "</a><br>"

main :: IO ()
main = do
    createDirectoryIfMissing True "site"
    files <- filter (\f -> takeExtension f == ".md") <$> listDirectory "content"
    pages <- mapM (processFile . ("content" </>)) files
    
    let pageMap = [(pageTitle p, p) | p <- pages]
        backlinks = M.fromListWith (++) 
            [(target, [pageTitle p]) | p <- pages, target <- pageLinks p]
    
    mapM_ (\p -> writeFile ("site" </> pageTitle p ++ ".html") 
                          (generateHTML backlinks p))
          pages
