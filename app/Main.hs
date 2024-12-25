{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Commonmark as CM
import Control.Monad (forM_)
import Data.List (isSuffixOf)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory (createDirectoryIfMissing, doesFileExist, listDirectory, removeFile)
import System.FilePath (takeBaseName, takeExtension, (</>))
import TableOfContents (generateTOC)
import Types (Page (..))
import Data.Maybe (fromMaybe)

data SiteConfig = SiteConfig
  { customCss :: Maybe String,
    customHtml :: Maybe String
  }

findLinks :: String -> [String]
findLinks str = go str []
  where
    go "" acc = acc
    go s acc = case break (== '[') s of
      (_, "") -> acc
      (_, _ : rest) -> case break (== ']') rest of
        (_, "") -> acc
        (_, ']' : '(' : url) -> case break (== ')') url of
          (link, rest')
            | ".html" `isSuffixOf` link ->
                go rest' (takeBaseName link : acc)
          (_, rest') -> go rest' acc
        (_, rest') -> go rest' acc

markdownToHtml :: String -> String
markdownToHtml content = case CM.commonmark @(CM.Html ()) "doc" (T.pack content) of
  Left err -> "Error: " ++ show err
  Right html -> TL.unpack $ CM.renderHtml html

loadSiteConfig :: IO SiteConfig
loadSiteConfig = do
  cssExists <- doesFileExist "styles/custom.css"
  htmlExists <- doesFileExist "styles/template.html"
  css <-
    if cssExists
      then Just <$> readFile "styles/custom.css"
      else pure Nothing
  html <-
    if htmlExists
      then Just <$> readFile "styles/template.html"
      else pure Nothing
  pure $ SiteConfig css html

processFile :: FilePath -> IO Page
processFile path = do
  content <- readFile path
  pure $
    Page
      { pageTitle = takeBaseName path,
        pageHtml = markdownToHtml content,
        pageLinks = findLinks content
      }

getPreview :: Int -> String -> String
getPreview n content =
  let words' = take n . takeWhile (/= '\n') . dropWhile (== '#') $ content
   in words' ++ " ... "

generateHTML :: SiteConfig -> Map String [String] -> Page -> String
generateHTML config backlinks page =
  maybe defaultTemplate insertIntoTemplate (customHtml config)
  where
    defaultTemplate =
      unlines
        [ "<!DOCTYPE html>",
          "<html><head><title>" ++ pageTitle page ++ "</title>",
          "<style>",
          "body { font-family: monospace; max-width: 650px; margin: 40px auto; padding: 20px; }",
          "a { color: #333; }",
          ".backlinks { margin-top: 2em; border-top: 1px solid #eee; padding-top: 1em; }",
          fromMaybe "" (customCss config),
          "</style></head><body>",
          pageHtml page,
          generateBacklinks,
          "</body></html>"
        ]

    insertIntoTemplate template =
      replace "{{title}}" (pageTitle page) $
        replace "{{content}}" (pageHtml page ++ generateBacklinks) $
          replace "{{custom_css}}" (fromMaybe "" (customCss config)) template

    generateBacklinks =
      case M.lookup (pageTitle page) backlinks of
        Just links
          | not (null links) ->
              unlines
                [ "<div class='backlinks'><h2>See also:</h2>",
                  unlines [makeLink title | title <- links],
                  "</div>"
                ]
        _ -> ""

    makeLink title = "<a href='" ++ title ++ ".html'>" ++ title ++ "</a><br>"

replace :: String -> String -> String -> String
replace old new content = T.unpack $ T.replace (T.pack old) (T.pack new) (T.pack content)

pruneOrphanedFiles :: [String] -> IO ()
pruneOrphanedFiles validBasenames = do
  htmlFiles <- filter (\f -> takeExtension f == ".html") <$> listDirectory "site"
  let htmlBasenameSet = S.fromList $ map takeBaseName htmlFiles
      validBasenameSet = S.fromList validBasenames
      orphanedFiles = S.difference htmlBasenameSet validBasenameSet

  forM_ orphanedFiles $ \basename ->
    removeFile ("site" </> basename ++ ".html")

main :: IO ()
main = do
  createDirectoryIfMissing True "site"
  createDirectoryIfMissing True "styles"

  config <- loadSiteConfig

  files <- filter (\f -> takeExtension f == ".md") <$> listDirectory "content"
  pages <- mapM (processFile . ("content" </>)) files

  let backlinks =
        M.fromListWith
          (++)
          [(target, [pageTitle p]) | p <- pages, target <- pageLinks p]
      validBaseNames = map pageTitle pages

  mapM_
    ( \p ->
        writeFile
          ("site" </> pageTitle p ++ ".html")
          (generateHTML config backlinks p)
    )
    pages

  pruneOrphanedFiles validBaseNames

  let tocContent = generateTOC pages backlinks
      tocPage = Page "map" (markdownToHtml tocContent) []
  writeFile
    ("site" </> "map.html")
    (generateHTML config backlinks tocPage)
