{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Reader

import Data.Text        (Text)
import System.FilePath  ((</>), takeExtension)
import System.Directory (getDirectoryContents, doesDirectoryExist)

import qualified Text.Pandoc                   as P
import qualified Web.Scotty                    as S
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html.Renderer.Text as R

data Config = Config { configPort  :: Int
                     , configRoot  :: FilePath
                     , configTitle :: Text
                     } deriving (Show, Read)

-- XXX: Read from cmd line params
defaultConfig :: Config
defaultConfig = Config 8000 "/tmp" "My Blog"

type Blog = ReaderT Config IO

main :: IO ()
main = do
    S.scotty (configPort defaultConfig) $ do
        S.get "/blog" $ do
            index <- liftIO $ dispatch renderIndex
            render index
    where dispatch = flip runReaderT $ defaultConfig
          render   = S.html . R.renderHtml


renderIndex :: Blog H.Html
renderIndex = do
    title <- reader configTitle
    posts <- liftM (map H.toHtml) searchPosts
    post <- renderPost "post.md" -- XXX
    return $ H.docTypeHtml $ do
        H.head $ do
            H.title $ H.toHtml title
        H.body $ do
            H.h1 "Blog"
            H.ul $ do
                mapM_ H.li posts
            H.hr -- XXX
            post














renderPost :: FilePath -> Blog H.Html
renderPost path = do
    -- XXX: Protect for non-readable files
    root <- reader configRoot
    text <- liftIO $ readFile $ root </> path
    -- TODO: try readMarkdownWithWarnings
    return $ P.writeHtml P.def $ P.readMarkdown P.def text
    -- TODO: try: getReader :: String -> Either String (ReaderOptions -> String -> IO Pandoc)
    -- Retrieve reader based on formatSpec (format+extensions).
    --
    -- readers :: [(String, ReaderOptions -> String -> IO Pandoc)]
    -- (in Text.Pandoc)










isMarkdown :: FilePath -> Bool
isMarkdown file = elem (takeExtension file) [".md", ".mdown", ".markdown"]

searchPosts :: Blog [FilePath]
searchPosts = do
    -- XXX: Protect for non-readable files
    parent <- reader configRoot
    files' <- liftIO $ getDirectoryContents parent
    let files = filter (`notElem` [".", ".."]) files'
    paths <- forM files $ \file -> do
        let path = parent </> file
        isDir <- liftIO $ doesDirectoryExist path
        if isDir
          then local (\c -> c {configRoot = path}) searchPosts
          else return $ do guard (isMarkdown file)
                           [file]
    return (concat paths)
