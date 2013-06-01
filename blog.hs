{-# LANGUAGE OverloadedStrings #-}

-- XXX: Test for space leaks with curl

import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import System.IO.Error

import Data.Text                     (Text, pack)
import System.FilePath               ((</>), takeExtension)
import System.Directory              (getDirectoryContents, doesDirectoryExist)
import Text.Pandoc.Definition        (Pandoc(..), Meta(..), Inline(..))
import Text.Pandoc.Shared            (stringify)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Text.Pandoc      as P
import qualified Web.Scotty       as S
import qualified Text.Blaze.Html5 as H

data Config = Config { configPort  :: Int
                     , configRoot  :: FilePath
                     , configTitle :: Text
                     } deriving (Show, Read)

-- XXX: Read from cmd line params
defaultConfig :: Config
defaultConfig = Config 8000 "." "rlog"

type Blog = ReaderT Config IO

main :: IO ()
main = do
    S.scotty (configPort defaultConfig) $ do
        S.get "/blog" $ do
            index <- liftIO $ dispatch renderIndex
            render index
        where dispatch = flip runReaderT $ defaultConfig
              render   = S.html . renderHtml

renderIndex :: Blog H.Html
renderIndex = do
    title      <- reader configTitle
    postPaths  <- liftM sort searchPosts
    postTitles <- mapM renderPostName postPaths
    return $ H.docTypeHtml $ do
        H.head $ do
            H.title $ H.toHtml title
        H.body $ do
            H.h1 $ H.toHtml title
            H.hr
            H.ul $ do
                mapM_ H.li postTitles
            H.hr

renderPostName :: FilePath -> Blog H.Html
renderPostName path = do
    text <- readPost path
    return $ H.toHtml $ postTitle $ fromMarkdown text

postTitle :: Pandoc -> Text
postTitle (Pandoc (Meta t _ _ ) _) = pack $ stringify t

renderPost :: FilePath -> Blog H.Html
renderPost path = do
    -- TODO: try readMarkdownWithWarnings
    text <- readPost path
    return $ toHtml $ fromMarkdown text

readPost :: FilePath -> Blog String
readPost path = do
    -- XXX: Handle up error case like in searchPosts?
    root <- reader configRoot
    text <- liftIO $ readFile $ root </> path
    return text

fromMarkdown :: String -> Pandoc
fromMarkdown = P.readMarkdown P.def

toHtml :: Pandoc -> H.Html
toHtml = P.writeHtml P.def -- TODO: HTML5?

isMarkdown :: FilePath -> Bool
isMarkdown file = elem (takeExtension file) [".md", ".mdown", ".markdown"]

searchPosts :: Blog [FilePath]
searchPosts = do
    -- XXX: Move upper error handling
    parent <- reader configRoot
    files  <- liftIO $ listDir parent
    paths  <- forM files $ \file -> do
        let path = parent </> file
        isDir <- liftIO $ doesDirectoryExist path
        if isDir
          then local (\c -> c {configRoot = path}) searchPosts
          else return $ do guard (isMarkdown file)
                           [path]
    return (concat paths)
        where check e   = isDoesNotExistError e || isPermissionError e
              listDir d = do
                  r <- tryJust (guard . check) $ getDirectoryContents d
                  case r of
                    Left _   -> return []
                    Right fs -> return $ filter (`notElem` [".", ".."]) fs
