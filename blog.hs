{-# LANGUAGE OverloadedStrings #-}

-- XXX: Test for space leaks with curl

import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import System.IO.Error

import Data.Text              (Text, pack)
import System.FilePath        ((</>), takeExtension)
import System.Directory       (getDirectoryContents, doesDirectoryExist)
import Text.Pandoc.Definition (Pandoc(..), Meta(..), Inline(..))

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
    posts <- liftM (map H.toHtml . sort) searchPosts -- XXX: renderPostName
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



-- XXX
--renderPostName :: FilePath -> H.Html
-- XXX




-- XXX": Extraer el resto de la info
postTitle :: Pandoc -> Text
postTitle (Pandoc (Meta t _ _ ) _) = extractTitle t
    where extractTitle [Str s] = pack s -- XXX: Mejorar, que sucede si no hay title?
          extractTitle _       = error "postTitle: non-textual title" -- XXX: Use filename replacing - and _ by spaces?







renderPost :: FilePath -> Blog H.Html
renderPost path = do
    root <- reader configRoot
    text <- liftIO $ readFile $ root </> path
    -- TODO: try readMarkdownWithWarnings
    return $ P.writeHtml P.def $ P.readMarkdown P.def text

isMarkdown :: FilePath -> Bool
isMarkdown file = elem (takeExtension file) [".md", ".mdown", ".markdown"]

searchPosts :: Blog [FilePath]
searchPosts = do
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
