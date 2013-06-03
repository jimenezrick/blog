{-# LANGUAGE OverloadedStrings #-}

-- XXX: Test for space leaks with curl
-- FIXME: Scotty returns 404 without <html>! Fix?

import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import System.IO.Error

import Data.Text.Lazy                (Text)
import System.FilePath               ((</>), takeFileName, takeExtension, dropExtension)
import System.Directory              (getDirectoryContents, doesDirectoryExist)
import Text.Pandoc.Definition        (Pandoc(..), Meta(..))
import Text.Pandoc.Shared            (stringify)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Text.Pandoc                 as P
import qualified Web.Scotty                  as S
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA

data Config = Config { configPort  :: Int
                     , configRoot  :: FilePath
                     , configTitle :: Text
                     } deriving (Show, Read)

-- XXX: Read from cmd line params
defaultConfig :: Config
defaultConfig = Config 8000 "posts" "rlog"

type Blog = ReaderT Config IO

main :: IO ()
main = do
    -- XXX: Handle exceptions from here
    S.scotty (configPort defaultConfig) $ do
        S.get "/blog" $ do
            index <- liftIO $ dispatch renderIndex
            render index
        -- XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX
        -- XXX: Read regex in Scotty code
        S.get (S.regex "^/post/.*$") $ do
            -- XXX: Show error when unable to read post
            zero <- S.param "0"
            --one <- S.param "1"
            --two <- S.param "2"
            -- TODO: try S.params
            S.html $ zero
        -- XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX XXX
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
                mapM_ (H.li . uncurry postLink) (zip postPaths postTitles)
            H.hr

link :: FilePath -> H.Html -> H.Html
link u t = H.a H.! HA.href (H.toValue u) $ t

postLink :: FilePath -> H.Html -> H.Html
postLink = link . ("post" </>) . dropExtension

renderPostName :: FilePath -> Blog H.Html
renderPostName path = do
    text <- readPost path
    let post  = fromMarkdown text
        title = maybe (titleFromFilename path) id (postTitle post)
    return $ H.toHtml title

postTitle :: Pandoc -> Maybe String
postTitle (Pandoc (Meta t _ _ ) _) = case stringify t of
                                       [] -> Nothing
                                       s  -> Just s

titleFromFilename :: FilePath -> String
titleFromFilename file = foldr f [] $ (dropExtension . takeFileName) file
    where f '-' cs = ' ':cs
          f '_' cs = ' ':cs
          f c   cs = c:cs

renderPost :: FilePath -> Blog H.Html
renderPost path = do
    -- TODO: try readMarkdownWithWarnings
    text <- readPost path
    return $ toHtml $ fromMarkdown text

readPost :: FilePath -> Blog String
readPost path = do
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
    root <- reader configRoot
    liftIO $ search root "."
        where list dir = liftM (filter (`notElem` [".", ".."]))
                         $ getDirectoryContents dir
              search root parent = do
                  files <- list (root </> parent)
                  paths <- forM files $ \file -> do
                      let path = parent </> file
                      isDir <- doesDirectoryExist (root </> path)
                      if isDir
                        then search root path
                        else return $ do guard (isMarkdown file)
                                         [path]
                  return (concat paths)
