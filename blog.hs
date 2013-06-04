{-# LANGUAGE OverloadedStrings #-}

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
import Text.Pandoc.Options           (writerHtml5)
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Text.Pandoc                 as P
import qualified Web.Scotty                  as S
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as HA

data Config = Config { configPort  :: Int
                     , configRoot  :: FilePath
                     , configTitle :: Text -- XXX: Remove?
                     } deriving (Show, Read)

-- XXX: Configure Warp
-- XXX: Add wai-static handler for serving statics
-- XXX: Read from cmd line params
-- XXX: Show more info about a post, date? Add home link in each post
-- XXX: look in my web/r-log hakyll previous experiment
defaultConfig :: Config
defaultConfig = Config 8000 "posts" "rlog"

type Blog = ReaderT Config IO

main :: IO ()
main = do
    -- XXX: Handle exceptions from here
    S.scotty (configPort defaultConfig) $ do
        S.get "/blog" $ do
            index <- dispatch renderIndex
            render index
        S.get (S.regex "^/post/(.+)$") $ do
            path <- S.param "1"
            post <- dispatch $ renderPost (path ++ ".md")
            render post
    where dispatch = liftIO . flip runReaderT defaultConfig
          render   = S.html . renderHtml

renderIndex :: Blog H.Html
renderIndex = do
    title      <- reader configTitle
    postPaths  <- liftM sort searchPosts
    postTitles <- mapM renderPostName postPaths
    return $ H.docTypeHtml $ do
        H.head $ do
            utf8Charset
            H.title $ H.toHtml title
        H.body $ do
            H.h1 $ H.toHtml title
            H.hr
            H.ul $ do
                mapM_ (H.li . uncurry postLink) (zip postPaths postTitles)
            H.hr

utf8Charset :: H.Html
utf8Charset = H.meta H.! HA.charset "utf-8"

link :: FilePath -> H.Html -> H.Html
link u t = H.a H.! HA.href (H.toValue u) $ t

postLink :: FilePath -> H.Html -> H.Html
postLink = link . ("post" </>) . dropExtension

renderPostName :: FilePath -> Blog H.Html
renderPostName path = do
    text <- readPost path
    let post  = fromMarkdown text
        title = postTitle path post
    return $ H.toHtml title

postTitle :: FilePath -> Pandoc -> String
postTitle path post = maybe (titleFromFilename path) id (postTitle' post)

postTitle' :: Pandoc -> Maybe String
postTitle' (Pandoc (Meta t _ _ ) _) = case stringify t of
                                        [] -> Nothing
                                        s  -> Just s

titleFromFilename :: FilePath -> String
titleFromFilename file = foldr f [] $ (dropExtension . takeFileName) file
    where f '-' cs = ' ':cs
          f '_' cs = ' ':cs
          f c   cs = c:cs

renderPost :: FilePath -> Blog H.Html
renderPost path = do
    text <- readPost path
    let post    = fromMarkdown text
        content = toHtml post
        title   = postTitle path post
    return $ H.docTypeHtml $ do
        H.head $ do
            utf8Charset
            H.title $ H.toHtml title
        H.body $ do
            H.h1 $ H.toHtml title
            H.hr
            content
            H.hr

readPost :: FilePath -> Blog String
readPost path = do
    root <- reader configRoot
    text <- liftIO $ readFile $ root </> path
    return text

fromMarkdown :: String -> Pandoc
fromMarkdown = P.readMarkdown P.def

toHtml :: Pandoc -> H.Html
toHtml = P.writeHtml P.def {writerHtml5 = True}

isMarkdown :: FilePath -> Bool
isMarkdown = (".md" ==) . takeExtension

searchPosts :: Blog [FilePath]
searchPosts = do
    root <- reader configRoot
    liftIO $ search root "."
        where search root parent = do
                  files <- listDir (root </> parent)
                  paths <- forM files $ \file -> do
                      let path = parent </> file
                      isDir <- doesDirectoryExist (root </> path)
                      if isDir
                        then search root path
                        else return $ do guard (isMarkdown file)
                                         [path]
                  return (concat paths)

listDir :: FilePath -> IO [FilePath]
listDir dir = liftM (filter (`notElem` [".", ".."])) $ getDirectoryContents dir
