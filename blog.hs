{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import System.IO.Error

import Data.Text.Lazy                (Text, pack)
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

data Config = Config { configPort       :: Int
                     , configRoot       :: FilePath
                     , configCss        :: FilePath
                     , configTitle      :: Text -- XXX: Configure title/intro in another place?
                     , configPostExt    :: String
                     , configPostParser :: PostParser
                     }

-- XXX: Configure Warp
-- XXX: Add wai-static handler for serving statics
-- XXX: Read from cmd line params
-- XXX: Show more info about a post, date? Add home link in each post
-- XXX: look in my web/r-log hakyll previous experiment
defaultConfig :: Config
defaultConfig = Config 8000 "posts" "rlog" "/static/style.css" ".md" P.readMarkdown

type Blog = ReaderT Config IO

type PostParser = P.ReaderOptions -> String -> Pandoc

main :: IO ()
main = do
    -- XXX: Handle exceptions from here
    S.scotty (configPort conf) $ do
        S.get "/blog" $ do
            index <- dispatch renderIndex
            render index
        S.get (S.regex "^/post/(.+)$") $ do
            path <- S.param "1"
            post <- dispatch $ renderPost (path ++ configPostExt conf)
            render post
    where dispatch = liftIO . flip runReaderT conf
          render   = S.html . renderHtml
          conf     = defaultConfig

renderHead :: FilePath -> Text -> H.Html
renderHead path title =
    H.head $ do
        utf8Charset
        css path
        H.title $ H.toHtml title

utf8Charset :: H.Html
utf8Charset = H.meta H.! HA.charset "utf-8"

css :: FilePath -> H.Html
css path = H.link
           H.! HA.rel "stylesheet"
           H.! HA.type_ "text/css"
           H.! HA.href (H.toValue path)

renderIndex :: Blog H.Html
renderIndex = do
    cssPath    <- reader configCss
    title      <- reader configTitle
    postPaths  <- liftM sort searchPosts
    postTitles <- mapM renderPostName postPaths
    return $ H.docTypeHtml $ do
        renderHead cssPath title
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
    post <- parsePost text
    let title = postTitle path post
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
    cssPath <- reader configCss
    text    <- readPost path
    post    <- parsePost text
    let content = toHtml post
        title   = pack $ postTitle path post
    return $ H.docTypeHtml $ do
        renderHead cssPath title
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

parsePost :: String -> Blog Pandoc
parsePost text = do
    parser <- reader configPostParser
    return (parser P.def text)

toHtml :: Pandoc -> H.Html
toHtml = P.writeHtml P.def {writerHtml5 = True}

searchPosts :: Blog [FilePath]
searchPosts = do
    root <- reader configRoot
    ext  <- reader configPostExt
    liftIO $ search ext root "."
        where search ext root parent = do
                  files <- listDir (root </> parent)
                  paths <- forM files $ \file -> do
                      let path = parent </> file
                      isDir <- doesDirectoryExist (root </> path)
                      if isDir
                        then search ext root path
                        else return $ do guard (takeExtension file == ext)
                                         [path]
                  return (concat paths)

listDir :: FilePath -> IO [FilePath]
listDir dir = liftM (filter (`notElem` [".", ".."])) $ getDirectoryContents dir
