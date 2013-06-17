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

import qualified Text.Pandoc                          as P
import qualified Web.Scotty                           as S
import qualified Network.Wai.Middleware.Static        as WS
import qualified Network.Wai.Middleware.RequestLogger as WL
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as HA

type Blog = ReaderT Config IO

type PostParser = P.ReaderOptions -> String -> Pandoc

data Config = Config { configPort       :: Int
                     , configRoot       :: FilePath
                     , configCss        :: FilePath
                     , configPostExt    :: String
                     , configPostParser :: PostParser
                     }

-- XXX: Configure Warp: S.scottyOpts {verbose = 0} and Warp options
-- XXX: Read from cmd line params
-- XXX: Show more info about a post, date. Add home link in each post, contact email, author, about (about.md), github link
-- XXX: Footer: by Ricardo Catali.. email
-- XXX CSS: monospaced text with left solid line
defaultConfig :: Config
defaultConfig = Config 8000 "." "/css/style.css" ".md" P.readMarkdown

main :: IO ()
main = do
    S.scotty (configPort conf) $ do
        S.middleware WL.logStdout
        S.middleware $ WS.staticPolicy (WS.noDots WS.>-> WS.addBase staticPath)
        S.get "/blog" $ do
            index <- dispatch renderIndex
            render index
        S.get (S.regex "^/post/(.+)$") $ do
            path <- S.param "1"
            post <- dispatch $ renderPost (path ++ configPostExt conf)
            render post
        S.notFound $ do
            error404 <- dispatch renderError404
            render error404
    where dispatch   = liftIO . liftM eitherToMaybe . tryJust (guard . check) . runBlog
          check e    = isDoesNotExistError e || isPermissionError e
          runBlog    = flip runReaderT conf
          render     = maybe S.next (S.html . renderHtml)
          conf       = defaultConfig
          staticPath = configRoot conf </> "static"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

renderError404 :: Blog H.Html
renderError404 = do
    cssPath <- reader configCss
    return $ H.docTypeHtml $ do
        renderHead cssPath msg
        H.body $ do
            H.h1 $ H.toHtml msg
    where msg = "404: Nothing"

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

footer :: H.Html
footer = H.div H.! HA.class_ "footer" $ author
    where author = "Ricardo Catalinas Jiménez"

renderIndex :: Blog H.Html
renderIndex = do
    cssPath    <- reader configCss
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
            footer
    where title = "rlog"

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
            footer

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
