{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Control.Exception
import System.IO.Error

import Data.Default                                   (def)
import Data.Text.Lazy                                 (Text, pack)
import System.FilePath                                ((</>), takeFileName, takeExtension, dropExtension)
import System.Directory                               (getDirectoryContents, doesDirectoryExist)
import Text.Pandoc.Definition                         (Pandoc(..), Meta(..))
import Text.Pandoc.Shared                             (stringify)
import Text.Blaze.Html.Renderer.Text                  (renderHtml)

import qualified Data.Set                             as S
import qualified Text.Pandoc                          as P
import qualified Web.Scotty                           as SC
import qualified Network.Wai.Middleware.Static        as WS
import qualified Network.Wai.Middleware.RequestLogger as WL
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as HA

type Blog = ReaderT Config IO

type PostInfo = (Text, Maybe Text, Maybe Text)

data Config = Config { configPort    :: Int
                     , configRoot    :: FilePath
                     , configFormat  :: String
                     , configTitle   :: Text
                     , configName    :: Text
                     , configEmail   :: String
                     , configGitHub  :: String
                     , configTwitter :: String
                     }

defaultConfig :: Config
defaultConfig = Config { configPort    = 2000
                       , configRoot    = "."
                       , configFormat  = "markdown"
                       , configTitle   = "rlog"
                       , configName    = "Ricardo Catalinas Jiménez"
                       , configEmail   = "jimenezrick@gmail.com"
                       , configGitHub  = "https://github.com/jimenezrick"
                       , configTwitter = "https://twitter.com/_jimenezrick"
                       }

formatError :: a
formatError = error "Error: post format not supported"

postExtension :: String -> String
postExtension "markdown" = ".md"
postExtension "rst"      = ".rst"
postExtension "textile"  = ".textile"
postExtension _          = formatError

main :: IO ()
main = do
    SC.scotty port $ do
        logging
        favicon
        staticDispatch "static/" root
        staticDispatch "posts/" root
        SC.get "/" $ do
            home <- dispatch $ renderPost ("../home" ++ ext)
            render home
        SC.get "/blog" $ do
            index <- dispatch renderIndex
            render index
        SC.get (SC.regex "/post/(.+)") $ do
            path <- SC.param "1"
            post <- dispatch $ renderPost (path ++ ext)
            render post
        SC.get "/about" $ do
            about <- dispatch $ renderPost ("../about" ++ ext)
            render about
        SC.notFound $ do
            error404 <- dispatch renderError404
            render error404
    where dispatch = liftIO . liftM eitherToMaybe . tryJust (guard . check) . runBlog
          check e  = isDoesNotExistError e || isPermissionError e
          runBlog  = flip runReaderT conf
          render   = maybe SC.next (SC.html . renderHtml)
          ext      = postExtension (configFormat conf)
          conf     = defaultConfig
          root     = configRoot conf
          port     = configPort conf

logging :: SC.ScottyM ()
logging = SC.middleware WL.logStdout

favicon :: SC.ScottyM ()
favicon = SC.middleware $ WS.staticPolicy $ WS.predicate (== "favicon.ico")

staticDispatch :: String -> String -> SC.ScottyM ()
staticDispatch prefix root =
    SC.middleware
    $ WS.staticPolicy
    $ WS.noDots WS.>-> WS.hasPrefix prefix WS.>-> WS.addBase root

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

renderError404 :: Blog H.Html
renderError404 =
    return $ H.docTypeHtml $ do
        head_ msg
        H.body $
            H.div H.! HA.id "error404" $
                H.h1 $ H.toHtml msg
    where msg = "404: Nothing"

head_ :: Text -> H.Html
head_ title =
    H.head $ do
        utf8Charset
        css
        H.title $ H.toHtml title

utf8Charset :: H.Html
utf8Charset = H.meta H.! HA.charset "utf-8"

css :: H.Html
css = H.link
      H.! HA.rel "stylesheet"
      H.! HA.type_ "text/css"
      H.! HA.href path
          where path = "/static/css/style.css"

space :: H.Html
space = " "

header :: Text -> H.Html
header title = H.div H.! HA.id "header" $ do
    H.h1 (H.toHtml title)
    link "/" "home/"
    space
    link "/blog" "blog/"
    space
    link "/about" "about/"

info :: Maybe Text -> Maybe Text -> H.Html
info date author = H.div H.! HA.id "info" $ do
    maybe mempty (\d -> H.toHtml ("Published on " `mappend` d) >> H.br) date
    maybe mempty (\a -> H.toHtml ("by " `mappend` a)) author

footer :: Text -> String -> String -> String -> H.Html
footer name email github twitter = H.div H.! HA.id "footer" $ do
    link ("mailto:" ++ email) (H.toHtml name)
    link github githubLogo
    link twitter twitterLogo
        where githubLogo  = H.img H.! HA.src "/static/img/github.png"
              twitterLogo = H.img H.! HA.src "/static/img/twitter.png"

renderIndex :: Blog H.Html
renderIndex = do
    title      <- reader configTitle
    name       <- reader configName
    email      <- reader configEmail
    github     <- reader configGitHub
    twitter    <- reader configTwitter
    postPaths  <- liftM sort searchPosts
    postTitles <- mapM (renderPostInfo (\(t, _, _) -> H.toHtml t)) postPaths
    postDates  <- mapM (renderPostInfo (\(_, _, d) -> dateToHtml d)) postPaths
    return $ H.docTypeHtml $ do
        head_ title
        H.body $
            H.div H.! HA.id "index" $ do
                header title
                H.ul $
                    mapM_ (\(post, date) -> H.li (uncurry postLink post >> space >> date))
                          (zip (zip postPaths postTitles) postDates)
                footer name email github twitter
    where dateToHtml = maybe mempty H.toHtml

link :: FilePath -> H.Html -> H.Html
link u = H.a H.! HA.href (H.toValue u)

postLink :: FilePath -> H.Html -> H.Html
postLink = link . ("post" </>) . dropExtension

renderPostInfo :: (PostInfo -> H.Html) -> FilePath -> Blog H.Html
renderPostInfo f path = do
    text <- readPost path
    post <- parsePost text
    return $ f (postInfo path post)

postInfo :: FilePath -> Pandoc -> PostInfo
postInfo path (Pandoc (Meta t as d) _) = (t', a, f d)
    where f [] = Nothing
          f x  = Just (pack $ stringify x)
          a    = case as of
                   [] -> Nothing
                   _  -> f (head as)
          t'   = fromMaybe (pack $ titleFromFilename path) (f t)

titleFromFilename :: FilePath -> String
titleFromFilename file = foldr f [] $ (cap . dropExtension . takeFileName) file
    where f '-' cs   = ' ':cs
          f '_' cs   = ' ':cs
          f c   cs   = c:cs
          cap (c:cs) = toUpper c:cs
          cap []     = []

renderPost :: FilePath -> Blog H.Html
renderPost path = do
    name    <- reader configName
    email   <- reader configEmail
    github  <- reader configGitHub
    twitter <- reader configTwitter
    text    <- readPost path
    post    <- parsePost text
    let content               = toHtml post
        (title, author, date) = postInfo path post
    return $ H.docTypeHtml $ do
        head_ title
        H.body $
            H.div H.! HA.id "post" $ do
                header title
                info date author
                content
                footer name email github twitter

readPost :: FilePath -> Blog String
readPost path = do
    root <- reader configRoot
    liftIO $ readFile $ root </> "posts" </> path

parsePost :: String -> Blog Pandoc
parsePost text = do
    format <- reader configFormat
    maybe formatError (\p -> liftIO $ p opts text) (lookup format P.readers)
        where exts = S.delete P.Ext_implicit_figures (P.readerExtensions def)
              opts = def {P.readerExtensions = exts}

toHtml :: Pandoc -> H.Html
toHtml = P.writeHtml def {P.writerHtml5 = True}

searchPosts :: Blog [FilePath]
searchPosts = do
    root   <- reader configRoot
    format <- reader configFormat
    let ext = postExtension format
    liftIO $ search ext (root </> "posts") "."
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
