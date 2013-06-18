{-# LANGUAGE OverloadedStrings #-}

import Data.Char
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
import Text.Blaze.Html.Renderer.Text (renderHtml)

import qualified Data.Set                             as S
import qualified Text.Pandoc                          as P
import qualified Web.Scotty                           as SC
import qualified Network.Wai.Middleware.Static        as WS
import qualified Network.Wai.Middleware.RequestLogger as WL
import qualified Text.Blaze.Html5                     as H
import qualified Text.Blaze.Html5.Attributes          as HA

type Blog = ReaderT Config IO

data Config = Config { configPort   :: Int
                     , configRoot   :: FilePath
                     , configFormat :: String
                     }

-- XXX: Configure Warp: SC.scottyOpts {verbose = 0} and Warp options
-- XXX: Read from cmd line params
-- XXX: Show post dates
defaultConfig :: Config
defaultConfig = Config 8000 "." "markdown"

formatError :: a
formatError = error "Error: post format not supported"

postExtension :: String -> String
postExtension "markdown" = ".md"
postExtension "rst"      = ".rst"
postExtension "textile"  = ".textile"
postExtension _          = formatError

main :: IO ()
main = do
    SC.scotty (configPort conf) $ do
        SC.middleware WL.logStdout
        SC.middleware $ WS.staticPolicy $ WS.noDots WS.>-> WS.addBase staticPath
        SC.middleware $ WS.staticPolicy $ WS.noDots WS.>-> WS.hasPrefix "posts/" WS.>-> WS.addBase root
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
    where dispatch   = liftIO . liftM eitherToMaybe . tryJust (guard . check) . runBlog
          check e    = isDoesNotExistError e || isPermissionError e
          runBlog    = flip runReaderT conf
          render     = maybe SC.next (SC.html . renderHtml)
          conf       = defaultConfig
          root       = configRoot conf
          staticPath = root </> "static"
          ext        = postExtension (configFormat conf)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

renderError404 :: Blog H.Html
renderError404 = do
    return $ H.docTypeHtml $ do
        head_ msg
        H.body $ do
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
          where path = "/css/style.css"

space :: H.Html
space = " "

header :: Text -> H.Html
header title = do
    H.h1 $ H.toHtml title
    H.div H.! HA.id "header" $ do
        link "/blog" "home/"
        space
        link "/about" "about/"

footer :: H.Html
footer = H.div H.! HA.id "footer" $ do
    link ("mailto:" ++ email) name
    link github logo
        where name   = "Ricardo Catalinas Jiménez"
              email  = "jimenezrick@gmail.com"
              github = "https://github.com/jimenezrick"
              logo   = H.img H.! HA.src "/img/github.png"

renderIndex :: Blog H.Html
renderIndex = do
    postPaths  <- liftM sort searchPosts
    postTitles <- mapM renderPostName postPaths
    return $ H.docTypeHtml $ do
        head_ title
        H.body $ do
            header title
            H.ul $ do
                mapM_ (H.li . uncurry postLink) (zip postPaths postTitles)
            footer
    where title = "rlog"

link :: FilePath -> H.Html -> H.Html
link u = H.a H.! HA.href (H.toValue u)

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
titleFromFilename file = foldr f [] $ (cap . dropExtension . takeFileName) file
    where f '-' cs   = ' ':cs
          f '_' cs   = ' ':cs
          f c   cs   = c:cs
          cap (c:cs) = toUpper c:cs
          cap []     = []

renderPost :: FilePath -> Blog H.Html
renderPost path = do
    text    <- readPost path
    post    <- parsePost text
    let content = toHtml post
        title   = pack $ postTitle path post
    return $ H.docTypeHtml $ do
        head_ title
        H.body $ do
            header title
            content
            footer

readPost :: FilePath -> Blog String
readPost path = do
    root <- reader configRoot
    text <- liftIO $ readFile $ root </> "posts" </> path
    return text

parsePost :: String -> Blog Pandoc
parsePost text = do
    format <- reader configFormat
    maybe formatError (\p -> liftIO $ p P.def text) (lookup format P.readers)

toHtml :: Pandoc -> H.Html
toHtml = P.writeHtml P.def {P.writerHtml5 = True, P.writerExtensions = exts}
    where exts = S.delete P.Ext_implicit_figures (P.writerExtensions P.def)

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
