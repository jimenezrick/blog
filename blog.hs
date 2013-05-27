{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Reader
import Data.Text        (Text)
import System.FilePath  ((</>))
import System.Directory (getDirectoryContents)

import qualified Text.Pandoc                   as P
import qualified Web.Scotty                    as S
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html.Renderer.Text as R

data Config = Config { port  :: Int
                     , root  :: FilePath
                     , title :: Text
                     } deriving (Show, Read)

defaultConfig :: Config
defaultConfig = Config 8000 "/tmp" "My Blog"

main :: IO ()
main = do
    S.scotty (port defaultConfig) $ do
        S.get "/blog" $ do
            index <- liftIO $ dispatch renderIndex
            render index
    where dispatch = flip runReaderT $ defaultConfig
          render   = S.html . R.renderHtml


renderIndex :: Blog H.Html
renderIndex = do
    title <- blogTitle
    index <- liftM (map H.toHtml) listPosts
    post <- renderPost "post.md" -- XXX
    return $ H.docTypeHtml $ do
        H.head $ do
            H.title $ H.toHtml title
        H.body $ do
            H.h1 "Blog"
            H.ul $ do
                mapM_ H.li index
            H.hr -- XXX
            post





type Blog = ReaderT Config IO

listPosts :: Blog [FilePath]
listPosts = postsRoot >>= liftIO . getDirectoryContents

postsRoot :: Blog FilePath
postsRoot = liftM root ask

httpPort :: Blog Int
httpPort = liftM port ask

blogTitle :: Blog Text
blogTitle = liftM title ask






renderPost :: FilePath -> Blog H.Html
renderPost path = do
    root <- postsRoot
    text <- liftIO $ readFile $ root </> path
    -- TODO: try readMarkdownWithWarnings
    return $ P.writeHtml P.def $ P.readMarkdown P.def text
