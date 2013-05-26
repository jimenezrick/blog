{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Monad.Reader

import qualified Data.Text.Lazy                as T
import qualified System.Directory              as D

import qualified Web.Scotty                    as S
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html.Renderer.Text as R

data Config = Config { port  :: Int
                     , root  :: FilePath
                     , title :: T.Text
                     } deriving (Show, Read)

defaultConfig :: Config
defaultConfig = Config 8000 "." "My Blog"

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
    posts <- liftM (map H.toHtml) listPosts
    return $ H.docTypeHtml $ do
            H.head $ do
                H.title $ H.toHtml title
            H.body $ do
                H.h1 "rlog"
                H.ul $
                    mapM_ H.li posts




type Blog = ReaderT Config IO

listPosts :: Blog [FilePath]
listPosts = postsRoot >>= liftIO . D.getDirectoryContents

postsRoot :: Blog FilePath
postsRoot = liftM root ask

httpPort :: Blog Int
httpPort = liftM port ask

blogTitle :: Blog T.Text
blogTitle = liftM title ask
