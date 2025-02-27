{-# LANGUAGE OverloadedStrings #-}
import Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    match "static/*" $ do
        route $ gsubRoute "static/" $ const "" -- put static/* files in the root
        compile copyFileCompiler

    match "css/styles.css" $ do
        route $ constRoute "styles.css"
        compile compressCssCompiler

    match "images/*" $ do
        route $ gsubRoute "images/" $ const "" -- put images/* files in the root
        compile copyFileCompiler

----------------------------------------------
{-
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx = listField "posts" postCtx (return posts) <> constField "title" "Archives" <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "index.html" $ do
        route $ constRoute "sample-index.html"
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx = listField "posts" postCtx (return posts) <> defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler
-}

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext
