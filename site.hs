{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Char (toUpper)
import System.FilePath (takeBaseName)
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
    match "css/*.css" $ do
        route idRoute
        compile compressCssCompiler
    match "images/*" $ do
        route $ gsubRoute "images/" $ const "" -- put images/* files in the root
        compile copyFileCompiler
    match "icons/*" $ do
        route idRoute
        compile $ getResourceBody
    match "templates/*" $
        compile templateCompiler
    create ["my-icons-gallery.html"] $ do
        route idRoute
        compile $ do
            icons <- loadAll "icons/*.svg" -- :: Compiler [Item String] 
            let context = listField "icons" iconContext (return icons) <> constField "title" "My icons gallery" <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/my-icon-gallery.html" context
                >>= relativizeUrls

iconContext :: Context String
iconContext = svgField <> labelField <> defaultContext where
    -- urlField = field "url" $ pure . toUrl . toFilePath . itemIdentifier
    svgField = bodyField "svg"
    labelField = field "label" $ pure . name2label . takeBaseName . toFilePath . itemIdentifier
    name2label "" = ""
    name2label (c0:cs) = [toUpper c0] <> map (\c -> if c == '-' then ' ' else c) cs
