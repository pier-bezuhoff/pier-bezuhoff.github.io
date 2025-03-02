{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.List (isPrefixOf)
import Data.Char (toUpper)
import System.FilePath (takeBaseName)
import Hakyll

-- TODO: automatically insert correct last modified date into sitemap and index/strucutred data (json ld)
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
    create ["my-icon-gallery.html"] $ do
        route idRoute
        compile $ do
            icons <- loadAll "icons/*.svg" -- :: Compiler [Item String] 
            let pageTitle = "My icon gallery"
            let context = listField "icons" iconContext (pure icons) <> constField "title" pageTitle <> defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/my-icon-gallery.html" context
                >>= relativizeUrls
    -- cannot copy .nojekyll and .gitignore

iconContext :: Context String
iconContext = svgField <> labelField <> defaultContext where
    svgField = field "svg" $ pure . cleanSvg . itemBody
    cleanSvg svg = if "<?xml" `isPrefixOf` svg
        then drop 1 $ snd $ break (== '\n') svg -- drop first line with <?xml...?>
        else svg
    labelField = field "label" $ pure . name2label . takeBaseName . toFilePath . itemIdentifier
    name2label "" = ""
    name2label (c0:cs) = [toUpper c0] <> map (\c -> if c == '-' then ' ' else c) cs
