{-# LANGUAGE OverloadedStrings, Arrows #-}

module Main
  where

import Control.Arrow ((>>>), (>>^), arr)
import Data.Monoid (mempty)
import System.FilePath (joinPath, splitPath)

import Hakyll

main :: IO ()
main = hakyllWith config $ do
    match "templates/*" $ compile templateCompiler

    match "css/fonts/**" $ route idRoute >> compile copyFileCompiler
    match "css/*" $ do
        route $ setExtension "css"
        compile $ byExtension (error "Not a CSS/Stylus file")
            [ (".css", compressCssCompiler)
            , (".styl", stylusCompiler)
            ]

    match "js/*" $ do
        route $ setExtension "js"
        compile $ byExtension (error "Not a JS/CoffeScript file")
            [ (".js", compressJsCompiler)
            , (".coffee", coffeeCompiler)
            ]

    match "images/favicon.ico" $ do
        route $ customRoute (joinPath . tail . splitPath . toFilePath)
        compile copyFileCompiler
    match "images/*" $ do
        route idRoute
        compile copyFileCompiler

    match "files/**" $ do
        route idRoute
        compile copyFileCompiler

    match "pages/**" $ do
        route $ customRoute (joinPath . tail . splitPath . toFilePath)
            `composeRoutes` setExtension "html"
        compile $ pageCompiler
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    match "blog/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/blog-post.html"
            >>> applyTemplateCompiler "templates/default.html"

    match "blog.html" $ route idRoute
    create "blog.html" $ constA mempty
        >>> arr (setField "title" "Blog")
        >>> setFieldPageList recentFirst
                "templates/blog-item.html" "posts" "blog/*"
        >>> applyTemplateCompiler "templates/blog.html"
        >>> applyTemplateCompiler "templates/default.html"

    create "tags" $
        requireAll "blog/*" (\_ ps -> readTags ps :: Tags String)

    match "tags/*" $ route $ setExtension "html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (fromCapture "tags/*" t, makeTagList t p)))

    match "feed.rss" $ route idRoute
    create "feed.rss" $ requireAll_ "blog/*"
        >>> renderRss feedConfiguration

    match "feed.atom" $ route idRoute
    create "feed.atom" $ requireAll_ "blog/*"
        >>> renderAtom feedConfiguration


    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

  where
    stylusCompiler :: Compiler Resource String
    stylusCompiler = getResourceString >>> unixFilter "stylus" ["-c"]

    compressJsCompiler :: Compiler Resource String
    compressJsCompiler = getResourceString >>> unixFilter "yuicompressor" ["--type", "js"]

    coffeeCompiler :: Compiler Resource String
    coffeeCompiler = getResourceString >>> unixFilter "coffee" ["-s", "-c"]
                                       >>> unixFilter "yuicompressor" ["--type", "js"]

    makeTagList :: String -> [Page String] -> Compiler () (Page String)
    makeTagList tag posts = constA posts
        >>> pageListCompiler recentFirst "templates/blog-item.html"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" ("Posts tagged " ++ tag))
        >>> applyTemplateCompiler "templates/blog.html"
        >>> applyTemplateCompiler "templates/default.html"

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { destinationDirectory = "_build"
    , storeDirectory       = "_cache"
    , deployCommand        = "rsync -ave ssh _build/* b52@reaktor42.de:/var/www/reaktor42.de"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "reaktor42"
    , feedDescription = ""
    , feedAuthorName  = "Oliver Mader"
    , feedRoot        = "http://reaktor42.de"
    }


