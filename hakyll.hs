{-# LANGUAGE OverloadedStrings, Arrows #-}

module Main
  where

import Control.Arrow ((>>>), (>>^), arr)
import Data.Monoid (mempty)
import System.FilePath (joinPath, splitPath)
import Text.Pandoc (WriterOptions (..), HTMLMathMethod(MathJax))

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
        compile $ pageCompiler'
            >>> applyTemplateCompiler "templates/default.hamlet"

    match "blog/*" $ do
        route $ setExtension "html"
        compile $ pageCompiler'
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/blog-post.hamlet"
            >>> applyTemplateCompiler "templates/default.hamlet"

    match "blog.html" $ route idRoute
    create "blog.html" $ constA mempty
        >>> arr (setField "title" "Blog")
        >>> setFieldPageList (take 5 . recentFirst)
                "templates/blog-item.hamlet" "posts" "blog/*"
        >>> applyTemplateCompiler "templates/blog.hamlet"
        >>> applyTemplateCompiler "templates/default.hamlet"

    create "tags" $
        requireAll "blog/*" (\_ ps -> readTags ps :: Tags String)

    match "tags/*" $ route $ setExtension "html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (fromCapture "tags/*" t, makeTagList t p)))

    match "rss.xml" $ route idRoute
    create "rss.xml" $ requireAll_ "blog/*" >>> renderRss feedConfiguration

    match "atom.xml" $ route idRoute
    create "atom.xml" $ requireAll_ "blog/*" >>> renderAtom feedConfiguration

  where
    stylusCompiler :: Compiler Resource String
    stylusCompiler = getResourceString >>> unixFilter "stylus" ["-c"]

    compressJsCompiler :: Compiler Resource String
    compressJsCompiler = getResourceString >>> unixFilter "yui-compressor" ["--type", "js"]

    coffeeCompiler :: Compiler Resource String
    coffeeCompiler = getResourceString >>> unixFilter "coffee" ["-s", "-c"]
                                       >>> unixFilter "yui-compressor" ["--type", "js"]

    pageCompiler' :: Compiler Resource (Page String)
    pageCompiler' = pageCompilerWith defaultHakyllParserState
        defaultHakyllWriterOptions { writerHtml5 = True
                                   , writerSectionDivs = True
                                   , writerHTMLMathMethod = MathJax ""
                                   }

    makeTagList :: String -> [Page String] -> Compiler () (Page String)
    makeTagList tag posts = constA posts
        >>> pageListCompiler recentFirst "templates/blog-item.hamlet"
        >>> arr (copyBodyToField "posts" . fromBody)
        >>> arr (setField "title" ("Posts tagged " ++ tag))
        >>> applyTemplateCompiler "templates/blog.hamlet"
        >>> applyTemplateCompiler "templates/default.hamlet"

config :: HakyllConfiguration
config = defaultHakyllConfiguration
    { destinationDirectory = "_build"
    , storeDirectory       = "_cache"
    , deployCommand        = "rsync -ave ssh _build/* b52@reaktor42.de:/var/www/reaktor42.de/www"
    }

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "reaktor42"
    , feedDescription = "a personal platform"
    , feedAuthorName  = "Oliver Mader"
    , feedRoot        = "http://reaktor42.de"
    }


