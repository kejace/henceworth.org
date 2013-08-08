--------------------------------------------------------------------------------
{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (forM_, zipWithM_, liftM)
import           Hakyll

import Control.Arrow ((>>>), arr, (^>>), (>>^))
import           Data.Monoid     ((<>), mconcat)
import           Prelude         hiding (id)
import Data.Monoid (mempty)
import Data.List (find)
import Data.Maybe (fromMaybe)

import Text.Pandoc
import Text.Pandoc.Shared (stringify)
import Text.Pandoc.Options

--------------------------------------------------------------------------------
import           Data.Monoid     ((<>), mconcat)
import           Prelude         hiding (id)
import           System.Cmd      (system)
import           System.FilePath (replaceExtension, takeDirectory)
import qualified Text.Pandoc     as Pandoc


--------------------------------------------------------------------------------
import           Hakyll


-- Allow for reference style links in markdown
pandocWriteOptions = defaultHakyllWriterOptions
    { writerReferenceLinks = True
    }

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    let allPosts = "posts/**/*.markdown"

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "stylesheets/*" $ do
        route   idRoute
        compile compressCssCompiler    


    tags <- buildCategories allPosts (fromCapture "tags/*.html")    

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/frontpage_skeleton.html" defaultContext
            >>= relativizeUrls


    -- Render each and every post
    match allPosts $ do
        route   $ setExtension ".html"
        compile $ do
            item <- getUnderlying
            pandocCompilerWithTransform defaultHakyllReaderOptions
                                    defaultHakyllWriterOptions {
                                        writerTableOfContents = True
                                      , writerTemplate = "$body$"
                                      , writerStandalone = True
                                      } removeNotes  

                >>= saveSnapshot "content"
             --   >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post_skeleton.html" (postCtx tags)
             --   >>= loadAndApplyTemplate "templates/frontpage_skeleton.html" defaultContext
                >>= relativizeUrls

    match allPosts $ version "footnotes" $
        compile $ pandocCompilerWithTransform defaultHakyllReaderOptions
                                    defaultHakyllWriterOptions {
                                        writerTableOfContents = True
                                      , writerTemplate = "$body$"
                                      , writerStandalone = True
                                      } extractNotes            

    -- Post list
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (allPosts .&&. hasNoVersion)
            let ctx = constField "title" "Posts" <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/archive_skeleton.html" ctx
                >>= relativizeUrls

    -- Post tags
    tagsRules tags $ \tag pattern -> do
        let title = "Category " ++ tag

        -- Copied from posts, need to refactor
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll (pattern .&&. hasNoVersion)
            let ctx = constField "title" title <>
                        listField "posts" (postCtx tags) (return posts) <>
                        defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" ctx
                >>= loadAndApplyTemplate "templates/frontpage_skeleton.html" ctx
                >>= relativizeUrls

    -- Index
    match (fromList ["index.html", "allposts.html"]) $ do
        route idRoute
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll (allPosts .&&. hasNoVersion)
            let indexContext =
                    listField "posts" (postCtx tags) (return posts) <>
                    field "tags" (\_ -> renderTagList tags) <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexContext
                >>= loadAndApplyTemplate "templates/frontpage_skeleton.html" indexContext
                >>= relativizeUrls

    -- Render RSS feed
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            loadAllSnapshots "posts/*" "content"
                >>= fmap (take 10) . recentFirst
                >>= renderAtom (feedConfiguration "All posts") feedCtx            

    -- Read templates
    match "templates/*" $ compile $ templateCompiler

--------------------------------------------------------------------------------

postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    --, constField "footnotes" "bla bla footnote"
    , field "footnotes" $ \item ->
        loadBody ((itemIdentifier item) { identifierVersion = Just "footnotes"})
    , field "toc" $ \item ->
        loadBody ((itemIdentifier item) { identifierVersion = Just "toc"})
    , defaultContext
    ]

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , defaultContext
    ]

feedConfiguration :: String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle       = "henceworth - " ++ title
    , feedDescription = "Latest articles from Henceworth"
    , feedAuthorName  = "Hence Worth"
    , feedAuthorEmail = "hello@henceworth.org"
    , feedRoot        = "http://henceworth.org"
    }

---------------------------------------------------------------------------------

-- extractNotes, with the help of John MacFarlane
extractNotes :: Pandoc -> Pandoc 
extractNotes (Pandoc meta blocks) = Pandoc meta blocks' 
    where blocks' = queryWith getNoteContents blocks 

getNoteContents :: Inline -> [Block] 
getNoteContents (Note bs) = bs 
getNoteContents _         = [] 

removeNotes :: Pandoc -> Pandoc
removeNotes = bottomUp go
    where go (Note _) = Str ""
          go x = x
