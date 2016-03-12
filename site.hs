--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map as M
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath ( (</>), (<.>)
                                 , splitExtension, splitFileName
                                 , takeDirectory )


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html" `composeRoutes` appendIndex
        let siteCtx = dropIndexHtml "url" `mappend` defaultContext
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html" `composeRoutes`
                dateAndCategory     `composeRoutes`
                appendIndex
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route appendIndex
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    dropIndexHtml "url"                      `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    dropIndexHtml "url"          `mappend`
    defaultContext


--------------------------------------------------------------------------------
appendIndex :: Routes
appendIndex = customRoute $
    (\(p, e) -> p </> "index" <.> e) . splitExtension . toFilePath


--------------------------------------------------------------------------------
dateAndCategory :: Routes
dateAndCategory = metadataRoute $ \md ->
    gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ \s ->
        (replaceAll "-" (const "/") s) ++ (md M.! "category") ++ "/"


--------------------------------------------------------------------------------
dropIndexHtml :: String -> Context a
dropIndexHtml key = mapContext transform (urlField key) where
    transform url = case splitFileName url of
                        (p, "index.html") -> takeDirectory p
                        _                 -> url
