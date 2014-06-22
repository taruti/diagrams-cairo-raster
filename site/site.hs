{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll

main :: IO ()
main = hakyll $ do
    match "img/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "haddock/*" $ do
        route   idRoute
        compile copyFileCompiler

    match roots $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "tmpl/default.html" tmplCtx
            >>= relativizeUrls

    match "tmpl/*" $ compile templateCompiler

roots = fromList ["index.md"]

tmplCtx :: Context String
tmplCtx =
    dateField "date" "%Y-%m-%d" <>
    defaultContext
