{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import App

import           Control.Concurrent.Async      (async)
import           Control.Monad                 (void)
import           Data.ByteString               (ByteString)

import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple


styles :: ByteString
styles =
    mconcat
        [ ".borders {border: 1px solid black; padding: 5px; margin: -1px; border-right-style: none}"
        , ".borderRight {border-right: 1px solid black; }"
        ]

main :: IO ()
main = do
    void $ Gtk.init Nothing
  -- Set up screen and CSS provider
    screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
    p <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData p styles
    Gtk.styleContextAddProviderForScreen screen p (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
  -- Start main loop
    void . async $ do
        void $ runLoop app
        Gtk.mainQuit
    Gtk.main
