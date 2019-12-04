{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Stack
import           StackSidebarCW

import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Vector                   (Vector, fromList)
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

type State = ()

data Event =
    Closed

view' :: State -> AppView Gtk.Window Event
view' s =
    bin Gtk.Window [#title := "Calculator", on #deleteEvent (const (False, Closed))] $
    stackBox [] Gtk.Box StackBoxProperties {containerConstructor = Gtk.Box, children = boxChildren}
  where
    boxChildren =
        [ BoxChild defaultBoxChildProperties {expand = True} stack
        , BoxChild defaultBoxChildProperties  sidebar
        ]
    stack =
        container
            Gtk.Stack
            []
            [ StackChild (mkStackChildProperties "her") (widget Gtk.Label [#label := "stack 1"])
            , StackChild (mkStackChildProperties "her2") (widget Gtk.Label [#label := "stack 2"])
            ]
    sidebar = widget Gtk.StackSidebar []

{-stackBox
        []
        Gtk.Paned
        StackBoxProperties {containerConstructor = Gtk.Paned, children = paneChildren}
  where
    paneChildren = Children(fromList[(pane paneProps stack), (pane paneProps sidebar)])
    paneProps = defaultPaneProperties {resize = True}-}
update' :: State -> Event -> Transition State Event
update' s e =
    case e of
        Closed -> Exit

main :: IO ()
main = do
    run App {view = view', update = update', inputs = [], initialState = ()}
