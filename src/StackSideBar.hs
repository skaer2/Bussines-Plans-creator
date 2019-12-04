{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module StackSidebar where

import           Data.Text
import           Data.Vector                        (Vector, fromList)
import           GHC.Ptr                            (nullPtr)
import qualified GI.GLib                            as GLib
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Widget

data SidebarChild event =
    SidebarChild
        { child :: Widget event
        }
    deriving (Functor)

instance Patchable SidebarChild where
    create = create . child
    patch s s1 s2 = patch s (child s1) (child s2)

instance EventSource SidebarChild where
    subscribe SidebarChild {..} = subscribe child

instance IsContainer Gtk.StackSidebar SidebarChild where
    appendChild bar _ widget = do
        stack <- Gtk.stackSidebarGetStack bar
        case (stack) of
            Nothing -> do
                stack' <- Gtk.castTo Gtk.Stack widget
                case (stack') of
                    Nothing -> putStrLn "Not a stack"
                    Just stack' -> Gtk.stackSidebarSetStack bar stack'
            _ ->
                GLib.logDefaultHandler
                    (Just $ pack "gi-gtk-declarative")
                    [GLib.LogLevelFlagsLevelWarning]
                    (Just $
                         pack "appendChild: The `GI.Gtk.StackSidebar` widget can have 1 stack. Additional children will be ignored.")
                    nullPtr
    replaceChild bar _ i old new = do
        Gtk.widgetDestroy old
        stack' <- Gtk.castTo Gtk.Stack new
        case (stack') of
            Nothing -> putStrLn "Not a stack"
            Just stack' -> Gtk.stackSidebarSetStack bar stack'

--setStack :: Widget event -> SidebarChild event
--setStack stack = SidebarChild {child = stack}
data Stack child =
    Stack child
    deriving (Functor)

instance ToChildren Gtk.StackSidebar Stack SidebarChild where
    toChildren _ (Stack s) = Children (fromList [s])
