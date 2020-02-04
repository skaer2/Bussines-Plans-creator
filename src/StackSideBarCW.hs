{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}

module StackSidebarCW where

import           Data.Typeable
import           Data.Vector                        (Vector, toList)

import qualified GI.Gtk                             as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.State

data StackBoxProperties widget parent child event =
    StackBoxProperties
        { containerConstructor :: (Gtk.ManagedPtr widget -> widget)
        , children             :: parent (child event)
        }

stackBox ::
       ( Typeable widget
       , Typeable parent
       , Typeable child
       , Typeable event
       , Patchable child
       , EventSource child
       , Gtk.IsWidget widget
       , Gtk.IsContainer widget
       , IsContainer widget child
       , ToChildren widget parent child
       )
    => Vector (Attribute widget event)
    -> (Gtk.ManagedPtr widget -> widget)
    -> StackBoxProperties widget parent child event
    -> Widget event
stackBox customAttributes cons customParams =
    Widget
        (CustomWidget
             { customWidget
             , customCreate
             , customPatch
             , customSubscribe
             , customAttributes
             , customParams
             })
  where
    customWidget = cons
    customCreate props = do
        containerPtr <- Gtk.new (containerConstructor props) []
        let theStack:theSidebar:_ =
                toList $ unChildren $ toChildren (containerConstructor props) $ children props
        stackState <- create $ theStack
        sidebarState <- create $ theSidebar
        stackPtrWidget <- someStateWidget stackState
        sidebarPtrWidget <- someStateWidget sidebarState
        stackPtr <- Gtk.castTo Gtk.Stack stackPtrWidget
        sidebarPtr <- Gtk.castTo Gtk.StackSidebar sidebarPtrWidget
        case (stackPtr, sidebarPtr) of
            (Nothing, Nothing) -> putStrLn "Not a sidebar, and not a stack"
            (Nothing, _) -> putStrLn "Not a stack"
            (_, Nothing) -> putStrLn "Not a sidebar"
            (Just stackPtr', Just sidebarPtr') -> do
                maybe (pure ()) Gtk.widgetDestroy =<< Gtk.stackSidebarGetStack sidebarPtr'
                Gtk.stackSidebarSetStack sidebarPtr' stackPtr'
                appendChild containerPtr theSidebar sidebarPtrWidget
                appendChild containerPtr theStack stackPtrWidget
        return (containerPtr, stackState)
    customPatch ::
        (ToChildren widget parent child, Patchable child) => 
           StackBoxProperties widget parent child event
        -> StackBoxProperties widget parent child event
        -> SomeState
        -> CustomPatch widget SomeState
    customPatch old new someState =
        case patch someState theStackOld theStackNew of
            Modify modify -> CustomModify $ \_ -> modify
            Replace _     -> CustomReplace
            Keep          -> CustomKeep
      where
        theStackOld:theSidebarOld:_ =
            toList $ unChildren $ toChildren (containerConstructor old) $ children old
        theStackNew:theSidebarNew:_ =
            toList $ unChildren $ toChildren (containerConstructor new) $ children new
    customSubscribe props someState _ cb = do
        let theStack:theSidebar:_ =
                toList $ unChildren $ toChildren (containerConstructor props) $ children props
        subscribe theStack someState cb
