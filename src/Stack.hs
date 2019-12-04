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

module Stack
    ( StackChild(..)
    , StackChildProperties(..)
    , mkStackChildProperties
    ) where

import           Data.Text                          (Text)
import           Data.Vector                        (Vector)
import           Data.Word                          (Word32)
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Widget

data StackChild event =
    StackChild
        { properties :: StackChildProperties
        , child      :: Widget event
        }
    deriving (Functor)

data StackChildProperties =
    StackChildProperties
        { name  :: Text
        , title :: Text
        }

mkStackChildProperties :: Text -> StackChildProperties
mkStackChildProperties name = StackChildProperties {name = name, title = name}

instance Patchable StackChild where
    create = create . child
    patch s sc1 sc2 = patch s (child sc1) (child sc2)

instance EventSource StackChild where
    subscribe StackChild {..} = subscribe child

instance ToChildren Gtk.Stack Vector StackChild

instance IsContainer Gtk.Stack StackChild where
    appendChild stack stackChild widget' =
        Gtk.stackAddTitled stack widget' (name stackChildProperties) (title stackChildProperties)
            where
                stackChildProperties = properties stackChild
    replaceChild stack stackChild i old new = do
        Gtk.widgetDestroy old
        appendChild stack stackChild new
