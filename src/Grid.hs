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

-- | Implementation of 'Gtk.Grid' as a declarative container.
module Grid
    ( GridChild(GridChild) -- (..)
    , GridChildProperties(..)
    , mkGridChildProperties
    ) where

import           Data.Vector                        (Vector)
import qualified GI.Gtk                             as Gtk

import           GI.Gtk.Declarative.Container.Class
import           GI.Gtk.Declarative.EventSource
import           GI.Gtk.Declarative.Patch
import           GI.Gtk.Declarative.Widget

-- | Describes a child widget to be added with 'boxAppend' to a 'Grid'.
data GridChild event =
    GridChild
        { properties :: GridChildProperties
        , child      :: Widget event
        }
    deriving (Functor)

-- | Values used when /packing/ child widgets into boxes.
data GridChildProperties =
    GridChildProperties
        { left   :: Int
        , top    :: Int
        , width  :: Int
        , height :: Int
        }
    deriving (Eq, Show)

-- Use this to construct properties and override width and height if needed.
-- It's undefined behavior to create two widgets with overlapping places in the
-- grid, taking left and top as a parameter provides some minimal
-- protection at the use site.
mkGridChildProperties :: Int -> Int -> GridChildProperties
mkGridChildProperties left top = GridChildProperties {left = left, top = top, width = 1, height = 1}

instance Patchable GridChild where
    create = create . child
    patch s (GridChild props1 child1) gc2@(GridChild props2 child2)
        | props1 == props2 = patch s child1 child2
        | otherwise = Replace $ create gc2

instance EventSource GridChild where
    subscribe GridChild {..} = subscribe child

instance ToChildren Gtk.Grid Vector GridChild

instance IsContainer Gtk.Grid GridChild where
    appendChild grid GridChild {properties = GridChildProperties {..}} widget' =
        Gtk.gridAttach
            grid
            widget'
            (fromIntegral left)
            (fromIntegral top)
            (fromIntegral width)
            (fromIntegral height)
    replaceChild grid gridChild' _ old new = do
        Gtk.widgetDestroy old
        appendChild grid gridChild' new
