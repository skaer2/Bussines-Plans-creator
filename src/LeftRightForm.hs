{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}

module LeftRightForm where

import Data.Vector (Vector)
import           Data.Text                      (Text, pack)
import qualified GI.GObject                     as GI
import qualified GI.Gtk                         as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.EventSource (fromCancellation)

data TextSubmitted =
    TextSubmitted Text Text

data LeftRightProperties =
    LeftRightProperties
        { leftPlaceHolder  :: Maybe Text
        , rightPlaceHolder :: Maybe Text
        }
    deriving (Eq)

noPlaceHolder :: LeftRightProperties
noPlaceHolder = LeftRightProperties Nothing Nothing

leftRightForm ::
       Vector (Attribute Gtk.Box TextSubmitted) -> LeftRightProperties -> Widget TextSubmitted
leftRightForm customAttributes customParams =
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
    customWidget = Gtk.Box
    customCreate props = do
        box <- Gtk.new Gtk.Box []
        entryLeft <- Gtk.new Gtk.Entry []
        entryRight <- Gtk.new Gtk.Entry []
        Gtk.entrySetPlaceholderText entryLeft $ leftPlaceHolder props
        Gtk.entrySetPlaceholderText entryRight $ rightPlaceHolder props
        confirmButton <- Gtk.new Gtk.Button [#label Gtk.:= pack "X"]
        #packStart box entryLeft False False 0
        #packStart box entryRight False False 0
        #packStart box confirmButton False False 0
        return (box, (entryLeft, entryRight, confirmButton))
    customPatch old new (entryLeft, entryRight, confirmButton)
        | old == new = CustomKeep
        | otherwise =
            CustomModify $ \_ -> do
                Gtk.entrySetPlaceholderText entryLeft $ leftPlaceHolder new
                Gtk.entrySetPlaceholderText entryRight $ rightPlaceHolder new
                return (entryLeft, entryRight, confirmButton)
    customSubscribe _ (entryLeft :: Gtk.Entry, entryRight :: Gtk.Entry, confirmButton :: Gtk.Button) _ cb = do
        leftText <- #getText entryLeft
        rightText <- #getText entryRight
        h <- Gtk.on confirmButton #clicked $ cb (TextSubmitted leftText rightText)
        return (fromCancellation (GI.signalHandlerDisconnect confirmButton h))
