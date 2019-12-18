{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Grid
import           ProjectInfo
import           Stack
import           StackSidebarCW

import           Control.Concurrent.Async      (async)
import           Control.Monad                 (void)
import           Data.ByteString               (ByteString)

import           Data.List.Index               (imap, indexed)
import           Data.Text                     (Text, pack, unpack)
import           Data.Time                     (Day, defaultTimeLocale, parseTimeM)
import           Data.Vector                   (Vector, fromList, snoc)
import           Text.Read                     (readMaybe)

import qualified GI.Gdk                        as Gdk
import qualified GI.Gtk                        as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple

type State = ProjectInfo

data Event
    = TextChanged Id Text
    | UpdateForms FormEvents
    | Closed
    | PrintState

tableClass :: [Text]
tableClass = ["borders"]

calculateBalance :: ProjectInfo -> [Int]
calculateBalance state = zipWith (-) (combineIncome state) (combineExpenses state)

view' :: State -> AppView Gtk.Window Event
view' state =
    bin Gtk.Window [#title := "Calculator", on #deleteEvent (const (False, Closed))] $
    stackBox [] Gtk.Box StackBoxProperties {containerConstructor = Gtk.Box, children = boxChildren}
  where
    boxChildren =
        [ BoxChild defaultBoxChildProperties {expand = True, padding = 10} stack
        , BoxChild defaultBoxChildProperties sidebar
        ]
    stack =
        container
            Gtk.Stack
            []
            [ StackChild
                  (mkStackChildProperties "Project Info")
                  (container
                       Gtk.Grid
                       [#columnSpacing := 3]
                       [ gridC 0 0 (label "StartDate")
                       , gridC 1 0 dateForm
                       , gridC 0 1 (label "Duration")
                       , gridC 1 1 durationForm
                       , gridCW 0 2 2 printStateBtn
                       ])
            , StackChild
                  (mkStackChildProperties "Income")
                  (container
                       Gtk.Grid
                       [#columnSpacing := 3]
                       [gridC 0 0 (label "Income"), gridC 1 0 incomeForm])
            , StackChild (mkStackChildProperties "Expenses") (boxedExpenses)
            , StackChild (mkStackChildProperties "Results") (resultsBox)
            ]
    toBoxChildVector x = fromList (map (BoxChild defaultBoxChildProperties) x)
    --makeExpenses :: State -> [box with [widget event]] EXPENSES - a box of QUARTERS
    boxedExpenses = boxV $ toBoxChildVector makeExpenses
    makeExpenses = map (boxedQuarters . forms) (expenses state)
    -- V(DONE)  makeQuarter :: ExpenseInfoQuarter -> [widget event] A QUARTER - a box of FORMS
    boxedQuarters x =
        boxV $
        snoc
            (toBoxChildVector (makeQuarter x))
            (BoxChild defaultBoxChildProperties $
             widget Gtk.Button [on #clicked (UpdateForms FormAdded), #label := pack "+"])
    makeQuarter expQuarter = map (boxedForms) $ indexed expQuarter
    -- V(DONE) makeForms :: (Int, (ValueName, Value)) ->  widget event A FORM - a box of widgets
    boxedForms x = boxH $ toBoxChildVector (makeForms x)
    makeForms :: (Int, (Text, Int)) -> [Widget Event]
    makeForms (pos, (name, value)) =
        [ widget
              Gtk.Entry
              [ #placeholderText := pack "Name"
              , onM #changed (fmap (UpdateForms . FormNameChanged pos) . Gtk.entryGetText)
              ]
        , widget
              Gtk.Entry
              [ #placeholderText := pack "Value"
              , onM #changed
                    (fmap (UpdateForms . FormValueChanged pos . parseTextToInt) . Gtk.entryGetText)
              ]
        , widget Gtk.Button [#label := "x", on #clicked $ UpdateForms $ FormDeleted pos]
        ]
    resultsBox = boxV [balanceTable]
    balanceTable =
        BoxChild defaultBoxChildProperties $
        container Gtk.Grid [#columnSpacing := 0] $ fromList $ imap quarter (calculateBalance state)
    quarter quarterIndex quarterBalance =
        gridC
            quarterIndex
            0
            (widget Gtk.Label [#label := pack (show quarterBalance), classes tableClass])
    gridC left' top' widget' = GridChild (mkGridChildProperties left' top') widget'
    gridCW left' top' width' widget' =
        GridChild ((mkGridChildProperties left' top') {width = width'}) widget'
    printStateBtn = widget Gtk.Button [#label := "print state", on #clicked PrintState]
    sidebar = widget Gtk.StackSidebar []
    dateForm = simpleForm StartDate 10
    label text = (widget Gtk.Label [#label := text])
    durationForm = simpleForm Duration 3
    incomeForm = simpleForm Income 0
    boxV widgets = container Gtk.Box [#orientation := Gtk.OrientationVertical] widgets
    boxH widgets = container Gtk.Box [#orientation := Gtk.OrientationHorizontal] widgets
    simpleForm widgetId size =
        (widget
             Gtk.Entry
             [ onM #changed (fmap (TextChanged widgetId) . Gtk.entryGetText)
             --, #maxWidthChars := size
             , #maxLength := size
             --, #widthChars := size
             ])

update' :: State -> Event -> Transition State Event
update' s e =
    case e of
        PrintState ->
            Transition
                s
                (do print s
                    return Nothing)
        UpdateForms formEvent -> Transition (updateExpenses formEvent s) (return Nothing)
        TextChanged widgetId text -> Transition (addTextToState s widgetId text) (return Nothing)
        Closed -> Exit

parseTextToDate :: Text -> Maybe Day
parseTextToDate text = parseTimeM False defaultTimeLocale "%d-%-m-%-Y" (unpack text)

parseTextToInt :: Text -> Int
parseTextToInt text =
    case (readMaybe $ unpack text) of
        Just x  -> x
        Nothing -> 0

addTextToState :: State -> Id -> Text -> State
addTextToState state StartDate text = state {info = (info state) {startDate = parseTextToDate text}}
addTextToState state Duration text =
    state {info = (info state) {duration = readMaybe $ unpack text}}
addTextToState state Income _ = state -- TODO delete this line

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
  where
    app = App {view = view', update = update', inputs = [], initialState = noProjectInfo}
