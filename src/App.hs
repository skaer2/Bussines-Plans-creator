{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module App
    ( app
    ) where

import           Grid
import           ProjectInfo
import           Stack
import           StackSidebarCW

import           Data.Int                           (Int32)
import           Data.List.Index                    (imap, indexed)
import           Data.Text                          (Text, pack, unpack)
import           Data.Time                          (Day, defaultTimeLocale, parseTimeM)
import           Data.Vector                        (Vector, fromList, snoc)
import           Text.Read                          (readMaybe)

import qualified GI.Gtk                             as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Class

app :: App Gtk.Window State Event
app = App {view = view', update = update', inputs = [], initialState = noProjectInfo}

type State = ProjectInfo

data Event
    = TextChanged Id Text
    | UpdateForms Int FormEvents
    | UpdateExpenseQuarters QuarterEvents
    | UpdateIncomeQuarters QuarterEvents
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
            [ StackChild (mkStackChildProperties "Project Info") (infoTab state)
            , StackChild (mkStackChildProperties "Income") (incomeTab state)
            , StackChild (mkStackChildProperties "Expenses") (expenseTab state)
            , StackChild (mkStackChildProperties "Results") (resultsBox state)
            ]
    sidebar = widget Gtk.StackSidebar []

simpleForm :: FromWidget (SingleWidget Gtk.Entry) target => Id -> Int32 -> target Event
simpleForm widgetId size =
    (widget
         Gtk.Entry
         [onM #changed (fmap (TextChanged widgetId) . Gtk.entryGetText), #maxLength := size])

label :: FromWidget (SingleWidget Gtk.Label) target => Text -> target Event
label text = (widget Gtk.Label [#label := text])

toBoxChildVector :: [Widget event] -> Vector (BoxChild event)
toBoxChildVector x = fromList (map (BoxChild defaultBoxChildProperties) x)

boxV ::
       FromWidget (Container Gtk.Box (Children BoxChild)) target
    => Vector (BoxChild event)
    -> target event
boxV widgets = container Gtk.Box [#orientation := Gtk.OrientationVertical] widgets

boxH ::
       FromWidget (Container Gtk.Box (Children BoxChild)) target
    => Vector (BoxChild event)
    -> target event
boxH widgets = container Gtk.Box [#orientation := Gtk.OrientationHorizontal] widgets

gridC :: Int -> Int -> Widget event -> GridChild event
gridC left' top' widget' = GridChild (mkGridChildProperties left' top') widget'

gridCW :: Int -> Int -> Int -> Widget event -> GridChild event
gridCW left' top' width' widget' =
    GridChild ((mkGridChildProperties left' top') {width = width'}) widget'

--quarterBox :: (QuarterEvents -> Event) -> Widget Event -> ProjectInfo
quarterBox eventWrapper f =
    boxV $
    snoc
        (toBoxChildVector f)
        (BoxChild defaultBoxChildProperties $
         widget Gtk.Button [#label := pack "+", on #clicked (eventWrapper QuarterAdded)])

quarterTopBox :: Int -> (QuarterEvents -> Event) -> BoxChild Event
quarterTopBox quarterPos eventWrapper =
    BoxChild defaultBoxChildProperties $
    container
        Gtk.Box
        []
        [ BoxChild defaultBoxChildProperties {expand = True, fill = True} $
          container
              Gtk.Box
              [#halign := Gtk.AlignStart]
              [ widget
                    Gtk.Entry
                    [ onM #changed
                          (fmap (eventWrapper . QuarterRepeatChanged quarterPos . parseTextToInt) .
                           Gtk.entryGetText)
                    , #maxLength := 3
                    , #widthChars := 3
                    ]
              , widget Gtk.Label [#label := pack "/3"]
              ]
        , BoxChild defaultBoxChildProperties {expand = True, fill = True} $
          container
              Gtk.Box
              [#halign := Gtk.AlignEnd]
              [ widget
                    Gtk.Button
                    [ on #clicked (UpdateExpenseQuarters $ QuarterDeleted quarterPos)
                    , #label := pack "X"
                    ]
              ]
        ]

infoTab ::
       FromWidget (Container Gtk.Grid (Children GridChild)) target => ProjectInfo -> target Event
infoTab _ =
    container
        Gtk.Grid
        [#columnSpacing := 3]
        [ gridC 0 0 (label "StartDate")
        , gridC 1 0 dateForm
        , gridC 0 1 (label "Duration")
        , gridC 1 1 durationForm
        , gridCW 0 2 2 printStateBtn
        ]
  where
    printStateBtn = widget Gtk.Button [#label := "print state", on #clicked PrintState]
    dateForm = simpleForm StartDate 10
    durationForm = simpleForm Duration 3

--incomeTab :: FromWidget (Container Gtk.Grid (Children GridChild)) target => ProjectInfo -> target Event
--incomeTab :: ProjectInfo -> Widget Event
incomeTab = quarterBox UpdateIncomeQuarters . makeIncome
  where
    makeIncome state = map makeQuarter (indexed $ getQuarters $ income state)
    makeQuarter (quarterPos, (repeating, incQuarter)) =
        container
            Gtk.Box
            [#orientation := Gtk.OrientationVertical]
            [quarterTopBox quarterPos UpdateExpenseQuarters, bottomBox]
      where
        bottomBox = widget Gtk.Entry []

--makeQuarter expQuarter quarterInfo =
--expenseTab :: ProjectInfo -> Widget Event
expenseTab = quarterBox UpdateExpenseQuarters . makeExpenses
  where
    makeExpenses state = map makeQuarter (indexed $ getQuarters $ expenses state)
    boxedForms quarterPos x =
        boxV $
        snoc
            (toBoxChildVector (makeForms quarterPos x))
            (BoxChild defaultBoxChildProperties $
             widget Gtk.Button [on #clicked (UpdateForms quarterPos FormAdded), #label := pack "+"])
    makeForms quarterPos expQuarter = map (boxedForm quarterPos) $ indexed expQuarter -- TODO Make quarters
    makeQuarter (quarterPos, (repeating, expQuarter)) =
        container
            Gtk.Box
            [#orientation := Gtk.OrientationVertical]
            [quarterTopBox quarterPos UpdateExpenseQuarters, bottomBox]
      where
        bottomBox = boxedForms quarterPos $ forms expQuarter
    boxedForm quarterPos x = boxH $ toBoxChildVector $ (makeForm quarterPos x)
    makeForm :: Int -> (Int, (Text, Int)) -> [Widget Event]
    makeForm quarterPos (pos, (name, value)) =
        [ widget
              Gtk.Entry
              [ #placeholderText := pack "Name"
              , onM #changed
                    (fmap (UpdateForms quarterPos . FormNameChanged pos) . Gtk.entryGetText)
              ]
        , widget
              Gtk.Entry
              [ #placeholderText := pack "Value"
              , onM #changed
                    (fmap (UpdateForms quarterPos . FormValueChanged pos . parseTextToInt) .
                     Gtk.entryGetText)
              ]
        , widget Gtk.Button [#label := "x", on #clicked $ UpdateForms quarterPos $ FormDeleted pos]
        ]

resultsBox ::
       FromWidget (Container Gtk.Box (Children BoxChild)) target => ProjectInfo -> target Event
resultsBox state = boxV [balanceTable]
  where
    balanceTable =
        BoxChild defaultBoxChildProperties $
        container Gtk.Grid [#columnSpacing := 0] $ fromList $ imap quarter (calculateBalance state)
    quarter quarterIndex quarterBalance =
        gridC
            quarterIndex
            0
            (widget Gtk.Label [#label := pack (show quarterBalance), classes tableClass])

update' :: State -> Event -> Transition State Event
update' s e =
    case e of
        PrintState ->
            Transition
                s
                (do print s
                    return Nothing)
        UpdateForms quarterPos formEvent ->
            Transition (updateExpenses formEvent quarterPos s) (return Nothing)
        UpdateExpenseQuarters quarterEvent ->
            Transition (s {expenses = updateQuarters quarterEvent (expenses s)}) (return Nothing)
        UpdateIncomeQuarters quarterEvent ->
            Transition (s {income = updateQuarters quarterEvent (income s)}) (return Nothing)
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
