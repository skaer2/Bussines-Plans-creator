{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module App
    ( app
    ) where

import           Calculations
import           FileSaves
import           Grid
import           ProjectInfo
import           Stack
import           StackSidebarCW

import           Control.Monad.State.Lazy           (evalState)
import           Data.Int                           (Int32)
import           Data.List.Index                    (imap, indexed)
import           Data.Text                          (Text, pack, unpack)
import           Data.Time                          (Day, defaultTimeLocale, formatTime, parseTimeM)
import           Data.Vector                        (Vector, cons, fromList, snoc)
import           Text.Read                          (readMaybe)

import qualified GI.Gtk                             as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import           GI.Gtk.Declarative.Container.Class

app :: App Gtk.Window State Event
app = App {view = view', update = update', inputs = [], initialState = empty}

type State = ProjectInfo

data Event
    = TextChanged Id Text
    | UpdateForms Int FormEvents
    | UpdateExpenseQuarters QuarterEvents
    | UpdateProducts Int ProductEvents
    | UpdateIncomeQuarters QuarterEvents
    | SaveFileChanged (Maybe Text)
    | SaveToFile FilePath
    | LoadFromFile FilePath
    | LoadState State
    | Closed
    | PrintState

tableClass :: [Text]
tableClass = ["borders"]

borderClass :: [Text]
borderClass = ["borderRight"]

view' :: State -> AppView Gtk.Window Event
view' state =
    bin Gtk.Window
        [ #title := pack (saveFile $ info state)
        , on #deleteEvent (const (False, Closed))
        , #defaultHeight := 800
        , #defaultWidth := 1000
        ] $
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
            [ StackChild (mkStackChildProperties "Project") (projectTab state)
            , StackChild (mkStackChildProperties "Project Info") (infoTab state)
            , StackChild (mkStackChildProperties "Income") (incomeTab state)
            , StackChild (mkStackChildProperties "Expenses") (expenseTab state)
            , StackChild (mkStackChildProperties "Results") (resultsBox state)
            ]
    sidebar = widget Gtk.StackSidebar []

projectTab state =
    boxV
        [ widget Gtk.Label [#label := pack "Choose a project file to load or save to"]
        , BoxChild defaultBoxChildProperties {expand = True, fill = True} $
          widget
              Gtk.FileChooserWidget
              [ #action := Gtk.FileChooserActionSave
              , onM #selectionChanged
                    (fmap (SaveFileChanged . fmap pack) . Gtk.fileChooserGetFilename)
              ]
        , boxH
              [ widget
                    Gtk.Button
                    [ #label := pack "Save Project"
                    , on #clicked (SaveToFile $ saveFile $ info state)
                    ]
              , widget
                    Gtk.Button
                    [ #label := pack "Load Project"
                    , on #clicked (LoadFromFile $ saveFile $ info state)
                    ]
              ]
        ]

simpleForm ::
       FromWidget (SingleWidget Gtk.Entry) target => Id -> Int32 -> Maybe Text -> target Event
simpleForm widgetId size Nothing =
    (widget
         Gtk.Entry
         [onM #changed (fmap (TextChanged widgetId) . Gtk.entryGetText), #maxLength := size])
simpleForm widgetId size (Just text) =
    (widget
         Gtk.Entry
         [ #text := text
         , onM #changed (fmap (TextChanged widgetId) . Gtk.entryGetText)
         , #maxLength := size
         ])

label :: FromWidget (SingleWidget Gtk.Label) target => Text -> target Event
label text = (widget Gtk.Label [#label := text])

toBoxChildVector :: [Widget event] -> Vector (BoxChild event)
toBoxChildVector x = fromList (map (BoxChild defaultBoxChildProperties {expand = False}) x)

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

quarterBox ::
       FromWidget (Container Gtk.Box (Children BoxChild)) target
    => (QuarterEvents -> Event)
    -> [Widget Event]
    -> target Event
quarterBox eventWrapper f =
    boxV $
    snoc
        (toBoxChildVector f)
        (BoxChild defaultBoxChildProperties $
         widget Gtk.Button [#label := pack "+", on #clicked (eventWrapper QuarterAdded)])

quarterTopBox :: Int -> Int -> (QuarterEvents -> Event) -> BoxChild Event
quarterTopBox quarterPos repeating eventWrapper =
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
                    [ #text := pack (show repeating)
                    , onM #changed
                          (fmap (eventWrapper . QuarterRepeatChanged quarterPos . parseTextToInt) .
                           Gtk.entryGetText)
                    , #maxLength := 3
                    , #widthChars := 3
                    ]
              , widget Gtk.Label [#label := pack "x"]
              ]
        , BoxChild defaultBoxChildProperties {expand = True, fill = True} $
          container
              Gtk.Box
              [#halign := Gtk.AlignEnd]
              [ widget
                    Gtk.Button
                    [on #clicked (eventWrapper $ QuarterDeleted quarterPos), #label := pack "X"]
              ]
        ]

infoTab :: FromWidget (Container Gtk.Grid (Children GridChild)) target => State -> target Event
infoTab state =
    container
        Gtk.Grid
        [#columnSpacing := 3]
        [ gridC 0 0 (label "StartDate")
        , gridC 1 0 dateForm
        , gridC 0 2 (label "Starting Funds")
        , gridC 1 2 startingFundsForm
        , gridCW 0 3 2 printStateBtn
        ]
  where
    printStateBtn = widget Gtk.Button [#label := "print state", on #clicked PrintState]
    dateForm =
        simpleForm
                StartDate
                10
                text
      where
        maybeDay = startDate $ info state
        text = case (maybeDay) of
                    (Nothing) -> Nothing
                    (Just day) -> (Just day) 
                    --(Just day) -> (Just $ pack (formatTime defaultTimeLocale "%-d.%-m.%-Y" day))
    startingFundsForm = simpleForm StartingFunds 0 (Just (pack (show $ startingFunds $ info state)))

incomeTab :: State -> Widget Event
incomeTab = quarterBox UpdateIncomeQuarters . makeIncome
  where
    makeIncome state = map makeQuarter (indexed $ getQuarters $ income state)
    makeQuarter (quarterPos, (repeating, incQuarter)) =
        container
            Gtk.Box
            [#orientation := Gtk.OrientationVertical]
            [quarterTopBox quarterPos repeating UpdateIncomeQuarters, bottomBox]
      where
        bottomBox = boxedProducts quarterPos incQuarter
    boxedProducts quarterPos x =
        boxV $
        snoc
            (toBoxChildVector (makeProducts quarterPos x))
            (BoxChild defaultBoxChildProperties $
             widget
                 Gtk.Button
                 [#label := pack "+", on #clicked (UpdateProducts quarterPos ProductAdded)])
    makeProducts quarterPos incQuarter =
        map (makeProduct quarterPos) (indexed $ getProducts incQuarter)
    makeProduct :: Int -> (Int, Product) -> Widget Event
    makeProduct quarterPos (pos, product) =
        container
            Gtk.Grid
            [#columnSpacing := 3]
            [ gridCW 0 0 2 $
              widget
                  Gtk.Entry
                  [ #text := productName product
                  , #placeholderText := "Product's name"
                  , onM #changed
                        (fmap (UpdateProducts quarterPos . ProductNameChanged pos) .
                         Gtk.entryGetText)
                  ]
            , gridC 2 0 $
              widget
                  Gtk.Button
                  [#label := pack "X", on #clicked (UpdateProducts quarterPos $ ProductDeleted pos)]
            , gridC 0 1 $ widget Gtk.Label [#label := "Product's sell quantity (in units)"]
            , gridC 1 1 $
              widget
                  Gtk.Entry
                  [ #text := (pack $ show $ sellQuantity product)
                  , #placeholderText := "Sell quantity"
                  , onM #changed
                        (fmap
                             (UpdateProducts quarterPos .
                              ProductSQuantityChanged pos . parseTextToInt) .
                         Gtk.entryGetText)
                  ]
            , gridC 0 2 $ widget Gtk.Label [#label := "Product's sell price (per unit)"]
            , gridC 1 2 $
              widget
                  Gtk.Entry
                  [ #text := (pack $ show $ sellPrice product)
                  , #placeholderText := "Sell price"
                  , onM #changed
                        (fmap
                             (UpdateProducts quarterPos . ProductSPriceChanged pos . parseTextToInt) .
                         Gtk.entryGetText)
                  ]
            , gridC 0 3 $ widget Gtk.Label [#label := "Product's producing cost (per unit)"]
            , gridC 1 3 $
              widget
                  Gtk.Entry
                  [ #text := (pack $ show $ producingCost product)
                  , #placeholderText := "Producing cost"
                  , onM #changed
                        (fmap
                             (UpdateProducts quarterPos .
                              ProductProducingCostChanged pos . parseTextToInt) .
                         Gtk.entryGetText)
                  ]
            ]

expenseTab :: State -> Widget Event
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
            [quarterTopBox quarterPos repeating UpdateExpenseQuarters, bottomBox]
      where
        bottomBox = boxedForms quarterPos $ forms expQuarter
    boxedForm quarterPos x = boxH $ toBoxChildVector $ (makeForm quarterPos x)
    makeForm :: Int -> (Int, (Text, Int)) -> [Widget Event]
    makeForm quarterPos (pos, (name', value)) =
        [ widget
              Gtk.Entry
              [ #text := name'
              , #placeholderText := pack "Name"
              , onM #changed
                    (fmap (UpdateForms quarterPos . FormNameChanged pos) . Gtk.entryGetText)
              ]
        , widget
              Gtk.Entry
              [ #text := pack (show value)
              , #placeholderText := pack "Value"
              , onM #changed
                    (fmap (UpdateForms quarterPos . FormValueChanged pos . parseTextToInt) .
                     Gtk.entryGetText)
              ]
        , widget Gtk.Button [#label := "x", on #clicked $ UpdateForms quarterPos $ FormDeleted pos]
        ]

resultsBox :: FromWidget (Container Gtk.Box (Children BoxChild)) target => State -> target Event
resultsBox state = boxV [balanceTable]
  where
    balanceTable =
        BoxChild defaultBoxChildProperties $
        container Gtk.Grid [#columnSpacing := 3] $
        cons (gridCW 0 0 2 $ widget Gtk.Label [#label := pack "Balance"]) $ quarters
    runCalculateBalance state =
        evalState (calculateBalance (combineIncome state) (combineExpenses state)) $
        startingFunds $ info state
    quarters = fromList $ imap quarter (runCalculateBalance state)
    quarter quarterIndex quarterBalance =
        gridC
            (quarterIndex + 2)
            0
            (widget
                 Gtk.Entry
                 [ #text := pack (show quarterBalance)
                 , #editable := False
                 , #canFocus := False
                 , classes tableClass
                 ])

update' :: State -> Event -> Transition State Event
update' s e =
    case e of
        PrintState ->
            Transition
                s
                (do print s
                    return Nothing)
        SaveFileChanged Nothing -> Transition s (return Nothing)
        SaveFileChanged (Just filename) ->
            Transition (addTextToState s SaveFile filename) (return Nothing)
        SaveToFile filename ->
            Transition
                s
                (do saveProject filename s
                    return Nothing)
        LoadFromFile filename ->
            Transition
                s
                (do newState <- loadProject filename
                    return $ Just (LoadState newState))
        LoadState newState -> Transition newState (return Nothing)
        UpdateForms quarterPos formEvent ->
            Transition (updateExpenses formEvent quarterPos s) (return Nothing)
        UpdateExpenseQuarters quarterEvent ->
            Transition (s {expenses = updateQuarters quarterEvent (expenses s)}) (return Nothing)
        UpdateProducts quarterPos productEvent ->
            Transition (updateIncome productEvent quarterPos s) (return Nothing)
        UpdateIncomeQuarters quarterEvent ->
            Transition (s {income = updateQuarters quarterEvent (income s)}) (return Nothing)
        TextChanged widgetId text -> Transition (addTextToState s widgetId text) (return Nothing)
        Closed -> Exit

parseTextToDate :: Text -> Maybe Day
parseTextToDate text = parseTimeM False defaultTimeLocale "%d.%-m.%-Y" (unpack text)

parseTextToInt :: Text -> Int
parseTextToInt text =
    case (readMaybe $ unpack text) of
        Just x  -> x
        Nothing -> 0

addTextToState :: State -> Id -> Text -> State
addTextToState state StartDate text = state {info = (info state) {startDate = Just text}}
addTextToState state StartingFunds text =
    state {info = (info state) {startingFunds = parseTextToInt text}}
addTextToState state SaveFile text = state {info = (info state) {saveFile = unpack text}}
