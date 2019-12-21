{-# LANGUAGE RecordWildCards #-}

module ProjectInfo where

import           Data.List.Index (deleteAt, modifyAt)
import           Data.Text       (Text, pack)
import           Data.Time

class Combinable a where
    combined :: a -> Int

data Id
    = StartDate
    | Duration

data BaseInfo =
    BaseInfo
        { startDate :: Maybe Day
        , duration  :: Maybe Int
        }
    deriving (Show)

noBaseInfo :: BaseInfo
noBaseInfo = BaseInfo Nothing Nothing

data ProjectInfo =
    ProjectInfo
        { info     :: BaseInfo
        , income   :: IncomeInfo
        , expenses :: ExpenseInfo
        , tax      :: TaxInfo
        }
    deriving (Show)

noProjectInfo :: ProjectInfo
noProjectInfo = ProjectInfo noBaseInfo noIncomeInfo noExpenseInfo Nothing

--quarter [(3,expenses), (1,expenses)]
--quarter - a list
newtype Quarters a =
    Quarters
        { getQuarters :: [(Int, a)]
        }
    deriving (Show)

instance Combinable a => Combinable (Quarters a) where
    combined = foldl (\acc -> (acc +) . f) 0 . getQuarters
      where
        f x = (fst x) * (combined (snd x))

data QuarterEvents
    = QuarterAdded
    | QuarterDeleted Int
    | QuarterRepeatChanged Int Int -- pos, newRepeat

updateQuarters :: HasEmpty a => QuarterEvents -> Quarters a -> Quarters a
updateQuarters QuarterAdded = addQuarter
updateQuarters (QuarterDeleted quarterPos) = deleteQuarter quarterPos
updateQuarters (QuarterRepeatChanged quarterPos newRepeat) =
    updateQuarterRepeat quarterPos newRepeat

class HasEmpty a where
    empty :: a

instance HasEmpty IncomeInfoQuarter where
    empty = IncomeInfoQuarter []

instance HasEmpty ExpenseInfoQuarter where
    empty = ExpenseInfoQuarter []

instance HasEmpty Product where
    empty = Product (pack "") 0 0 0

addQuarter :: HasEmpty a => Quarters a -> Quarters a
addQuarter (Quarters quarters) = Quarters $ (0, empty) : quarters

deleteQuarter :: Int -> Quarters a -> Quarters a
deleteQuarter quarterPos (Quarters quarters) = Quarters $ deleteAt quarterPos quarters

updateQuarterRepeat :: Int -> Int -> Quarters a -> Quarters a
updateQuarterRepeat quarterPos newRepeat (Quarters quarters) =
    Quarters $ modifyAt quarterPos (\(_, quarterContents) -> (newRepeat, quarterContents)) quarters

data Product =
    Product
        { productName   :: Text
        , sellQuantity  :: Int
        , sellPrice     :: Int
        , producingCost :: Int
        }
    deriving (Show)

instance Combinable Product where
    combined Product {..} = sellQuantity * (sellPrice - producingCost)

data ProductEvents
    = ProductAdded
    | ProductDeleted Int
    | ProductNameChanged Int Text
    | ProductSQuantityChanged Int Int
    | ProductSPriceChanged Int Int
    | ProductProducingCostChanged Int Int

mapIncomeQuarterAt :: Int -> (IncomeInfoQuarter -> IncomeInfoQuarter) -> ProjectInfo -> ProjectInfo
mapIncomeQuarterAt quarterPos f state =
    state {income = Quarters $ modifyAt quarterPos (\(x, y) -> (x, f y)) quarters}
  where
    quarters = getQuarters $ income state -- :: [ (Int, IncomeInfoQuarter) ]

updateIncome :: ProductEvents -> Int -> ProjectInfo -> ProjectInfo
updateIncome events pos = mapIncomeQuarterAt pos (updateProducts events)

updateProducts :: ProductEvents -> IncomeInfoQuarter -> IncomeInfoQuarter
updateProducts ProductAdded = IncomeInfoQuarter . addProduct . getProducts
updateProducts (ProductDeleted pos) = IncomeInfoQuarter . deleteProduct pos . getProducts
updateProducts (ProductNameChanged pos newName) =
    IncomeInfoQuarter . changeProductName pos newName . getProducts
updateProducts (ProductSQuantityChanged pos newValue) =
    IncomeInfoQuarter . changeProductSQ pos newValue . getProducts
updateProducts (ProductSPriceChanged pos newValue) =
    IncomeInfoQuarter . changeProductSP pos newValue . getProducts
updateProducts (ProductProducingCostChanged pos newValue) =
    IncomeInfoQuarter . changeProductPC pos newValue . getProducts

addProduct :: [Product] -> [Product]
addProduct products = empty : products

deleteProduct :: Int -> [Product] -> [Product]
deleteProduct pos products = deleteAt pos products

changeProductName :: Int -> Text -> [Product] -> [Product]
changeProductName pos newName = modifyAt pos (\product -> product {productName = newName})

changeProductSQ :: Int -> Int -> [Product] -> [Product]
changeProductSQ pos newValue = modifyAt pos (\product -> product {sellQuantity = newValue})

changeProductSP :: Int -> Int -> [Product] -> [Product]
changeProductSP pos newValue = modifyAt pos (\product -> product {sellPrice = newValue})

changeProductPC :: Int -> Int -> [Product] -> [Product]
changeProductPC pos newValue = modifyAt pos (\product -> product {producingCost = newValue})

type IncomeInfo = Quarters IncomeInfoQuarter -- TODO make quarter inputs

newtype IncomeInfoQuarter =
    IncomeInfoQuarter
        { getProducts :: [Product]
        }
    deriving (Show)

instance Combinable IncomeInfoQuarter where
    combined (IncomeInfoQuarter products) = foldl (\acc -> (acc +) . combined) 0 products

noIncomeInfoQuarter :: IncomeInfoQuarter
noIncomeInfoQuarter = IncomeInfoQuarter []

noIncomeInfo :: IncomeInfo
noIncomeInfo = Quarters []

combineIncome :: ProjectInfo -> [Int]
combineIncome ProjectInfo {..} = map (combined . snd) $ getQuarters income

type TaxInfo = Maybe Int

data SalaryInfo -- TODO salary
      =
    SalaryInfo
        { salaryInfo :: Int
        }
    deriving (Show)

noSalaryInfo :: SalaryInfo
noSalaryInfo = SalaryInfo 0

instance Combinable SalaryInfo where
    combined SalaryInfo {..} = salaryInfo

type ExpenseInfo = Quarters ExpenseInfoQuarter -- TODO make quarter inputs

noExpenseInfo :: ExpenseInfo
noExpenseInfo = Quarters []

type ValueName = Text

type Value = Int

newtype ExpenseInfoQuarter =
    ExpenseInfoQuarter
        { forms :: [(ValueName, Value)]
        }
    deriving (Show)

instance Combinable ExpenseInfoQuarter where
    combined (ExpenseInfoQuarter xs) = foldl (\acc -> (acc +) . snd) 0 xs
        --materialExpense + (combined salary) + rent + utilities + householdExpense

noExpenseInfoQuarter :: ExpenseInfoQuarter
noExpenseInfoQuarter = ExpenseInfoQuarter []

combineExpenses :: ProjectInfo -> [Int]
combineExpenses ProjectInfo {..} = map (combined . snd) $ getQuarters expenses

-- expenses [(name3, 300), (name2, 200), (name1,100)]
data FormEvents
    = FormAdded
    | FormDeleted Int
    | FormNameChanged Int Text
    | FormValueChanged Int Int

emptyForm :: (ValueName, Value)
emptyForm = (pack "", 0)
                                                    -- TODO change when Quarters added

updateExpenses :: FormEvents -> Int -> ProjectInfo -> ProjectInfo
updateExpenses FormAdded                       = addExpenses
updateExpenses (FormDeleted pos)               = deleteExpenses pos
updateExpenses (FormNameChanged pos newName)   = updateNames pos newName
updateExpenses (FormValueChanged pos newValue) = updateValues pos newValue

mapExpenseQuarterAt ::
       Int -> (ExpenseInfoQuarter -> ExpenseInfoQuarter) -> ProjectInfo -> ProjectInfo
mapExpenseQuarterAt quarterPos f state =
    state {expenses = Quarters $ modifyAt quarterPos (\(x, y) -> (x, f y)) quarters}
  where
    quarters = getQuarters $ expenses state -- :: [ (Int, ExpenseInfoQuarter) ]

addExpenses :: Int -> ProjectInfo -> ProjectInfo
addExpenses quarterPos = mapExpenseQuarterAt quarterPos addForm

deleteExpenses :: Int -> Int -> ProjectInfo -> ProjectInfo
deleteExpenses pos quarterPos = mapExpenseQuarterAt quarterPos $ deleteForm pos

updateNames :: Int -> ValueName -> Int -> ProjectInfo -> ProjectInfo
updateNames pos newName quarterPos = mapExpenseQuarterAt quarterPos $ updateFormName pos newName

updateValues :: Int -> Value -> Int -> ProjectInfo -> ProjectInfo
updateValues pos newValue quarterPos = mapExpenseQuarterAt quarterPos $ updateFormValue pos newValue

addForm :: ExpenseInfoQuarter -> ExpenseInfoQuarter
addForm (ExpenseInfoQuarter xs) = ExpenseInfoQuarter (emptyForm : xs)

deleteForm :: Int -> ExpenseInfoQuarter -> ExpenseInfoQuarter
deleteForm pos (ExpenseInfoQuarter xs) = ExpenseInfoQuarter (deleteAt pos xs)

updateFormName :: Int -> Text -> ExpenseInfoQuarter -> ExpenseInfoQuarter
updateFormName pos newName (ExpenseInfoQuarter xs) =
    ExpenseInfoQuarter (modifyAt pos (changeName newName) xs)
  where
    changeName newName' (_, value) = (newName', value)

updateFormValue :: Int -> Int -> ExpenseInfoQuarter -> ExpenseInfoQuarter
updateFormValue pos newValue (ExpenseInfoQuarter xs) =
    ExpenseInfoQuarter (modifyAt pos (changeValue newValue) xs)
  where
    changeValue newValue' (name, _) = (name, newValue')
