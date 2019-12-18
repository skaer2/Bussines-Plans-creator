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
    | Income
    | Expense

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

type IncomeInfo = [Int] -- TODO make quarter inputs

noIncomeInfo :: IncomeInfo
noIncomeInfo = replicate 4 0

combineIncome :: ProjectInfo -> [Int]
combineIncome ProjectInfo {..} = income

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

type ExpenseInfo = [ExpenseInfoQuarter] -- TODO make quarter inputs

noExpenseInfo :: ExpenseInfo
noExpenseInfo = replicate 1 noExpenseInfoQuarter

type ValueName = Text

type Value = Int

newtype ExpenseInfoQuarter =
    ExpenseInfoQuarter { forms :: [(ValueName, Value)]}
    deriving (Show)

instance Combinable ExpenseInfoQuarter where
    combined (ExpenseInfoQuarter xs) = foldl (\acc -> (acc +) . snd) 0 xs
        --materialExpense + (combined salary) + rent + utilities + householdExpense

noExpenseInfoQuarter :: ExpenseInfoQuarter
noExpenseInfoQuarter = ExpenseInfoQuarter []

combineExpenses :: ProjectInfo -> [Int]
combineExpenses ProjectInfo {..} = map combined expenses

-- expenses [(name3, 300), (name2, 200), (name1,100)]
data FormEvents
    = FormAdded
    | FormDeleted Int
    | FormNameChanged Int Text
    | FormValueChanged Int Int

emptyForm :: (ValueName, Value)
emptyForm = (pack "", 0)
                                                    -- TODO change when Quarters added

updateExpenses :: FormEvents -> ProjectInfo -> ProjectInfo
updateExpenses FormAdded                       = addExpenses
updateExpenses (FormDeleted pos)               = deleteExpenses pos
updateExpenses (FormNameChanged pos newName)   = updateNames pos newName
updateExpenses (FormValueChanged pos newValue) = updateValues pos newValue

mapExpenses :: (ExpenseInfo -> ExpenseInfo) -> ProjectInfo -> ProjectInfo
mapExpenses f state = state {expenses = f (expenses state)}

addExpenses :: ProjectInfo -> ProjectInfo
addExpenses = mapExpenses $ map addForm

deleteExpenses :: Int -> ProjectInfo -> ProjectInfo
deleteExpenses pos = mapExpenses $ map (deleteForm pos)

updateNames :: Int -> ValueName -> ProjectInfo -> ProjectInfo
updateNames pos newName = mapExpenses $ map (updateFormName pos newName)

updateValues :: Int -> Value -> ProjectInfo -> ProjectInfo
updateValues pos newValue = mapExpenses $ map (updateFormValue pos newValue)

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
