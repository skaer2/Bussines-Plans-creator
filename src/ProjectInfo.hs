{-# LANGUAGE RecordWildCards #-}

module ProjectInfo where
    --( Id(..)
    --, ProjectInfo(..)
    --, noProjectInfo
    --, BaseInfo(..)
    --, noBaseInfo(..)
    --,
    --) where

import           Data.Text (Text)
import           Data.Time

class Combinable a where
    combined :: a -> Int

data Id
    = StartDate
    | Duration
    | Income

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

type IncomeInfo = [Int]

noIncomeInfo :: IncomeInfo
noIncomeInfo = replicate 4 0

combineIncome :: ProjectInfo -> [Int]
combineIncome ProjectInfo {..} = income

type TaxInfo = Maybe Int

data SalaryInfo =
    SalaryInfo
        { salaryInfo :: Int
        }
    deriving (Show)

noSalaryInfo :: SalaryInfo
noSalaryInfo = SalaryInfo 0

instance Combinable SalaryInfo where
    combined SalaryInfo {..} = salaryInfo

type ExpenseInfo = [ExpenseInfoQuarter]

noExpenseInfo :: ExpenseInfo
noExpenseInfo = replicate 4 noExpenseInfoQuarter

type ValueName = Text

type Value = Int

data ExpenseInfoQuarter =
    ExpenseInfoQuarter [(ValueName, Value)]
    deriving (Show)

instance Combinable ExpenseInfoQuarter where
    combined (ExpenseInfoQuarter xs) = foldl (\acc -> (acc +) . snd) 0 xs
        --materialExpense + (combined salary) + rent + utilities + householdExpense

noExpenseInfoQuarter :: ExpenseInfoQuarter
noExpenseInfoQuarter = ExpenseInfoQuarter []

combineExpenses :: ProjectInfo -> [Int]
combineExpenses ProjectInfo {..} = map combined expenses
