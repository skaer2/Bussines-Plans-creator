{-# LANGUAGE RecordWildCards #-}

module ProjectInfo where
    --( Id(..)
    --, ProjectInfo(..)
    --, noProjectInfo
    --, BaseInfo(..)
    --, noBaseInfo(..)
    --,
    --) where

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

data ExpenseInfoQuarter =
    ExpenseInfoQuarter
        { materialExpense  :: Int
        , salary           :: SalaryInfo
        , rent             :: Int
        , utilities        :: Int
        , householdExpense :: Int
        }
    deriving (Show)

instance Combinable ExpenseInfoQuarter where
    combined ExpenseInfoQuarter {..} =
        materialExpense + (combined salary) + rent + utilities + householdExpense

noExpenseInfoQuarter :: ExpenseInfoQuarter
noExpenseInfoQuarter = ExpenseInfoQuarter 0 noSalaryInfo 0 0 0

combineExpenses :: ProjectInfo -> [Int]
combineExpenses ProjectInfo {..} = map combined expenses
