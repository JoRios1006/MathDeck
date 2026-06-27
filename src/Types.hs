module Types where

data Polynomial = Polynomial [Rational]
  deriving (Show, Eq)

data RationalFunc = RationalFunc
  { numerator   :: Polynomial
  , denominator :: Polynomial
  } deriving (Show, Eq)

data SignInterval = Positive | Negative
  deriving (Show, Eq)

data SignChart = SignChart
  { roots     :: [Rational]
  , intervals :: [(SignInterval, String)]
  } deriving (Show, Eq)

data EndBehavior = EndBehavior
  { asXPosInf :: String
  , asXNegInf :: String
  } deriving (Show, Eq)

data FunctionAnalysis = FunctionAnalysis
  { polynomial   :: Polynomial
  , derivative   :: Polynomial
  , integral     :: Polynomial
  , signChart    :: SignChart
  , endBehavior  :: EndBehavior
  } deriving (Show, Eq)

data Difficulty = Easy | Medium | Hard | Mixed
  deriving (Show, Eq)
