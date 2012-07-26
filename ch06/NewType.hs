data DataInt = D Int
    deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)

newtype UniqueID = UniqueID Int
    deriving (Eq)
