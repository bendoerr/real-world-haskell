-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

myInfo = Book 9872368736483 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID String
                  deriving (Show)

data BetterReview = BetterReview BookInfo CustomerID ReviewBody
                    deriving (Show)

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

bookID      (Book id _     _      ) = id
bookTitle   (Book _  title _      ) = title
bookAuthors (Book _  _     authors) = authors

data Customer = Customer {
        customerID      :: CustomerID,
        customerName    :: String,
        customerAddress :: Address
    } deriving (Show)
