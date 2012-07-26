-- ok: any number of fields and constructors
data TwoFields = TwoFields Int Int

-- ok: just one field
newtype Okay = ExactlyOne Int

-- ok: type params are no problem
newtype Param a b = Param (Either a b)

-- ok: record syntax is fine
newtype Record = Record {
    getInt :: Int
}

-- bad: no fields
-- newtype TooFew = TooFew

-- bad: more than one field
-- newtype TooMany = TooMany Int Int

-- bad: too many constructors
-- newtype TooManyConstructors = This
--                             | That
