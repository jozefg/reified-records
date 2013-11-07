{-# LANGUAGE ScopedTypeVariables #-}
module Data.Generics.Record where
import Data.Data
import Data.Maybe
import Data.List
import Control.Monad
import Data.Function

-- | A phantom type used to parameterize functions based on records.
-- This let's us avoid passing @undefined@s or manually creating instances
-- all the time.
data RecordT a = RecordT

-- | Returns the fields for the record @a@
fields :: forall a. Data a => RecordT a -> Maybe [String]
fields _ = if length cs == 1 && length fs > 0 then Just fs else Nothing
  where cs = dataTypeConstrs . dataTypeOf $ (undefined :: a)
        fs = cs >>= constrFields

-- | Returns @True@ if @a@ is a data type with a single constructor
-- and is a record.
isRecord :: forall a. Data a => RecordT a -> Bool
isRecord r = length cs == 1 && maybe False ((>0) . length) fs
  where cs = dataTypeConstrs . dataTypeOf $ (undefined :: a)
        fs = fields r

-- | Return a record where all fields are _|_
emptyRecord :: forall a. Data a => RecordT a -> a
emptyRecord _ = fromConstr . head . dataTypeConstrs . dataTypeOf $ (undefined :: a)
