{-# LANGUAGE ScopedTypeVariables #-}
module Data.Generics.Record (
  RecordT,
  isRecord,
  recordT,
  fields,
  emptyRecord,
  recordStructure) where
import Data.Data
import Data.Maybe

-- | A phantom type used to parameterize functions based on records.
-- This let's us avoid passing @undefined@s or manually creating instances
-- all the time. It can only be created for types which are records.
data RecordT a = RecordT

-- | Returns @True@ if @a@ is a data type with a single constructor
-- and is a record.
isRecord :: forall a . Data a => a -> Bool
isRecord _ = isJust (recordT :: Maybe (RecordT a))

-- | The smart constructor for @RecordT@s. This
-- will return a @RecordT@ if and only if the type is a record.
recordT :: forall a. Data a => Maybe (RecordT a)
recordT = if isRecord' ra then Just ra else Nothing
  where ra = (RecordT :: RecordT a)
        
isRecord' :: forall a. Data a => RecordT a -> Bool
isRecord' r = length cs == 1 && length (fields r) > 0
  where cs = dataTypeConstrs . dataTypeOf $ (undefined :: a)

-- | Returns the fields for the record @a@
fields :: forall a. Data a => RecordT a -> [String]
fields _ = fs
  where cs = dataTypeConstrs . dataTypeOf $ (undefined :: a)
        fs = cs >>= constrFields


-- | Return a record where all fields are _|_
emptyRecord :: forall a. Data a => RecordT a -> a
emptyRecord _ = fromConstr . head . dataTypeConstrs . dataTypeOf $ (undefined :: a)

recordStructure :: forall a. Data a => RecordT a -> [(TypeRep, String)]
recordStructure a = zip (gmapQ typeOf (emptyRecord a)) $ fields a
