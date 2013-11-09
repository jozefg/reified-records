{-# LANGUAGE ScopedTypeVariables, Safe #-}
module Data.Generics.Record (
  RecordT,
  isRecord,
  recordT,
  fields,
  emptyRecord,
  recordStructure) where
import Data.Data
import Data.Maybe
import Control.Monad (guard)
-- | A phantom type used to parameterize functions based on records.
-- This let's us avoid passing @undefined@s or manually creating instances
-- all the time. It can only be created for types which are records and
-- is used as a token to most of the API's functions.
data RecordT a = RecordT { constr :: Constr
                         , fs     :: [String]}

-- | Returns @True@ if @a@ is a data type with a single constructor
-- and is a record. @a@ may be bottom.
isRecord :: forall a . Data a => a -> Bool
isRecord _ = isJust (recordT :: Maybe (RecordT a))

-- | The smart constructor for @RecordT@s. This
-- will return a @RecordT@ if and only if the type is a record.
recordT :: forall a. Data a => Maybe (RecordT a)
recordT = recordTAt 0

-- | A constructor for @RecordT@s that selects the @i@th constructor
-- from the left. This starts from 0.
recordTAt :: forall a. Data a => Int -> Maybe (RecordT a)
recordTAt i = guard (length cs > i && length (fields r) > 0) >> Just r
  where cs = dataTypeConstrs . dataTypeOf $ (undefined :: a)
        r = RecordT (cs !! i) (constrFields $ cs !! i) :: RecordT a

-- | Returns the fields for the record @a@
fields :: forall a. Data a => RecordT a -> [String]
fields = fs


-- | Return a record where all fields are _|_
emptyRecord :: forall a. Data a => RecordT a -> a
emptyRecord = fromConstr . constr

-- | Return a records structure of as a list of types paired with field names.
recordStructure :: forall a. Data a => RecordT a -> [(TypeRep, String)]
recordStructure a = zip (gmapQ typeOf (emptyRecord a)) $ fields a
