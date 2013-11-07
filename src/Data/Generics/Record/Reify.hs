{-# LANGUAGE ScopedTypeVariables, Safe #-}
module Data.Generics.Record.Reify (reify, reflect) where
import Data.Generics.Record
import Data.Dynamic
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Control.Monad.State
import Control.Applicative

-- | If @a@ is a record, this will return a @Map@ where the keys
-- are the field names and the values are wrapped in @toDyn@. Otherwise
-- @Nothing@ will be returned.
reify :: forall a. Data a => a -> Maybe (Map String Dynamic)
reify a = fields (RecordT :: RecordT a) >>= return . M.fromList . flip zip (gmapQ toDyn a)

-- | Reflect a @Map@ of strings to an arbitrary type. If the type is a record, each of
-- its field names will be looked up in the record. If any of the types don't match
-- or if @a@ isn't a record, @Nothing@ will be returned.
reflect :: forall a. Data a => Map String Dynamic -> Maybe a
reflect vault = result
  where constrs     = dataTypeConstrs . dataTypeOf $ fromJust result
        fs          = fields (RecordT :: RecordT a) >>= mapM (flip M.lookup vault)
        result      = flip evalStateT fs . gmapM popFields . fromConstr . head $ constrs
        popFields _ = (get >>= lift . (fromDynamic . head =<<)) <* modify (fmap tail)