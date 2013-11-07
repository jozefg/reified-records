{-# LANGUAGE DeriveDataTypeable #-}
module Data.Generics.Reify.Record (reify, reflect) where
import Data.Dynamic
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Control.Monad.State
import Control.Applicative

fields :: Data a => a -> Maybe [String]
fields a = if length cs == 1 && length fs > 0 then Just fs else Nothing
  where cs = dataTypeConstrs . dataTypeOf $ a
        fs = cs >>= constrFields

reify :: Data a => Map String Dynamic -> Maybe a
reify vault = result
  where constrs     = dataTypeConstrs . dataTypeOf $ fromJust result
        fs          = fields (fromJust result) >>= mapM (flip M.lookup vault)
        result      = flip evalStateT fs . gmapM popFields . fromConstr . head $ constrs
        popFields _ = (get >>= lift . (fromDynamic . head =<<)) <* modify (fmap tail)

reflect :: Data a => a -> Maybe (Map String Dynamic)
reflect a = fields a >>= return . M.fromList . flip zip (gmapQ toDyn a)
