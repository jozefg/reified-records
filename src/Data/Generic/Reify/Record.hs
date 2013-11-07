{-# LANGUAGE DeriveDataTypeable #-}
import Data.Dynamic
import Data.Typeable
import Data.Data
import qualified Data.Map as M
import Data.Map (Map)
import Data.Maybe
import Control.Monad.State
import Control.Applicative

reify :: Data a => Map String Dynamic -> Maybe a
reify vault | length (concatMap constrFields constrs) > 0 && length constrs == 1 = result
            | otherwise = Nothing
  where constrs     = dataTypeConstrs . dataTypeOf $ fromJust result
        fields      = map (flip M.lookup vault) $ constrs >>= constrFields
        result      = flip evalStateT fields . gmapM popFields . fromConstr . head $ constrs
        popFields _ = (get >>= lift . head >>= lift . fromDynamic) <* modify tail


-- | Example
data Test = Test { foo :: Integer, bar :: String} deriving(Data, Typeable, Show)
m = M.fromList [("bar", toDyn "bazfoo"), ("foo", toDyn (1 :: Integer))]
t = reify m :: Maybe Test
