{-# LANGUAGE ScopedTypeVariables #-}
module Data.Generics.Record.Subtype (
  SubtypeWit,
  genSubtype,
  isSubtype,
  upcast) where
import Data.Data
import Data.List
import Data.Function
import Control.Monad
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe
import Data.Generics.Record.Reify
import Data.Generics.Record

-- | A witness for a subtyping relation between two records so that @ a <: b @
newtype SubtypeWit a b = SubWit {unSubWit :: [(String, String)]}

-- | Returns a witness for a @SubtypeWit@ by traversing
-- the fields of @a@ and @b@ and pairing each field of @a@ with
-- the first one of the same type in @b@.
genSubtype :: forall a b. (Data a, Data b) => RecordT a -> RecordT b -> Maybe (SubtypeWit a b)
genSubtype ra rb = (SubWit . fst) `fmap` foldM findMatch ([], recordStructure rb) (recordStructure ra)
  where findMatch (matches, remaining) (t, n) = do
          n' <- lookup t remaining
          return ((n, n') : matches, deleteBy ((==) `on` fst) (t, "") remaining)

-- | Returns true if @ a <: b @ according to the algorithm for @genSubtype@
isSubtype :: forall a b. (Data a, Data b) => RecordT a -> RecordT b -> Bool
isSubtype ra rb = isJust $ genSubtype ra rb

-- | Upcast a type according to a subtyping witness.
upcast :: forall a b. (Data a, Data b) => SubtypeWit a b -> a -> b
upcast (SubWit fs) a = fromJust $ reify a >>=
                     reflect
                     . flip (foldl' updater) fs
                     . M.filterWithKey (\ k a -> isJust . flip lookup fs $ k)
  where updater m (na, nb)  = M.insert nb (m ! na) $ M.delete na m 
