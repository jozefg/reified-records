module Data.Generics.Record.Subtype where

-- | A witness for a subtyping relation between two records so that @ a <: b @
newtype SubtypeWit a b = SubWit {unSubWit :: [(String, String)]}

-- | Returns a witness for a @SubtypeWit@ by traversing
-- the fields of @a@ and @b@ and pairing each field of @a@ with
-- the first one of the same type in @b@.
genSubtype :: forall a b. (Data a, Data b) => RecordT a -> RecordT b -> Maybe (SubtypeWit a b)
genSubtype ra rb = do
  as <- fsA
  bs <- fsB
  sub<- fst `fmap` foldM findMatch ([], bs) as
  return $ SubWit sub
  where fsA = zip (gmapQ typeOf (emptyRecord ra)) `fmap` fields ra
        fsB = zip (gmapQ typeOf (emptyRecord rb)) `fmap` fields rb
        findMatch (matches, remaining) (t, n) = do
          n' <- lookup t remaining
          return ((n, n') : matches, deleteBy ((==) `on` fst) (t, "") remaining)

-- | Returns true if @ a <: b @ according to the algorithm for @genSubtype@
isSubtype :: forall a b. (Data a, Data b) => RecordT a -> RecordT b -> Bool
isSubtype ra rb = isJust $ genSubtype ra rb
