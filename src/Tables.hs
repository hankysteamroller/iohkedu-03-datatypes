module Tables (Table(Table), empty, insert, delete, lookup, mapValues, mapKeys, alter, allTableProp, Tables.filter, null) where

import           Prelude hiding (lookup, null)

-- START HERE AFTER reaching the pointer in Datatypes.hs

newtype Table k v = Table [(k, v)]
  deriving (Show)

-- In the following, we first reimplement the functions
-- from the slides, but with the @newtype@-based version
-- of the 'Table' type.

-- Task Tables-1.
--
-- Re-implement 'empty'.

empty :: Table k v
empty = Table []

-- Task Tables-2.
--
-- Re-implement 'insert'.

insert :: k -> v -> Table k v -> Table k v
insert k v (Table kvs) = Table ((k,v):kvs)

-- Task Tables-3.
--
-- Re-implement 'delete'.

delete :: Eq k => k -> Table k v -> Table k v
delete k (Table kvs) = Table (Prelude.filter (\kv -> fst kv /= k) kvs)

-- Task Tables-4.
--
-- Re-implement 'lookup'.

lookup :: Eq k => k -> Table k v -> Maybe v
lookup _ (Table []) = Nothing
lookup k' (Table ((k,v):kvs)) = if (k' == k) then (Just v) else lookup k' (Table kvs)

-- Task Tables-5.
--
-- Implement a map function on the table values.

mapValues :: (v1 -> v2) -> Table k v1 -> Table k v2
mapValues f (Table kvs) = (Table (map (\(k, v) -> (k, f v)) kvs))

-- Task Tables-6.
--
-- Implement a map function on the table keys.
--
-- Tricky additional question:
-- Can you identify a potential problem with
-- this function?

mapKeys :: (k1 -> k2) -> Table k1 v -> Table k2 v
mapKeys f (Table kvs) = (Table (map (\(k, v) -> (f k, v)) kvs))

-- Task Tables-7.
--
-- Implement a more general table update function.
-- The function 'alter' takes a function and a key.

alter :: Eq k => (Maybe v -> Maybe v) -> k -> Table k v -> Table k v
alter f k t =
  let
    vBefore = lookup k t
    vAfter = f vBefore
  in
    alter' k vBefore vAfter t

alter' :: Eq k => k -> Maybe v -> Maybe v -> Table k v -> Table k v
alter' _ Nothing Nothing t  = t
alter' k Nothing (Just v) t = insert k v t
alter' k (Just _) Nothing t = delete k t
alter' k _ (Just v) t       = modify k v t

modify :: Eq k => k -> v -> Table k v -> Table k v
modify k v (Table kvs) = (Table (map (\(k', v') -> if (k == k') then (k, v) else (k', v')) kvs))

-- Task Tables-8.
--
-- Add an export list to the module, exporting
-- all the functions, and the 'Table' type, but
-- no constructors. The syntax
--
--   Table()
--
-- can be used in the export list to export a
-- datatype or newtype without any of its
-- constructors.

-- GO TO Transactions.hs

allTableProp :: ((k, v) -> Bool) -> Table k v -> Bool
allTableProp f (Table kvs) = all f kvs

filter :: (v -> Bool) -> Table k v -> Table k v
filter _ (Table [])  = Table ([])
filter f (Table kvs) = Table ([(k, v) | (k, v) <- kvs, f v])

null :: Table k v -> Bool
null (Table []) = True
null _          = False
