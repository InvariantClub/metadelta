{-
  Main diff section.
-}

module Hasa.Diff where

import "lens" Control.Lens
import "base" Data.List ( sort, sortBy )
import "base" Data.Maybe ( catMaybes )
import Hasa.Conversion ( convert )
import Hasa.Types.Frontend
import Hasa.Types.Hasura
import "patience" Patience qualified as P


atomDiff :: Old Permissions -> New Permissions -> Permissions
atomDiff = atomDiffByPatience


atomDiffByPatience :: Old Permissions -> New Permissions -> Permissions
atomDiffByPatience (Old o) (New n) = sort $ resolve diff
  where
    o'   = sortBy compareKeyFields o
    n'   = sortBy compareKeyFields n
    diff = P.diff (map ByKeyField o') (map ByKeyField n')
    resolve = map (g . unwrap)
      where
        g (P.New  a)   = ListItemAdded   $ extract a
        g (P.Old  a)   = ListItemRemoved $ extract a
        g (P.Both a b) = ListItemNoOp    $ carefullyCompare (extract a) (extract b)


unwrap :: P.Item (ByKeyField a) -> P.Item a
unwrap (P.New  (ByKeyField a))                = P.New a
unwrap (P.Old  (ByKeyField a))                = P.Old a
unwrap (P.Both (ByKeyField a) (ByKeyField b)) = P.Both a b


newtype ByKeyField a
  = ByKeyField a


instance Eq (ByKeyField (ListItemDiffOp PermissionAtom)) where
  (ByKeyField a) == (ByKeyField b) = byKeyFields a b


instance Ord (ByKeyField (ListItemDiffOp PermissionAtom)) where
  (ByKeyField a) `compare` (ByKeyField b) = compareKeyFields a b


byKeyFields
  :: ListItemDiffOp PermissionAtom
  -> ListItemDiffOp PermissionAtom
  -> Bool
byKeyFields a' b' =
  let (ListItemNoOp a) = a'
      (ListItemNoOp b) = b'
   in
    (a ^. #database) == (b ^. #database)
      &&
    (a ^. #schema) == (b ^. #schema)
      &&
    (a ^. #table) == (b ^. #table)
      &&
    (a ^. #role) == (b ^. #role)
      &&
    (actionName $ extract $ a ^. #action) == (actionName $ extract $ b ^. #action)


compareKeyFields
  :: ListItemDiffOp PermissionAtom
  -> ListItemDiffOp PermissionAtom
  -> Ordering
compareKeyFields (ListItemNoOp a) (ListItemNoOp b) =
  let elt x
        = ( x ^. #database
          , x ^. #schema
          , x ^. #table
          , x ^. #role
          , actionName $ extract $ x ^. #action
          )
   in elt a `compare` elt b
compareKeyFields _ _ = error "Unsupported case hit in `atomDiff`"


-- | We now know there is a difference here. But, what is it? Well, we need to
-- walk over all the fields and compare.
--
-- Note that at this point we need to run 'extract' on everything.
--
-- The result of the 'careful comparision' here is a Diff-like object. The
-- diff is what we would need to do in order to convert 'a' into 'b'.
carefullyCompare
  :: PermissionAtom -- Identity PermissionAtom
  -> PermissionAtom -- Identity PermissionAtom
  -> PermissionAtom -- WithDiff PermissionAtom
carefullyCompare a b =
  PermissionAtom
    { database  = newDatabase
    , role      = newRole
    , schema    = newSchema
    , table     = newTable
    , rowFilter = newRowFilter
    , action    = newAction
    }
  where
    newDatabase  = new (a ^. #database) (b ^. #database)
    newRole      = new (a ^. #role) (b ^. #role)
    newSchema    = new (a ^. #schema) (b ^. #schema)
    newTable     = new (a ^. #table) (b ^. #table)
    newRowFilter = new (a ^. #rowFilter) (b ^. #rowFilter)
    newAction    = carefullyCompareAction (extract $ a ^. #action) (extract $ b ^. #action)


new :: (Eq a) => FieldDiffOp a -> FieldDiffOp a -> FieldDiffOp a
new x' y' =
  let x = extract x'
      y = extract y'
  in if x == y then FieldNoOp y else FieldReplaced (Old x) (New y)


carefullyCompareAction
  :: SomeAction
  -> SomeAction
  -> FieldDiffOp SomeAction
carefullyCompareAction (Select sp) (Select sp') = compareSelect sp sp'
carefullyCompareAction (Insert ip) (Insert ip') = compareInsert ip ip'
carefullyCompareAction (Update up) (Update up') = compareUpdate up up'
carefullyCompareAction Delete Delete            = FieldNoOp Delete
carefullyCompareAction a b                      = FieldReplaced (Old a) (New b)


-- Note: For these, we leave it as a 'FieldNoOp' because the _Action_ isn't
-- changing, and then we compute differents on the _fields_ of these things.
-- This, I hope, will be informative enough.
compareInsert :: InsertProps -> InsertProps -> FieldDiffOp SomeAction
compareInsert a b = if a == b then FieldNoOp (Insert b) else newAction
  where
    newAction :: FieldDiffOp SomeAction
    newAction =
      FieldNoOp $
        Insert $
          InsertProps
            { columns = newColumns
            }
    newColumns = diffConcrete (map extract $ a ^. #columns) (map extract $ b ^. #columns)


compareUpdate :: UpdateProps -> UpdateProps -> FieldDiffOp SomeAction
compareUpdate a b = if a == b then FieldNoOp (Update b) else newAction
  where
    newAction :: FieldDiffOp SomeAction
    newAction =
      FieldNoOp $
        Update $
          UpdateProps
            { postFilter = newPostFilter
            , columns = newColumns
            }
    newPostFilter = new (a ^. #postFilter) (b ^. #postFilter)
    newColumns    = diffConcrete (map extract $ a ^. #columns) (map extract $ b ^. #columns)


compareSelect :: SelectProps -> SelectProps -> FieldDiffOp SomeAction
compareSelect a b = if a == b then FieldNoOp (Select b) else newAction
  where
    newAction :: FieldDiffOp SomeAction
    newAction =
      FieldNoOp $
        Select $
          SelectProps
            { rowLimit = newRowLimit
            , allowAggregation = newAllowAggregation
            , columns = newColumns
            }
    newRowLimit         = new (a ^. #rowLimit) (b ^. #rowLimit)
    newAllowAggregation = new (a ^. #allowAggregation) (b ^. #allowAggregation)
    newColumns          = diffConcrete (map extract $ a ^. #columns) (map extract $ b ^. #columns)


diffConcrete :: (Show a, Eq a, Ord a) => [a] -> [a] -> [ListItemDiffOp a]
diffConcrete as bs = itemToLDiff $ P.diff as bs


itemToLDiff :: [P.Item a] -> [ListItemDiffOp a]
itemToLDiff = map f
  where
    f (P.Old a)    = ListItemRemoved a
    f (P.New a)    = ListItemAdded a
    f (P.Both a _) = ListItemNoOp a


applyFieldDiffOp :: FieldDiffOp a -> FieldDiffOp a
applyFieldDiffOp = \case
  FieldNoOp a             -> FieldNoOp a
  FieldReplaced _ (New a) -> FieldNoOp a


applyListItemDiffOp :: ListItemDiffOp a -> Maybe (ListItemDiffOp a)
applyListItemDiffOp = \case
    ListItemNoOp  a            -> Just $ ListItemNoOp a
    ListItemAdded a            -> Just $ ListItemNoOp a
    ListItemRemoved _          -> Nothing
    ListItemReplaced _ (New a) -> Just $ ListItemNoOp a


applyList :: [ListItemDiffOp a] -> [ListItemDiffOp a]
applyList = catMaybes . map applyListItemDiffOp


applySomeAction :: SomeAction -> SomeAction
applySomeAction = \case
  Delete -> Delete
  Insert (InsertProps cols) ->
    Insert $ InsertProps (applyList cols)
  Update (UpdateProps cols f) ->
    Update $ UpdateProps (applyList cols) (applyFieldDiffOp f)
  Select (SelectProps cols f g) ->
    Select $ SelectProps (applyList cols) (applyFieldDiffOp f) (applyFieldDiffOp g)


applyPermissionAtom :: PermissionAtom -> PermissionAtom
applyPermissionAtom (PermissionAtom {database,table,schema,role,action,rowFilter}) =
  PermissionAtom
    (applyFieldDiffOp database)
    (applyFieldDiffOp role)
    (applyFieldDiffOp schema)
    (applyFieldDiffOp table)
    (FieldNoOp $ applySomeAction (extract $ applyFieldDiffOp action))
    (applyFieldDiffOp rowFilter)


metadataDiff :: Old (Src, MetadataV3) -> New (Src, MetadataV3) -> PermissionData
metadataDiff old' new' =
  let o = convert . snd <$> old'
      n = convert . snd <$> new'
      atoms = atomDiff o n
      mode = Diff (fst <$> old') (fst <$> new')
  in mkPermissionData mode atoms


