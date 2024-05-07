{-# language LambdaCase #-}

-- | The front-end model as implemented in Elm.
module Hasa.Types.Frontend where

import "lens" Control.Lens
import "aeson" Data.Aeson
import "generic-lens" Data.Generics.Labels ()
import "base" Data.List ( foldl' )
import "base" Data.String ( IsString )
import "text" Data.Text ( Text )
import "base" GHC.Generics ( Generic )

anythingChanged :: PermissionData -> Bool
anythingChanged pd = foldl' (||) False (map changed (items pd))

-- TODO: All of this could be replaced by a sufficiently-interesting/powerful
-- application of some kind of barbie thing.

isAtomChanged :: ListItemDiffOp PermissionAtom -> Bool
isAtomChanged (ListItemNoOp a') = g a'
  where
  g a =  isChangedF (a ^. #database)
      || isChangedF (a ^. #role)
      || isChangedF (a ^. #schema)
      || isChangedF (a ^. #table)
      || isActionChanged (a ^. #action)
      || isChangedF (a ^. #rowFilter)
isAtomChanged _ = True

isChangedL :: ListItemDiffOp a -> Bool
isChangedL (ListItemNoOp _) = False
isChangedL _                = True

isChangedF :: FieldDiffOp a -> Bool
isChangedF (FieldNoOp _) = False
isChangedF _             = True

isActionChanged :: FieldDiffOp SomeAction -> Bool
isActionChanged (FieldNoOp a) = g a
  where
    g Delete     = False
    g (Select s) = isSelectChanged s
    g (Insert i) = isInsertChanged i
    g (Update u) = isUpdateChanged u
isActionChanged _ = True

isSelectChanged :: SelectProps -> Bool
isSelectChanged p
  =  isChangedF (p ^. #rowLimit)
  || isChangedF (p ^. #allowAggregation)
  || cols
  where
    cols = foldl' (||) False (map isChangedL (p ^. #columns))

isInsertChanged :: InsertProps -> Bool
isInsertChanged p = foldl' (||) False (map isChangedL (p ^. #columns))

isUpdateChanged :: UpdateProps -> Bool
isUpdateChanged p
  =  isChangedF (p ^. #postFilter)
  || cols
  where
    cols = foldl' (||) False (map isChangedL (p ^. #columns))


-- | Handy alias; we only want to diff permissions; this keeps the list
-- wrapped in the diff operation.
type Permissions = [ListItemDiffOp PermissionAtom]

-- TODO: Make some lenses to make all this accessing a bit nicer?

data ListItemDiffOp a
  = ListItemNoOp a
  | ListItemAdded a
  | ListItemRemoved a
  | ListItemReplaced (Old a) (New a)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


class Extract f where
  extract :: f a -> a


instance Extract ListItemDiffOp where
  extract = \case
    ListItemNoOp a -> a
    ListItemAdded a -> a
    ListItemRemoved a -> a
    ListItemReplaced _ (New a) -> a


instance Extract FieldDiffOp where
  extract = extractFieldDiffOp


data FieldDiffOp a
  = FieldNoOp a
  | FieldReplaced (Old a) (New a)
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


instance Functor FieldDiffOp where
  fmap f (FieldNoOp a)       = FieldNoOp (f a)
  fmap f (FieldReplaced a b) = FieldReplaced (f <$> a) (f <$> b)


extractFieldDiffOp :: FieldDiffOp a -> a
extractFieldDiffOp = \case
  FieldNoOp a -> a
  FieldReplaced _ (New a) -> a


newtype Old a
  = Old { runOld :: a }
  deriving stock (Eq, Functor, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype New a
  = New { runNew :: a }
  deriving stock (Eq, Functor, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


data Mode
  = Diff (Old Src) (New Src)
  | SingleSource Src
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


data Src
  = Src
      { label :: Text
      , link  :: Maybe Text
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


data PermissionData
  = PermissionData
      { version :: Int
      , items   :: [PermissionWithMetadata]
      , mode    :: Mode
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


data Filter
  = Filter
      { cond :: Text
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


barePermissionAtom
  :: Database
  -> Role
  -> Schema
  -> Table
  -> SomeAction
  -> Maybe Filter
  -> PermissionAtom
barePermissionAtom d r s t a f =
  PermissionAtom
    { database  = FieldNoOp d
    , role      = FieldNoOp r
    , schema    = FieldNoOp s
    , table     = FieldNoOp t
    , action    = FieldNoOp a
    , rowFilter = FieldNoOp f
    }


-- | When it comes time to write out the JSON for the frontend
data PermissionWithMetadata
  = PermissionWithMetadata
      { permission :: ListItemDiffOp PermissionAtom
      , changed    :: Bool
      , ref        :: Int
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


mkPermissionData :: Mode -> Permissions -> PermissionData
mkPermissionData mode ps =
  let f i p = PermissionWithMetadata
            { permission = p
            , changed = isAtomChanged p
            , ref = i
            }
   in PermissionData
      { version = 3
      , items   = zipWith f [1..] ps
      , mode    = mode
      }

newtype Role
  = Role Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype (IsString, Semigroup)


newtype Table
  = Table Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype (IsString, Semigroup)


newtype Schema
  = Schema Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype (IsString, Semigroup)


newtype Column
  = Column Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype (IsString, Semigroup)


newtype Database
  = Database Text
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
  deriving newtype (IsString, Semigroup)


data PermissionAtom
  = PermissionAtom
      { database  :: FieldDiffOp Database
      , role      :: FieldDiffOp Role
      , schema    :: FieldDiffOp Schema
      , table     :: FieldDiffOp Table
      , action    :: FieldDiffOp SomeAction
      , rowFilter :: FieldDiffOp (Maybe Filter)
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


data SomeAction
  = Select SelectProps
  | Insert InsertProps
  | Update UpdateProps
  | Delete
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


actionName :: SomeAction -> String
actionName = \case
  Delete   -> "delete"
  Insert _ -> "insert"
  Select _ -> "select"
  Update _ -> "update"


bareSelectProps
  :: [Text]
  -> Maybe Int
  -> Bool
  -> SelectProps
bareSelectProps c r a =
  SelectProps
    (map (ListItemNoOp . Column) c)
    (FieldNoOp r)
    (FieldNoOp a)


data SelectProps
  = SelectProps
      { columns          :: [ListItemDiffOp Column]
      , rowLimit         :: FieldDiffOp (Maybe Int)
      , allowAggregation :: FieldDiffOp Bool
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


bareUpdateProps
  :: [Text]
  -> Maybe Filter
  -> UpdateProps
bareUpdateProps c p =
  UpdateProps
    (map (ListItemNoOp . Column) c)
    (FieldNoOp p)


data UpdateProps
  = UpdateProps
      { columns    :: [ListItemDiffOp Column]
      , postFilter :: FieldDiffOp (Maybe Filter)
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)


bareInsertProps :: [Text] -> InsertProps
bareInsertProps c =
  InsertProps (map (ListItemNoOp . Column) c)


data InsertProps
  = InsertProps
      { columns :: [ListItemDiffOp Column]
      }
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (FromJSON, ToJSON)
