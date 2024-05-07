-- | The Hasura model; i.e. how to read their JSON format.
module Hasa.Types.Hasura where

import "aeson" Data.Aeson
import "text" Data.Text ( Text )
import "base" GHC.Generics ( Generic )


data MetadataFile
  = MetadataFile
      { metadata :: MetadataV3
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data MetadataV3
  = MetadataV3
      { sources :: [Source]
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data Source
  = Source
      { kind   :: Text
      , name   :: Text
      , tables :: [Table]
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data Table
  = Table
      { table              :: TableInfo
      , select_permissions :: Maybe [SelectPermission]
      , insert_permissions :: Maybe [InsertPermission]
      , update_permissions :: Maybe [UpdatePermission]
      , delete_permissions :: Maybe [DeletePermission]
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data TableInfo
  = TableInfo
      { name   :: Text
      , schema :: Text
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data DeletePermission
  = DeletePermission
      { role       :: Text
      , permission :: DeletePermissionDetail
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data SelectPermission
  = SelectPermission
      { role       :: Text
      , permission :: SelectPermissionDetail
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data InsertPermission
  = InsertPermission
      { role       :: Text
      , permission :: InsertPermissionDetail
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data UpdatePermission
  = UpdatePermission
      { role       :: Text
      , permission :: UpdatePermissionDetail
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data DeletePermissionDetail
  = DeletePermissionDetail
      { filter :: Value
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data InsertPermissionDetail
  = InsertPermissionDetail
      { columns :: [Text]
      , check   :: Value
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data SelectPermissionDetail
  = SelectPermissionDetail
      { columns            :: [Text]
      , filter             :: Value
      , limit              :: Maybe Int
      , allow_aggregations :: Maybe Bool
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)


data UpdatePermissionDetail
  = UpdatePermissionDetail
      { columns :: [Text]
      , filter  :: Value
      , check   :: Value
      }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)
