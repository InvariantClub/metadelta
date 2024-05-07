{-# language DuplicateRecordFields #-}
{-# language NoMonoLocalBinds      #-}

module Hasa.Conversion where

import "lens" Control.Lens
import "aeson" Data.Aeson
import "bytestring" Data.ByteString qualified as B
import "bytestring" Data.ByteString.Lazy qualified as BL
import "generic-lens" Data.Generics.Labels ()
import "base" Data.Maybe ( fromMaybe )
import "text" Data.Text ( Text )
import "text" Data.Text.Encoding qualified as T

import Hasa.Types.Frontend
import Hasa.Types.Hasura hiding ( Table )
import Hasa.Types.Hasura qualified as Hasura


toText :: BL.ByteString -> Text
toText = T.decodeUtf8 . B.concat . BL.toChunks


toSingleSource :: Permissions -> Src -> PermissionData
toSingleSource atoms src
  = mkPermissionData (SingleSource src) atoms


convert :: MetadataV3 -> Permissions
convert m = atoms
  where
    s :: [PermissionAtom]
    s = concatMap fromSource (m ^. #sources)

    fromSource :: Source -> [PermissionAtom]
    fromSource s' = concatMap (fromTable (s' ^. #name)) (s' ^. #tables)

    fromTable :: Text -> Hasura.Table -> [PermissionAtom]
    fromTable sourceName t =
      let se = fromMaybe [] $ t ^. #select_permissions
          is = fromMaybe [] $ t ^. #insert_permissions
          up = fromMaybe [] $ t ^. #update_permissions
          de = fromMaybe [] $ t ^. #delete_permissions
          g f xs = map (f db (t ^. #table)) xs
          db = Database sourceName
      in concat $
          [ g fromSelect se
          , g fromInsert is
          , g fromUpdate up
          , g fromDelete de
          ]
    atoms = map ListItemNoOp s


fromSelect :: Database -> TableInfo -> SelectPermission -> PermissionAtom
fromSelect db t s =
  barePermissionAtom
    db
    (Role (s ^. #role))
    (Schema (t ^. #schema))
    (Table (t ^. #name))
    action
    rowFilter
  where
    action = Select props
    props =
      bareSelectProps
        (s ^. #permission ^. #columns)
        (s ^. #permission ^. #limit)
        (fromMaybe False $ s ^. #permission ^. #allow_aggregations)
    rowFilter = getFilter (s ^. #permission ^. #filter)


fromInsert :: Database -> TableInfo -> InsertPermission -> PermissionAtom
fromInsert db t s =
  barePermissionAtom
    db
    (Role (s ^. #role))
    (Schema (t ^. #schema))
    (Table (t ^. #name))
    action
    rowFilter
  where
    action = Insert props
    props = bareInsertProps (s ^. #permission ^. #columns)
    rowFilter = getFilter (s ^. #permission ^. #check)


fromDelete :: Database -> TableInfo -> DeletePermission -> PermissionAtom
fromDelete db t s =
  barePermissionAtom
    db
    (Role (s ^. #role))
    (Schema (t ^. #schema))
    (Table (t ^. #name))
    action
    rowFilter
  where
    action = Delete
    rowFilter = getFilter (s ^. #permission ^. #filter)


fromUpdate :: Database -> TableInfo -> UpdatePermission -> PermissionAtom
fromUpdate db t s =
  barePermissionAtom
    db
    (Role (s ^. #role))
    (Schema (t ^. #schema))
    (Table (t ^. #name))
    action
    rowFilter
  where
    action = Update props
    props = bareUpdateProps (s ^. #permission ^. #columns) postFilter
    postFilter = getFilter (s ^. #permission ^. #check)
    rowFilter = getFilter (s ^. #permission ^. #filter)


getFilter :: Value -> (Maybe Filter)
getFilter Null = Nothing
getFilter v@(Object o) =
  if null o
    then Nothing
    else Just . Filter . toText . encode $ v
getFilter f = error $ "Invalid filter: " ++ show f
