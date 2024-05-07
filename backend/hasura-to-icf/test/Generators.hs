{-# OPTIONS_GHC -fno-warn-orphans #-}

module Generators where

import "lens" Control.Lens hiding ( op )
import "generic-lens" Data.Generics.Labels ()
import "base" Data.List ( nub, nubBy )
import "text" Data.Text ( singleton )
import "hasura-to-icf" Hasa.Types.Frontend
import "QuickCheck" Test.QuickCheck

-- <https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html>
-- quickcheck basics


mkFlexList :: Gen a -> Gen [a]
mkFlexList a = sized $ \n ->
  frequency
    [ (1, return [])
    , (n, (:) <$> a <*> mkFlexList a)
    ]


flexList :: Arbitrary a => Gen [a]
flexList = mkFlexList arbitrary


newtype Unchanged a
  = Unchanged a
  deriving (Show)


instance Arbitrary (Unchanged PermissionAtom) where
  arbitrary = Unchanged <$> genUnchangedPermissionAtom


instance Arbitrary (Unchanged SomeAction) where
  arbitrary = Unchanged <$> genUnchangedAction


instance Arbitrary a => Arbitrary (FieldDiffOp a) where
  arbitrary = oneof [ FieldNoOp     <$> arbitrary
                    , FieldReplaced <$> (Old <$> arbitrary) <*> (New <$> arbitrary)
                    ]


genUnchangedFieldDiffOp :: Arbitrary a => Gen (FieldDiffOp a)
genUnchangedFieldDiffOp = FieldNoOp <$> arbitrary


genUnchangedListItemDiffOp :: Arbitrary a => Gen (ListItemDiffOp a)
genUnchangedListItemDiffOp = ListItemNoOp <$> arbitrary


instance Arbitrary Filter where
  arbitrary = Filter <$> cond
    where
      -- TODO: This could be more interesting.
      cond = pure "{ 'x': 2 }"


-- | Columns will be lower-case letters.
instance Arbitrary Column where
  arbitrary = oneof $ map (pure . Column . singleton) $ [ 'a' .. 'z' ]


-- | Roles will be upper-case letters.
instance Arbitrary Role where
  arbitrary = oneof $ map (pure . Role . singleton) $ [ 'A' .. 'Z' ]


instance Arbitrary Schema where
  arbitrary = oneof $ map (pure . Schema) $ [ "public", "shmubplic" ]


instance Arbitrary Database where
  arbitrary = oneof $ map (pure . Database) $ [ "default", "schmefault" ]


-- | Tables will be any two lower-case letters.
instance Arbitrary Table where
  arbitrary = oneof $ map (pure . Table) twoLetters
    where
      chars = [ 'a' .. 'z' ]
      twoLetters = [ singleton x <> singleton y | x <- chars, y <- chars ]



instance Arbitrary InsertProps where
  arbitrary = InsertProps <$> genColumns


instance Arbitrary UpdateProps where
  arbitrary = UpdateProps <$> genColumns <*> arbitrary


instance Arbitrary SelectProps where
  arbitrary = SelectProps <$> genColumns <*> arbitrary <*> arbitrary


instance Arbitrary SomeAction where
  arbitrary = oneof [ Insert <$> arbitrary
                    , Update <$> arbitrary
                    , Select <$> arbitrary
                    , pure Delete
                    ]


instance Arbitrary PermissionAtom where
  arbitrary
    = PermissionAtom
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary


-- | We have a newtype for a list of permissions because we want to drop ones
-- that are duplicated with the same (table, role, action) triple. We do this
-- with the arbitrary instance below.
newtype SomePermissionAtoms
  = SomePermissionAtoms [ListItemDiffOp PermissionAtom]
  deriving (Show)


instance Arbitrary SomePermissionAtoms where
  arbitrary = SomePermissionAtoms <$> atoms
    where
      -- Extract the table, role, and action name
      tup p = (p ^. #table, p ^. #role, actionName $ extract $ (p ^. #action))

      -- True if the above tuples are equal
      f :: ListItemDiffOp PermissionAtom -> ListItemDiffOp PermissionAtom -> Bool
      f p1 p2 = tup (extract p1) == tup (extract p2)

      -- Don't have any duplicate role, table, action atoms.
      atoms :: Gen [ListItemDiffOp PermissionAtom]
      atoms = nubBy f <$> mkFlexList mk

      -- Note: No 'Replaced' here, as we never want that, because we never do
      -- a pure-replace on a PermissionAtom, we always look deeper.
      mk = oneof [ ListItemNoOp    <$> arbitrary
                 , ListItemAdded   <$> genUnchangedPermissionAtom
                 , ListItemRemoved <$> genUnchangedPermissionAtom
                 ]


genUnchangedPermissionAtom :: Gen PermissionAtom
genUnchangedPermissionAtom
    = barePermissionAtom
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> genUnchangedAction
    <*> arbitrary


genUnchangedAction :: Gen SomeAction
genUnchangedAction =
  oneof [ pure Delete
        , Insert <$> (InsertProps <$> mkFlexList genUnchangedListItemDiffOp)
        , Update <$> (UpdateProps <$> mkFlexList genUnchangedListItemDiffOp
                                  <*> genUnchangedFieldDiffOp
                     )
        , Select <$> (SelectProps <$> mkFlexList genUnchangedListItemDiffOp
                                  <*> genUnchangedFieldDiffOp
                                  <*> genUnchangedFieldDiffOp
                     )
        ]


genColumns :: Gen [ListItemDiffOp Column]
genColumns =
  let cols :: Gen [Column]
      cols = nub <$> flexList
      op c = oneof $ map pure [ ListItemNoOp     c
                              , ListItemAdded    c
                              , ListItemRemoved  c
                              , ListItemReplaced (Old $ "old-" <> c) (New $ "new-" <> c)
                              ]
   in cols >>= mapM op
