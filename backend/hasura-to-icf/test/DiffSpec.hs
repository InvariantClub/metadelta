{-# language TupleSections #-}
module DiffSpec where

import "base" Data.List ( sort )
import Generators
import "hasura-to-icf" Hasa.Diff
  ( applyFieldDiffOp
  , applyList
  , applyPermissionAtom
  , applySomeAction
  , atomDiff
  , carefullyCompare
  , carefullyCompareAction
  , diffConcrete
  )
import "hasura-to-icf" Hasa.Types.Frontend
import "hspec" Test.Hspec
import "QuickCheck" Test.QuickCheck

main :: IO ()
main = hspec spec

(<?>) :: (Testable p) => p -> String -> Property
(<?>) = flip (Test.QuickCheck.counterexample . ("Notes: " ++))
infixl 2 <?>

-- | Check that the 'concrete' list apply resolves to the right thing (not
-- even using any of our types; just integers.)
prop_simpleListApplyWorks :: [Int] -> [Int] -> Bool
prop_simpleListApplyWorks as bs = prop
  where
    ds   = diffConcrete as bs
    cs   = applyList ds
    prop = bs == (map extract cs)

-- | Check that, given two Actions, a and b, then the diff of a b to applied to
-- a gives b.
prop_actionApplyWorks :: Unchanged SomeAction -> Unchanged SomeAction -> Property
prop_actionApplyWorks (Unchanged a) (Unchanged b)
  = collect prefixes $ prop <?> show c
  where
    c'   = extract . applyFieldDiffOp $ carefullyCompareAction a b
    c    = applySomeAction c'
    prop = c == b
    prefixes = actionName a ++ "-" ++ actionName b

-- | Same thing but for atoms.
prop_atomApplyWorks :: Unchanged PermissionAtom -> Unchanged PermissionAtom -> Property
prop_atomApplyWorks (Unchanged a) (Unchanged b)
  = prop <?> show c
  where
    c    = applyPermissionAtom $ carefullyCompare a b
    prop = b == c

-- | Our main test: Given two lists of atoms, as and bs, does the difference of
-- as to bs, applied to as, give bs?
prop_atomDiffWorks :: [Unchanged PermissionAtom] -> [Unchanged PermissionAtom] -> Property
prop_atomDiffWorks as bs = prop <?> "cs = " ++ show cs ++ "; diff = " <> show diff
  where
    ex (Unchanged a) = ListItemNoOp a
    old  = Old (map ex as)
    new  = New (map ex bs)
    diff = atomDiff old new
    cs   = map (ListItemNoOp . applyPermissionAtom . extract) $ applyList diff
    prop = sort cs == sort (map ex bs)


-- TODO: Add a test for the specific case of table-ish renamings; i.e. the two
-- lists are almost the same save some additional things and changes to the
-- non-name structure of the tables.

spec :: Spec
spec = do
  describe "atoms" $ do
    it "diffs lists" $ property $ prop_atomDiffWorks
    it "applies individually" $ property $ prop_atomApplyWorks
  describe "actions" $ do
    it "applies individually" $ property $ prop_actionApplyWorks
  describe "lists" $ do
    it "applies lists correctly" $ property $ prop_simpleListApplyWorks
