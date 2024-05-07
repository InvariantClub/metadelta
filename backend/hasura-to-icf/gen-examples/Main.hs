module Main where

import "base" Control.Monad ( forM_ )
import "aeson" Data.Aeson ( encode )
import "text" Data.Text.IO qualified as T
import Hasa.Conversion ( toSingleSource, toText )
import Hasa.Diff ( atomDiff )
import Hasa.Types.Frontend

allExamples :: [(([PermissionAtom], [PermissionAtom]), String)]
allExamples
  = [ (exRoleChange,            "role-change")
    , (exAdded,                 "permission-added")
    , (exColumns1,              "renamed-column")
    , (exColumns2,              "new-columns")
    , (exColumnsAndFilter,      "column-and-filter-change")
    , (exFilterDiff,            "simply-filter-change")
    , (exFilterAndAction,       "simply-filter-and-action-change")
    , (exTableNameChange,       "table-name-change")
    , (exActionChange1,         "change-action-1")
    , (exActionChange2,         "change-action-2")
    , (exRowFilter,             "add-row-filter")
    , (nothingAndSomething,     "two-things-add-select")
    , (rowLimitAndAggregations, "row-limit-and-aggregations")
    , (someBbbExample,          "some-bbb-example-115-to-114")
    , (justAddOne,              "just-add-one-permission")
    , (old2New1,                "old-2-new-1")
    , (exDemoDatabase,          "demo-database-issue")
    ]


a :: Role -> Table -> SomeAction -> Maybe Filter -> PermissionAtom
a r t s f = barePermissionAtom "default" r "public" t s f


exDemoDatabase :: ([PermissionAtom], [PermissionAtom])
exDemoDatabase = (old, new)
  where
    -- 98b1123
    old = [ a "user" "public.book" s1 Nothing
          , a "user" "public.favourite_book" i1 Nothing
          , a "user" "public.user" u1 Nothing
          ]
    s1  = Select $ bareSelectProps [ "isbn", "title" ] Nothing False
    i1  = Insert $ bareInsertProps [ "isbn", "user_id" ]
    u1  = Update $ bareUpdateProps [ "name", "id" ] Nothing
    -- 3b627cd
    new = [ a "user" "public.book" s1 Nothing
          , a "user" "public.favourite_book" Delete f1
          , a "user" "public.favourite_book" i2 Nothing
          , a "user" "public.favourite_book" s2 Nothing
          , a "user" "public.favourite_book" u2 Nothing
          , a "user" "public.user" i3 f1
          , a "user" "public.user" u3 f1
          ]
    f1 = Just $ Filter "{ \"user_id\" : { \"_eq\": \"x-hasura-user-id\" } }"
    i2 = Insert $ bareInsertProps [ "affinity", "isbn", "user_id" ]
    s2 = Select $ bareSelectProps [ "affinity", "isbn", "user_id" ] (Just 5000) False
    u2 = Update $ bareUpdateProps [ "affinity", "isbn", "user_id" ] f1
    i3 = Insert $ bareInsertProps [ "name", "id" ]
    u3 = Update $ bareUpdateProps [ "name", "id" ] Nothing


old2New1 :: ([PermissionAtom], [PermissionAtom])
old2New1 = (old, new)
  where
    old = [ a "a" "plants" Delete Nothing
          , a "b" "people" Delete Nothing
          ]
    new = [ a "c" "cats" Delete Nothing ]

rowLimitAndAggregations :: ([PermissionAtom], [PermissionAtom])
rowLimitAndAggregations = (old, new)
  where
    old = [ a "admin" "plants" s1  Nothing
          , a "staff" "plants" s1' Nothing
          ]
    new = [ a "admin" "plants" s2  Nothing
          , a "staff" "plants" s2' Nothing
          ]
    s1  = Select $ bareSelectProps [ "a", "b", "c" ] (Just 100) True
    s2  = Select $ bareSelectProps [ "a", "b", "c" ] (Just 30)  False
    s1' = Select $ bareSelectProps [ "a", "b", "c" ] Nothing    False
    s2' = Select $ bareSelectProps [ "a", "b", "c" ] (Just 10)  True

nothingAndSomething :: ([PermissionAtom], [PermissionAtom])
nothingAndSomething = (old, new)
  where
    old = [ a "staff" "plants" Delete Nothing
          , a "admin" "people" Delete Nothing
          ]
    new = [ a "staff" "plants" Delete Nothing
          , a "admin" "people" Delete Nothing
          , a "admin" "plants" s1 Nothing
          ]
    s1 = Select $ bareSelectProps [ "a", "b", "c" ] (Just 100) True

exRowFilter :: ([PermissionAtom], [PermissionAtom])
exRowFilter = (old, new)
  where
    old = [ a "role" "table" Delete Nothing ]
    new = [ a "role" "toble" Delete (Just $ Filter $ "{ \"x\": 1}") ]

exTableNameChange :: ([PermissionAtom], [PermissionAtom])
exTableNameChange = (old, new)
  where
    old = [ a "role" "toble" Delete Nothing ]
    new = [ a "role" "table" Delete Nothing ]

exRoleChange :: ([PermissionAtom], [PermissionAtom])
exRoleChange = (old, new)
  where
    old = [ a "rool" "table" Delete Nothing ]
    new = [ a "role" "table" Delete Nothing ]

exActionChange1 :: ([PermissionAtom], [PermissionAtom])
exActionChange1 = (old, new)
  where
    old = [ a "role" "table" a1 Nothing ]
    new = [ a "role" "table" a2 Nothing ]
    a1 = Update $ bareUpdateProps ["a"] (Just $ Filter "{ \"x\": false }")
    a2 = Insert $ bareInsertProps ["a"]

exActionChange2 :: ([PermissionAtom], [PermissionAtom])
exActionChange2 = (old, new)
  where
    old = [ a "role" "table" a1 Nothing ]
    new = [ a "role" "table" a2 Nothing ]
    a1 = Update $ bareUpdateProps ["a"] (Just $ Filter "{ \"x\": false }")
    a2 = Insert $ bareInsertProps ["b", "c", "d"]

exAdded :: ([PermissionAtom], [PermissionAtom])
exAdded = (old, new)
  where
    old = [ ]
    new = [ a "role" "table" Delete Nothing ]

exColumns1 :: ([PermissionAtom], [PermissionAtom])
exColumns1 = (old, new)
  where
    old = [ a "role" "table" u1 Nothing ]
    new = [ a "role" "table" u2 Nothing ]
    u1 = Update $ bareUpdateProps ["a", "b", "c"] Nothing
    u2 = Update $ bareUpdateProps ["a", "bee", "c"] Nothing

exColumns2 :: ([PermissionAtom], [PermissionAtom])
exColumns2 = (old, new)
  where
    old = [ a "role" "table" u1 Nothing ]
    new = [ a "role" "table" u2 Nothing ]
    u1 = Update $ bareUpdateProps ["a"] Nothing
    u2 = Update $ bareUpdateProps ["a", "b", "c"] Nothing

exColumnsAndFilter :: ([PermissionAtom], [PermissionAtom])
exColumnsAndFilter = (old, new)
  where
    old = [ a "role" "table" u1 Nothing ]
    new = [ a "role" "table" u2 Nothing ]
    u1 = Update $ bareUpdateProps ["a", "b", "c"] Nothing
    u2 = Update $ bareUpdateProps ["a"] (Just $ Filter "{ \"x\": true }")

exFilterDiff :: ([PermissionAtom], [PermissionAtom])
exFilterDiff = (old, new)
  where
    old = [ a "role" "table" u1 Nothing ]
    new = [ a "role" "table" u2 Nothing ]
    u1 = Update $ bareUpdateProps ["a"] (Just $ Filter "{ \"x\": false }")
    u2 = Update $ bareUpdateProps ["a"] (Just $ Filter "{ \"x\": true }")

exFilterAndAction :: ([PermissionAtom], [PermissionAtom])
exFilterAndAction = (old, new)
  where
    old = [ a "role" "table" u1 Nothing ]
    new = [ a "role" "table" u2 Nothing ]
    u1 = Update $ bareUpdateProps ["a"] (Just $ Filter "{ \"x\": false }")
    u2 = Insert $ bareInsertProps ["a"]

justAddOne :: ([PermissionAtom], [PermissionAtom])
justAddOne = (old, new)
  where
    old = [ bp "a"
          , bp "b"
          , bp "c"
          ]
    new = [ bp "a"
          , bp "b"
          , bp "e"
          , bp "c"
          ]
    bp x = a "client" x Delete Nothing

someBbbExample :: ([PermissionAtom], [PermissionAtom])
someBbbExample = (old, new)
  where
    -- special-0000000115-f641b71b4a.json
    old = [ a "client" "user" s1a Nothing
          , a "client" "v_user_camera" s1 Nothing
          , a "client" "v_user_microphone" s1 Nothing
          ]
    -- special-0000000114-b2bc0c838d.json
    new = [ a "client" "user" s1b Nothing
          , a "client" "v_user_breakoutRoom" s1 Nothing
          , a "client" "v_user_camera" s1 Nothing
          , a "client" "v_user_microphone" s1 Nothing
          , a "client" "v_user_whiteboard" s1 Nothing
          ]
    s1  = Select $ bareSelectProps ["a"] Nothing False
    -- Problem: Changing these two statements brings up the problem. The issue
    -- was that the list diff thing doesn't pick up the equality, and then
    -- just does what it wants (which is bad.)
    -- s1a = Select $ bareSelectProps ["a"] Nothing False
    s1a = Select $ bareSelectProps ["a"] (Just 10) False
    s1b = Select $ bareSelectProps ["a"] Nothing False


singleSrcExample :: [PermissionAtom]
singleSrcExample = ex
  where
    ex = fst exFilterDiff


singleExamples :: [([PermissionAtom], String)]
singleExamples =
  [ (singleSrcExample, "update-and-filter")
  ]


main :: IO ()
main = do
  forM_ allExamples $ \((o', n'), name) -> do
    compareAndWrite (o', n') name
    compareAndWrite (n', o') (name ++ "-otherway")
  runSingleExamples


runSingleExamples :: IO ()
runSingleExamples = do
  let outDir = "out"
  forM_ singleExamples $ \(ps, name) -> do
    let o = toSingleSource (map ListItemNoOp ps) (Src "<generated>" Nothing)
        outFile = outDir ++ "/" ++ "single-generated-" ++ name ++ ".json"
    putStrLn name
    T.writeFile outFile $ toText $ encode $ o


compareAndWrite :: ([PermissionAtom], [PermissionAtom]) -> String -> IO ()
compareAndWrite (o', n') name = do
  let outDir = "out"
  let n = New $ map ListItemNoOp n'
      o = Old $ map ListItemNoOp o'
      perms = atomDiff o n
      outFile = outDir ++ "/" ++ "generated-" ++ name ++ ".json"
      mode = Diff
              (Old $ Src "<generated>" Nothing)
              (New $ Src "<generated>" Nothing)
      pd = mkPermissionData mode perms

  putStrLn name

  T.writeFile outFile $ toText $ encode $ pd

