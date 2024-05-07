# Repo Change Comparision tool

Computes all the hasura metadata json values for a given repo, at all
revisions where such path was changed.

Example usage:

```shell
cabal run repo-change-comparer -- \
          -r ../../../demo-database \
          -p hasura/metadata \
          -o output
```

(If you have a copy of <https://github.com/InvariantClub/demo-database> checked out
 to that folder.)

This will give you an output like:

```shell
> tree output
output
├── 0000000000-3b627cd.json
├── 0000000001-98b1123.json
├── 0000000002-4a26090.json
├── 0000000003-27a1274.json
└── current.json
```

I.e. a bunch of files at the given commits, numbered where the first one is
the _most recent_ and the last one is the _oldest_. Current, naturally, is the
one at the checked-out revision.
