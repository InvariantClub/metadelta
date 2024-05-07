#!/usr/bin/env sh

set -euo pipefail

echo "Deleting all existing json files ..."
rm out/*.json || true
mkdir -p out

echo "Building ..."
cabal build


echo "Generated examples"
cabal run gen-examples


echo "Custom examples from around town"
echo Row-level ...
cabal exec hasura-to-icf -- diff \
  -n ./example-files/plants/row-level/b.json \
  -o ./example-files/plants/row-level/a.json \
  >out/row-level-plants.json

echo Strictly-stronger ...
cabal exec hasura-to-icf -- diff \
  -n ./example-files/plants/strictly-stronger/new.json \
  -o ./example-files/plants/strictly-stronger/old.json \
  >out/strictly-stronger-plants.json

echo Table-rename ...
cabal exec hasura-to-icf -- diff \
  -n ./example-files/plants/table-rename/new.json \
  -o ./example-files/plants/table-rename/old.json \
  >out/table-rename-plants.json

echo Add-column ...
cabal exec hasura-to-icf -- diff \
  -n ./example-files/plants/add-column/new.json \
  -o ./example-files/plants/add-column/old.json \
  >out/add-column-plants.json

echo BigBlueButton examples ...
cd ./example-files/reference-repos/bbb/
bash run.sh
mv out/*.json ../../../out/
cd ../../../

echo Special single ones we want to see ...
# cabal exec hasura-to-icf -- single \
#   -p ./example-files/reference-repos/bbb/0000000115-f641b71b4a.json \
#   >out/special-0000000115-f641b71b4a.json
# cabal exec hasura-to-icf -- single \
#   -p ./example-files/reference-repos/bbb/0000000114-b2bc0c838d.json \
#   >out/special-0000000114-b2bc0c838d.json

cabal exec hasura-to-icf -- single \
  -p ./example-files/reference-repos/bbb/0000000000-f4e5803b15.json \
  >out/special-bbb-0000000000-f4e5803b15.json.json
cabal exec hasura-to-icf -- single \
  -p ./example-files/reference-repos/bbb/current.json \
  >out/special-bbb-current.json.json

for i in out/*.json;
do
  jq '.' $i | sponge $i
done;

./refresh-ui-examples.sh
