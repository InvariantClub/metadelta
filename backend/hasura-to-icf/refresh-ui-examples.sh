#!/usr/bin/env bash

set -exuo pipefail

rm ../../frontend/static/examples/*.json || true
cp out/* ../../frontend/static/examples/
