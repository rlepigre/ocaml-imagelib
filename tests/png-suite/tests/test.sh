#!/bin/bash

for IMG in "$@"; do
  echo "Testing ${IMG}"
  imagelib-convert "${IMG}" "${IMG}.png" && \
    compare -metric AE "${IMG}" "${IMG}.png" "${IMG}.diff.png" 2>&1 | \
    sed 's/^[^ ]\+ (\(.*\))$/\1/'
  echo ""
done
