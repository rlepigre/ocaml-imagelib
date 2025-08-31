#!/bin/bash

for IMG in "$@"; do
  echo "Testing ${IMG}"
  imagelib-convert "${IMG}" "${IMG}.png" || echo "FAILED"
  compare -metric AE "${IMG}" "${IMG}.png" "${IMG}.diff.png"
  echo ""
done
