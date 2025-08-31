#!/bin/bash

for IMG in "$@"; do
  echo "Testing ${IMG}"
  imagelib-convert "${IMG}" "${IMG}.ppm" && echo "SHOULD HAVE FAILED"
done
