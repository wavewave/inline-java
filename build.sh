#!/bin/sh

set -e

BUILD_CMD="stack $EXTRA_STACK_ARGS build $EXTRA_BUILD_ARGS $*"

echo Building with \"$BUILD_CMD\"
eval $BUILD_CMD
