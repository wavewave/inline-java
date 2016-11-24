#!/bin/sh

set -e

case "$USE_JLI" in
   1) EXTRA_BUILD_ARGS="--flag jni:jli $EXTRA_BUILD_ARGS" ;;
   0) ;;
   *) if [ "Darwin" = "$(uname -s)" ]; then
         EXTRA_BUILD_ARGS="--flag jni:jli $EXTRA_BUILD_ARGS"
      fi
      ;;
esac

BUILD_CMD="stack $EXTRA_STACK_ARGS build $EXTRA_BUILD_ARGS $*"

echo Building with \"$BUILD_CMD\"
eval $BUILD_CMD
