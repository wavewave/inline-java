#!/bin/sh

set -e

(cd ../jni && ./clean.sh)
rm -f result
rm -fr dist dist-newstyle
rm -fr .stack-work
