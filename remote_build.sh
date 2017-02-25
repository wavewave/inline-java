#!/bin/sh

set -x

KEYFILE="$1"
USER_AT_IP="$2"

cleanup() {
   echo "Remotely cleaning up $REMOTE_DIR"
   if [ -n "$REMOTE_DIR" ] ; then
      ssh -o "StrictHostKeyChecking no" \
          -i "$KEYFILE" \
          "$USER_AT_IP" \
          "rm -rf \"${REMOTE_DIR}\""
   fi
}
trap cleanup INT QUIT TERM EXIT

# Create a temporary directory on the build server.
#
REMOTE_DIR=$(ssh -o "StrictHostKeyChecking no" \
                 -i "$KEYFILE" \
                 "$USER_AT_IP" \
                 "mktemp -d /tmp/leapyear-algorithms-remote-build-XXXXXXX")
if [ -z "$REMOTE_DIR" ]; then
   echo "Could not create remote directory with 'mktemp'. Exiting."
   exit 1
else
   echo REMOTE_DIR = $REMOTE_DIR
fi

# Make a directory into which to sync the source code.
#
SYNC_DIR=${REMOTE_DIR}/src-sync
echo SYNC_DIR = $SYNC_DIR
ssh -o "StrictHostKeyChecking no" \
    -i "$KEYFILE" \
    "$USER_AT_IP" \
    "mkdir -p ${SYNC_DIR}"

if [ 0 -eq $? ]; then

   # Sync the source code.
   #
   rsync -az -e \
      "ssh -o 'StrictHostKeyChecking no' -i \"$KEYFILE\"" \
      --exclude='.git/' \
      --exclude='_darcs/' \
      ./ \
      "$USER_AT_IP":"${SYNC_DIR}"

   if [ 0 -eq $? ]; then

      # Perform the build remotely.
      #
      ssh -o "StrictHostKeyChecking no" \
          -i "$KEYFILE" \
          "$USER_AT_IP" \
          "source ~/.bash_profile
           if [ \"Darwin\" = \"$(uname -s)\" ]; then
              IS_DARWIN=1
           fi
           export TMPDIR=${REMOTE_DIR}
           cd ${SYNC_DIR}
           ( cd jni && ./clean.sh && nix-build
           ) &&
           ( cd jvm && ./clean.sh && nix-build
           ) &&
           ( ./clean.sh && nix-build
           ) &&
           ( cd jni &&
             nix-shell --run \"./build.sh --fast --test --only-snapshot\" &&
             nix-shell --run \"./build.sh --fast --test\"
           ) &&
           ( cd jvm &&
             nix-shell --run \"./build.sh --fast --test --only-snapshot\" &&
             nix-shell --run \"./build.sh --fast --test\"
           ) &&
           nix-shell --run \"./build.sh --fast --test --only-snapshot\" &&
           nix-shell --run \"./build.sh --fast --test\"
          "
   fi
fi
