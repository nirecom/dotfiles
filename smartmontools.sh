#!/bin/bash
source ./bin/detectos.sh

if [ "$OSDIST" != "macos" ]; then
    echo "Not macos. Abort smartmontools install & run."
    exit 1
fi
if ! type smartmctl >/dev/null 2>&1; then
    brew install smartmontools
fi
sudo smartctl --all /dev/disk0
