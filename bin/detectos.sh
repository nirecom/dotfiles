#!/bin/bash
# Detect OS distribution

if [ -e /proc/sys/fs/binfmt_misc/WSLInterop ]; then
    ISWSL=true
else
    ISWSL=false
fi

if [ ""`arch`"" = "arm64" ]; then
    ISM1=true
else
    ISM1=false
fi

if [[ "$(uname)" == 'Darwin' ]]; then
    OSDIST=macos
elif [[ "$(expr substr $(uname -s) 1 5)" == 'Linux' ]]; then
    RELEASE_FILE=/etc/os-release
    if grep '^NAME="CentOS' "${RELEASE_FILE}" >/dev/null; then
        OSDIST=centos
    elif grep '^NAME="Amazon' "${RELEASE_FILE}" >/dev/null; then
        OSDIST=amazon
    elif grep '^NAME="Ubuntu' "${RELEASE_FILE}" >/dev/null; then
        OSDIST=ubuntu
    else
        OSDIST=
    fi
elif [ "$(expr substr $(uname -s) 1 5)" == 'MINGW' ]; then
    OSDIST=mingw
else
    OSDIST=
fi
