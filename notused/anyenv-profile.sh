#!/bin/bash
# ref https://caramelcase.com/aws-anyanv-rubyenv-nodenv/#toc4
# This is to be installed under /etc/profile.d of Amazon Linux
export ANYENV_ROOT="/usr/local/anyenv"
export ANYENV_DEFINITION_ROOT="/usr/local/anyenv/share/anyenv-install"
export PATH="${ANYENV_ROOT}/bin:${PATH}"
eval "$(anyenv init --no-rehash -)"
