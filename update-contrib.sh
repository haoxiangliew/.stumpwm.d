#!/usr/bin/env bash

if nc -zw1 google.com 443; then
    cd ~/.stumpwm.d/stumpwm-contrib
    git fetch origin
    git reset --hard origin/master
fi
