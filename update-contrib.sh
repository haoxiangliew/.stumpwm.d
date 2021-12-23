#!/usr/bin/env bash

if nc -zw1 google.com 443; then
    cd ~/.stumpwm.d/stumpwm-contrib
    git fetch upstream
    git checkout master
    git merge upstream/master
    git add *
    git commit -S -m "Automatic Update"
    git push
    git fetch origin
    git reset --hard origin/master
fi
