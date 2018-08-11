#!/bin/bash

cabal new-run bench-bot -O2 --enable-profiling --profiling-detail=all-functions --enable-executable-profiling --enable-library-profiling -- ~/.ssh/bitmex_hmmmbot.pub ~/.ssh/bitmex_hmmmbot ~/.ssh/elastic.user ~/.ssh/elastic.pw +RTS -p "-po$1" -s
