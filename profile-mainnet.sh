#!/bin/bash

cabal new-run bench-bot -O2 --enable-profiling --profiling-detail=all-functions --enable-executable-profiling --enable-library-profiling -- ~/.ssh/bitmex_hmmmbot.pub ~/.ssh/bitmex_hmmmbot ~/.ssh/elastic.id ~/.ssh/elastic.pw +RTS -p -pobench-bot-mainnet -s
