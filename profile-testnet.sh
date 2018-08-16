#!/bin/bash

cabal new-run bench-bot -O2 --enable-profiling --profiling-detail=all-functions --enable-executable-profiling --enable-library-profiling -- ~/.ssh/bmextestnet.pub ~/.ssh/bmextestnet ~/.ssh/elastic.id ~/.ssh/elastic.pw +RTS -p -pobench-bot-testnet -s
