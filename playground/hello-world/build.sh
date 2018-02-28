#!/bin/bash

## clean old build
rm main main.hi main.o

# the dynamic flag tells the compiler to link dependencies dynamically instead
# of bundeling all of haskells basic dependencies into the binary
stack ghc -- -dynamic main.hs