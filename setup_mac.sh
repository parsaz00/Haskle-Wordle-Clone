#!/bin/bash
brew install gtk+3 haskell-stack
echo 'export PKG_CONFIG_PATH=$(brew --prefix gtk+3)/lib/pkgconfig:$PKG_CONFIG_PATH' >> ~/.zshrc
source ~/.zshrc
stack setup
stack build
