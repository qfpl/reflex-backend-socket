#! /usr/bin/env bash

nix-prefetch-git https://github.com/reflex-frp/reflex-platform > reflex-platform.json
nix-prefetch-git https://github.com/qfpl/reflex-basic-host > reflex-basic-host.json
nix-prefetch-git https://github.com/qfpl/reflex-binary > reflex-binary.json
