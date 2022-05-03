# update dependencies
deps:
    cabal2nix . > project0.nix

build:
    nix-build release0.nix 

myecho:
    echo "hello world, im testing here"

run:
    cabal run
    