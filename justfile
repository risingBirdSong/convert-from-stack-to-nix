# the below commands are the first version, version A
# update dependencies
depsA:
    cabal2nix . > project0.nix

buildA:
    nix-build release0.nix 


depsB:
    cabal2nix . > project1.nix

buildB:
    nix-build release1.nix 



run:
    cabal run
    