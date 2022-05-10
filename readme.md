i'm using a just file (instead of a make file) to run the nix commands

using version B to try to work with specific versions of the depenencies, so far just JWT.. 

I think a current problem is updating one version of a dependency but that new version is just specifically for that single package and then I'd need to and go update all it's dependencies to work with the new change. 

I'm guess what I'm looking for is a simple way to find a curated set of packages that are known to work together.. like stackage for nix.
