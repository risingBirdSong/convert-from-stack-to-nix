# Note: This should fail to build
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          project1 =
            haskellPackagesNew.callPackage ./project1.nix { };

          jwt =
            haskellPackagesNew.callPackage ./jwt.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { project1 = pkgs.haskellPackages.project1;
  }