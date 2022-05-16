# Note: This should fail to build
let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/aa2f845096f72dde4ad0c168eeec387cbd2eae04";
    sha256 = "0l732ci2g78pcgk9kqn6c18h4j47dhp1dys52cmqhzm4pyi6dl0z";
  };
  pkgs = import nixpkgs-src { inherit config; };
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

  project1 = pkgs.haskellPackages.project1;


in
  if pkgs.lib.inNixShell then project1.env else project1
