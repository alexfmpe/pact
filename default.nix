{ rpRef ? "f3ff81d519b226752c83eefd4df6718539c3efdc", rpSha ?  "1ijxfwl36b9b2j4p9j3bv8vf7qfi570m1c5fjyvyac0gy0vi5g8j", system ? builtins.currentSystem }:

let rp = builtins.fetchTarball {
  url = "https://github.com/reflex-frp/reflex-platform/archive/${rpRef}.tar.gz";
  sha256 = rpSha;
};
overlay = self: super: {
  z3 = super.z3.overrideAttrs (drv: {
    src = self.fetchFromGitHub {
      owner = "Z3Prover";
      repo = "z3";
      rev = "727929c9af003d71eab1f0d90cc8e01761943491";
      sha256 = "02p8rhflimc852ysgf7nmaypz6ga3w4iss3z8d3qrby5a2d464p9";
    };
  });
};

in
  (import rp { inherit system; nixpkgsOverlays = [ overlay ]; }).project ({ pkgs, ... }:
  let gitignore = pkgs.callPackage (pkgs.fetchFromGitHub {
        owner = "siers";
        repo = "nix-gitignore";
        rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
        sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
      }) {};
  in
  {
    name = "pact";
    overrides = self: super: with pkgs.haskell.lib;
      let guardGhcjs = p: if self.ghc.isGhcjs or false then null else p;
       in {
            pact = doCoverage (addBuildDepend super.pact pkgs.z3);
            haskeline = guardGhcjs super.haskeline;

            # tests for extra were failing due to an import clash (`isWindows`)
            extra = dontCheck super.extra;
            # tests try to use ghc-pkg and cabal (https://github.com/sol/doctest/issues/213)
            doctest = guardGhcjs (dontCheck (self.callHackage "doctest" "0.16.0" {}));
            # these want to use doctest, which doesn't work on ghcjs
            bytes = dontCheck super.bytes;
            intervals = dontCheck super.intervals;
            bound = dontCheck super.bound;
            trifecta = dontCheck super.trifecta;
            lens-aeson = dontCheck super.lens-aeson;
            # test suite for this is failing on ghcjs:
            hw-hspec-hedgehog = dontCheck super.hw-hspec-hedgehog;

            algebraic-graphs = dontCheck super.algebraic-graphs;

            # Needed to get around a requirement on `hspec-discover`.
            megaparsec = dontCheck super.megaparsec;

            hedgehog = self.callCabal2nix "hedgehog" (pkgs.fetchFromGitHub {
              owner = "hedgehogqa";
              repo = "haskell-hedgehog";
              rev = "38146de29c97c867cff52fb36367ff9a65306d76";
              sha256 = "1z8d3hqrgld1z1fvjd0czksga9lma83caa2fycw0a5gfbs8sh4zh";
            } + "/hedgehog") {};
            hlint = self.callHackage "hlint" "2.0.14" {};
            # hoogle = self.callHackage "hoogle" "5.0.15" {};

            # sbv with a patch to disable "unsupported query call" until it's fixed upstream
            sbv = dontCheck (self.callCabal2nix "sbv" (pkgs.fetchFromGitHub {
              owner = "joelburget";
              repo = "sbv";
              rev = "8d13e26255178c5ee5b5b3fad97873ff214d7470";
              sha256 = "13hzbf6pxl0spf2j5sdjflpqd3al895b71dwg82dspxp4jzgsjcs";
            }) {});

            # need crackNum 2.3
            crackNum = pkgs.haskell.lib.dontCheck (self.callCabal2nix "crackNum" (pkgs.fetchFromGitHub {
              owner = "LeventErkok";
              repo = "crackNum";
              rev = "54cf70861a921062db762b3c50e933e73446c3b2";
              sha256 = "02cg64rq8xk7x53ziidljyv3gsshdpgbzy7h03r869gj02l7bxwa";
            }) {});

            # Our own custom fork
            thyme = dontCheck (self.callCabal2nix "thyme" (pkgs.fetchFromGitHub {
              owner = "kadena-io";
              repo = "thyme";
              rev = "6ee9fcb026ebdb49b810802a981d166680d867c9";
              sha256 = "09fcf896bs6i71qhj5w6qbwllkv3gywnn5wfsdrcm0w1y6h8i88f";
            }) {});

            # weeder = self.callHackage "weeder" "1.0.5" {};
            weeder = self.callCabal2nix "weeder" (pkgs.fetchFromGitHub {
              owner = "ndmitchell";
              repo = "weeder";
              rev = "56b46fe97782e86198f31c574ac73c8c966fee05";
              sha256 = "005ks2xjkbypq318jd0s4896b9wa5qg3jf8a1qgd4imb4fkm3yh7";
            }) {};

            # aeson 1.4.2
            aeson = (if self.ghc.isGhcjs or false
              then (pkgs.lib.flip addBuildDepend self.hashable-time)
              else pkgs.lib.id) (self.callCabal2nix "aeson" (pkgs.fetchFromGitHub {
              owner = "bos";
              repo = "aeson";
              rev = "c3d04181eb64393d449a68084ffea3a94c3d8064";
              sha256 = "1l8lks6plj0naj9ghasmkqglshxym3f29gyybvjvkrkm770p2gl4";
            }) {});
          };
    packages = {
      pact = gitignore.gitignoreSource [] ./.;
    };
    shellToolOverrides = ghc: super: {
      z3 = pkgs.z3;
      stack = pkgs.stack;
    };
    shells = {
      ghc = ["pact"];
      ghcjs = ["pact"];
    };

  })
