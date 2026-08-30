{
  description = "UPI-MockEngine-Haskell: deterministic NPCI/UPI switch simulator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Pinned to the compiler in Stackage LTS 24.51 (design document, §7).
        ghcName = "ghc9101";
        hsPkgs = pkgs.haskell.packages.${ghcName};

        # package.yaml is authoritative; the .cabal file is generated and is not
        # in the repository. Materialise it in a derivation so that `nix build`
        # never depends on a developer having run hpack.
        cabalSrc = pkgs.runCommand "upi-mock-engine-src"
          { nativeBuildInputs = [ pkgs.hpack ]; }
          ''
            cp -r ${pkgs.lib.cleanSource ./.} $out
            chmod -R u+w $out
            cd $out
            hpack
          '';

        upi-mock-engine = hsPkgs.callCabal2nix "upi-mock-engine" cabalSrc { };
      in
      {
        packages = {
          default = upi-mock-engine;
          inherit upi-mock-engine;

          # Static-ish minimal container image. Phase 3 replaces this with the
          # musl/distroless build described in §14.2.
          container = pkgs.dockerTools.buildLayeredImage {
            name = "upi-mock-engine";
            tag = "0.1.0";
            contents = [ pkgs.cacert ];
            config = {
              Entrypoint = [ "${upi-mock-engine}/bin/upi-mock-engine" ];
              # warp binds all interfaces already; there is no --host flag, and
              # optparse-applicative exits on an unrecognised one.
              Cmd = [ "--port" "8080" "--database" "/state/upi.sqlite3" ];
              ExposedPorts."8080/tcp" = { };
            };
          };
        };

        apps.default = flake-utils.lib.mkApp { drv = upi-mock-engine; };

        devShells.default = pkgs.mkShell {
          name = "upi-mock-engine-dev";
          packages = [
            hsPkgs.ghc
            hsPkgs.haskell-language-server
            pkgs.cabal-install
            pkgs.hpack
            pkgs.hlint
            pkgs.fourmolu
            pkgs.sqlite
            pkgs.jq
            pkgs.hey          # macro load generation (§9.3)
            pkgs.pkg-config
            pkgs.zlib
          ];
          shellHook = ''
            echo "upi-mock-engine dev shell — ghc $(ghc --numeric-version)"
            echo "run 'hpack' after editing package.yaml"
          '';
        };

        checks.default = upi-mock-engine;

        formatter = pkgs.nixpkgs-fmt;
      });
}
