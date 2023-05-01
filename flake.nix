{
  description = "A very basic flake";

  inputs.miso = {
    url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.3.tar.gz";
    flake = false;
  };

  outputs = { self, nixpkgs, miso }:
    with
    (import miso { system = "x86_64-linux"; });
    let
      client = pkgs.haskell.packages.ghcjs86.callCabal2nix "myocardio" ./. { };
      server-prod = pkgs.haskell.packages.ghc865.callCabal2nix "myocardio" ./. { };
      server-dev = pkgs.haskell.packages.ghc865.callCabal2nix "myocardio" ./. { miso = miso-jsaddle; };
      reload-script = pkgs.writeScriptBin "reload" ''
        ${pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -c \
          '${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl' \
          -T ':run Main.main'
      '';
    in
    {

      packages.x86_64-linux.client = client;
      packages.x86_64-linux.server = server-prod;

      devShells.x86_64-linux.default = server-dev.env.overrideAttrs (old: { buildInputs = old.buildInputs ++ [ reload-script ]; });

    };
}
